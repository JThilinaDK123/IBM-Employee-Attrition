library(knitr)
library(recipes)  
library(glmnet)   
library(caret)    
library(vip) 
library(pls)
library(leaps)
library(pdp)
library(ggplot2)
library(ROSE)
library(rpart)

data1=read.csv("traindata.csv")
data2=read.csv("testdata.csv")

cols <- c("BusinessTravel","Department","Education","EducationField","EnvironmentSatisfaction",
          "Gender","JobLevel","JobRole","JobSatisfaction","MaritalStatus", "JobInvolvement",
          "OverTime","PerformanceRating","RelationshipSatisfaction","StockOptionLevel",
          "WorkLifeBalance")

for (col in cols) {
  data1[[col]] <- as.integer(factor(data1[[col]], levels = unique(data1[[col]])))
  data2[[col]] <- as.integer(factor(data2[[col]], levels = unique(data2[[col]])))
}

data1[cols] <- lapply(data1[cols], factor)
data2[cols] <- lapply(data2[cols], factor)

data1$Attrition<-factor(data1$Attrition,
                        levels = c('Voluntary_Resignation','Current_employee'),
                        labels = c(0,1))

data2$Attrition<-factor(data2$Attrition,
                        levels = c('Voluntary_Resignation','Current_employee'),
                        labels = c(0,1))

summary(data1)
summary(data2)

# Remove level 1 from 'EducationField'
data2$EducationField <- droplevels(data2$EducationField, exclude = "7")
data2 <- na.omit(data2)
summary(data2)

#Oversampling

data_balanced_over <- ovun.sample(Attrition ~ ., data = data1, method = "over",N = 30968,seed = 1)$data
table(data_balanced_over$Attrition)
x=model.matrix(Attrition~.,data_balanced_over)[,-1]
dim(x)
y=data_balanced_over$Attrition

####################################################################################
#            ridge                                                                 #       
####################################################################################

#cross validation to select best lambda
lambda<-10^seq(-3,3,length=100)
set.seed(111)

ridge.model<-train(
  x = x,
  y = y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10,
  family="binomial",
  tuneGrid = expand.grid(alpha = 0, lambda = lambda))

# Display regression coefficients
coef(ridge.model$finalModel, ridge.model$bestTune$lambda)

# Make predictions on the test data
x.test <- model.matrix(Attrition ~., data2)[,-1]
probabilities <- predict(ridge.model,x.test, type = "raw")
probabilities<-ordered(probabilities, levels=c("0","1"))
table(probabilities)
observed.classes=data2$Attrition
table(observed.classes)
summary(probabilities)

# Model accuracy
confusionMatrix(table(probabilities,observed.classes))

#plots
#vip
vip(ridge.model, num_features = 45, geom = "point")


####################################################################################
#                  lasso                                                           #
####################################################################################
#cross validation to select best lambda
lambda<-10^seq(-3,3,length=100)

set.seed(111)
lasso.model<-train(
  x = x,
  y = y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10,
  family="binomial",
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
  
)

# Display regression coefficients
coef(lasso.model$finalModel, lasso.model$bestTune$lambda)

# Make predictions on the test data
x.test <- model.matrix(Attrition ~., data2)[,-1]
probabilities <- predict(lasso.model,x.test, type = "raw")
probabilities<-ordered(probabilities, levels=c("0","1"))
table(probabilities)
table(observed.classes)

# Model accuracy
confusionMatrix(table(probabilities,observed.classes))

#plots
#vip
vip(ridge.model, num_features = 67, geom = "point")




###################################################################################
#                 elastic-net                                                     #
###################################################################################


# for reproducibility
set.seed(111)

# grid search across 
elastic.model <- train(
  x = x,
  y = y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10,
  family="binomial"
)
# Display regression coefficients
coef(elastic.model$finalModel, elastic.model$bestTune$lambda)

# Make predictions on the test data
probabilities <- predict(elastic.model,x.test, type = "raw")
probabilities<-ordered(probabilities, levels=c("0","1"))
table(probabilities)
table(observed.classes)
summary(probabilities)

# Model accuracy
confusionMatrix(table(probabilities,observed.classes))

#plots
#vip
vip(elastic.model, num_features = 67, geom = "point")

