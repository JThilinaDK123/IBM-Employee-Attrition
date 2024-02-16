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
library(randomForest)

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

############################### No sampling techniques ##################################

set.seed(111)
model3=train(
  Attrition~.,data = data1,method = "xgbTree",
  trControl = trainControl("cv",number = 3),
  tuneLength = 3
)

# Make predictions on the test data
probabilities <- model3 %>% predict(data2, type = "raw")
probabilities<-ordered(probabilities, levels=c("0","1"))
table(probabilities)
table(data2$Attrition)

# Model accuracy
confusionMatrix(table(probabilities,data2$Attrition))


############################### Under-sampling ##################################

data3 <- ovun.sample(Attrition ~ ., data = data1, method = "under",N =5756,seed = 1)$data
table(data3$Attrition)

set.seed(111)
model1=train(
  Attrition~.,data = data3,method = "xgbTree",
  trControl = trainControl("cv",number = 3),
  tuneLength = 3
)

# Make predictions on the test data
probabilities <- model1 %>% predict(data2, type = "raw")
probabilities <- ordered(probabilities, levels=c("0","1"))
table(probabilities)
table(data2$Attrition)
# Model accuracy
confusionMatrix(table(probabilities,data2$Attrition))
#plots
#vip
vip(model, num_features = 45, geom = "point")

############################### Over-sampling ##################################

data4 <- ovun.sample(Attrition ~ ., data = data1, method = "over",N =30968)$data
table(data4$Attrition)

set.seed(111)
model2=train(
  Attritiong~.,data = data4,method = "xgbTree",
  trControl = trainControl("cv",number = 10),
  tuneLength = 10
)

# Make predictions on the test data
probabilities <- model2 %>% predict(data2, type = "raw")
probabilities<-ordered(probabilities, levels=c("0","1"))
table(probabilities)
table(data2$Attrition)
# Model accuracy
confusionMatrix(table(probabilities,data2$Attrition))
