library(tidyverse)
library(caret)    
library(ROSE)

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


########################### No sampling techniques ###########################################
set.seed(111)
model1=train(
  Attrition~.,data = data1,method = "rpart",
  trControl = trainControl("cv",number = 10),
  tuneLength = 20
)
plot(model1)
model1$bestTune
par(xpd = NA)
plot(model1$finalModel)
text(model1$finalModel,digits = 3)

predicted.classes=model1 %>% predict(data2)
mean(predicted.classes == data2$Attrition)
predictions=model1 %>% predict(data2,type = "raw")
confusionMatrix(predictions,data2$Attrition)

############################### over-sampling ##################################################
data_balanced_over <- ovun.sample(Attrition ~ ., data = data1, method = "over",N = 30968)$data

set.seed(111)
model2=train(
  Attrition~.,data = data_balanced_over,method = "rpart",
  trControl = trainControl("cv",number = 10),
  tuneLength = 20
)
plot(model2)
model2$bestTune
par(xpd = NA)
plot(model2$finalModel)
text(model2$finalModel,digits = 3)

predicted.classes=model2 %>% predict(data2)
mean(predicted.classes == data2$Attrition)
predictions=model2 %>% predict(data2,type = "raw")
confusionMatrix(predictions,data2$Attrition)

############################### under-sampling #################################################
data_balanced_under <- ovun.sample(Attrition ~ ., data = data1, method = "under",N =5756 , seed = 1)$data

set.seed(111)
model3=train(
  Attrition~.,data = data_balanced_under,method = "rpart",
  trControl = trainControl("cv",number = 10),
  tuneLength = 20
)
plot(model3)
model3$bestTune
par(xpd = NA)
plot(model3$finalModel)
text(model3$finalModel,digits = 3)

predicted.classes=model3 %>% predict(data2)
mean(predicted.classes == data2$Attrition)
predictions=model3 %>% predict(data2,type = "raw")
confusionMatrix(predictions,data2$Attrition)

