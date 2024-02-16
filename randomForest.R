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
library(GoodmanKruskal)
library(corrplot)
library(tidyverse)
library(caret)    
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

#################################### No sampling ##################################

library(randomForest)
set.seed(111)
model1=randomForest(formula=Attrition~.,data = data1)
predictions=model1 %>% predict(data2,type = "class")
confusionMatrix(predictions,data2$Attrition)

############################### over-sampling #####################################

data3<- ovun.sample(Attrition ~ ., data = data1, method = "over",N =30968)$data
table(data3$Attrition_Flag)
set.seed(111)
model2=randomForest(formula=Attrition~.,data = data3,importance=TRUE)
predictions=model2 %>% predict(data2,type = "class")
confusionMatrix(predictions,data2$Attrition)


############################### under-sampling #####################################

data4 <- ovun.sample(Attrition ~ ., data = data1, method = "under",N =5756,seed = 1)$data
table(data4$Attrition)
set.seed(111)
model3=randomForest(formula=Attrition~.,data = data4,importance=TRUE)
predictions=model3 %>% predict(data2,type = "class")
confusionMatrix(predictions,data2$Attrition)

var_importance <- importance(model3)
var_importance <- var_importance[order(-var_importance[, 1]), , drop = FALSE]
barplot(var_importance[, 1], names.arg = rownames(var_importance), las = 2, col = "steelblue",
        main = "Variable Importance Plot",
        ylab = "Mean Decrease Gini")



