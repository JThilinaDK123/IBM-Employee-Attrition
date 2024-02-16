#Import the training dataset

data1=read.csv("cleaned_data.csv")
dim(data1)
library(ggplot2)
library(dplyr)
library(cowplot)
library(hrbrthemes)
data1$Attrition<-factor(data1$Attrition,
                         levels = c("Voluntary_Resignation","Current_employee"),
                         labels = c("Voluntary Resignation","Current Employee"))
data1$Education = factor(data1$Education,
                          levels = c(1,2,3,4,5),
                          labels = c('Below College', 'College','Bachelor'
                                     ,'Master','Doctor'))
data1$EnvironmentSatisfaction=factor(data1$EnvironmentSatisfaction,
                                      levels = c(1,2,3,4),
                                      labels=c('low','Medium','High','Very High'))

cols=c("Education","Department","EnvironmentSatisfaction")
data1[cols] <- lapply(data1[cols], factor)


counts <- table(data1$Attrition)
df <- data.frame(categories = names(counts), count = as.numeric(counts))

# Create bar plot using ggplot
ggplot(data = df, aes(x = categories, y = count, fill = categories)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = count), vjust = -0.5, fontface = "bold", size = 4) +
  labs(x = "Categories", y = "Count") +
  scale_fill_manual(values = c("#2196F3", "#4CAF50")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 7, face = "bold"),
        legend.position = "none")

## Business Travel vs Environment Satisfaction
ggplot(data1, aes(fill = EnvironmentSatisfaction, x = BusinessTravel)) +
  geom_bar(position = "fill")

## Attrition vs Environment Satisfaction
ggplot(data1, aes(fill = Attrition, x = EnvironmentSatisfaction)) +
  geom_bar(position = "fill") + labs(y = "Percentage") 

## Attrition vs Department
ggplot(data1, aes(fill = Attrition, x = Department)) +
  geom_bar(position = "fill") + labs(y = "Percentage") 

## Attrition vs Education
ggplot(data1, aes(fill = Attrition, x = Education)) +
  geom_bar(position = "fill") + labs(y = "Percentage") 

## Attrition vs Education Field
ggplot(data1, aes(fill = Attrition, x = EducationField)) +
  geom_bar(position = "fill") + labs(y = "Percentage") 

## Attrition vs Marital Status
data1$Attrition=as.factor(data1$Attrition)
data1$MaritalStatus=as.factor(data1$MaritalStatus)
p8=ggplot(data = data1,aes(x=Attrition,y=Age,fill=MaritalStatus))+geom_boxplot()+scale_x_discrete(labels=c("1"="Voluntary Resign","2"="Current Employees"))
p8


## Education, Monthly Income vs Attrtion
data1$Attrition=as.factor(data1$Attrition)
data1$Education=as.factor(data1$Education)
p7=ggplot(data = data1,aes(x=Attrition,y=MonthlyIncome,fill=Education))+geom_boxplot()+scale_x_discrete(labels=c("1"="Voluntary Resign","2"="Current Employees"))
p7


## Department, Monthly Income vs Attrition
data1$Attrition=as.factor(data1$Attrition)
data1$Education=as.factor(data1$Department)
p7=ggplot(data = data1,aes(x=Attrition,y=MonthlyIncome,fill=Department))+geom_boxplot()+scale_x_discrete(labels=c("1"="Voluntary Resign","2"="Current Employees"))
p7


## StockOptionLevel, Monthly Income vs Attrition
data1$Attrition=as.factor(data1$Attrition)
data1$StockOptionLevel=as.factor(data1$StockOptionLevel)
p9=ggplot(data = data1,aes(x=Attrition,y=MonthlyIncome,fill=StockOptionLevel))+geom_boxplot()+scale_x_discrete(labels=c("1"="Voluntary Resign","2"="Current Employees"))
p9

## Monthly Income
qplot(MonthlyIncome,data = data1,geom = "histogram")

## Percent Salary Hike
qplot(PercentSalaryHike,data = data1,geom = "histogram")

## Age vs Gender
mean_data <- aggregate(Age ~ Gender, data1, mean)
ggplot(data1, aes(x = Age, fill = Gender)) +
  geom_density(alpha = 0.6) +
  geom_vline(data = mean_data, aes(xintercept = Age, color = Gender),
             linetype = "dashed", size = 1) +
  labs(title = "Age Distribution along with Gender", x = "Age", y = "Density") +
  scale_fill_manual(values = c("blue", "pink")) +
  scale_color_manual(values = c("blue", "pink")) +
  theme_minimal()

## Gender vs Attrition
library(scales)
percentage_data <- with(data1, prop.table(table(Gender, Attrition), 1) * 100)
percentage_df <- as.data.frame(percentage_data)
ggplot(percentage_df, aes(x = Gender, y = Freq, fill = Attrition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Attrition by Gender",
       x = "Gender", y = "Percentage",
       fill = "Attrition") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal()


data1=read.csv("cleaned_data.csv")
data1$Attrition<-factor(data1$Attrition,
                        levels = c("Voluntary_Resignation","Current_employee"),
                        labels = c("Voluntary Resignation","Current Employee"))
data1$EnvironmentSatisfaction <- as.numeric(data1$EnvironmentSatisfaction)
averaged_data <- data1 %>%
  group_by(Department, Gender) %>%
  summarize(Average_Value = mean(EnvironmentSatisfaction))
ggplot(averaged_data, aes(x = Department, y = Average_Value, group = Gender, color = Gender)) +
  geom_line() +
  labs(x = "Department", y = "Average Environmental Satisfaction",
       color = "Gender") +
  theme_minimal()




















































