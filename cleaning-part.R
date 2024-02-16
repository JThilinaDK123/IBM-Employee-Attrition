## Load the data set
df = read.csv("IBM HR Data new.csv")
dim(df)

## Rename column names
names(df)[names(df) == "Application ID"] <- "Application_ID"
names(df)[names(df) == "Employee Source"] <- "Employee_Source"

## Remove duplicates
duplicate_rows <- df[duplicated(df$Application_ID), ]

# Print the duplicate rows
print(duplicate_rows) ##### No duplicate rows in the dataset

## Dropping unwanted columns
drop_cols=c("Application.ID","EmployeeCount","EmployeeNumber",
            "Employee.Source", "Over18" , "StandardHours")
df[drop_cols]=NULL
summary(df)
head(df)

## Missing values
df = df[complete.cases(df), ]
summary(df)

## Other pre-processing steps

quantitative_cols <- c("DistanceFromHome", "HourlyRate", "JobSatisfaction",
                          "MonthlyIncome", "PercentSalaryHike")

# Remove rows with string values in specific quantitative columns
clean_df <- df[!rowSums(sapply(df[quantitative_cols], function(x) is.na(as.numeric(x)))), ]

qualitative_cols <- c("Department", "EducationField", "Gender",
                       "JobRole", "MaritalStatus")
clean_df <- clean_df[!rowSums(sapply(clean_df[qualitative_cols], function(x) !is.na(as.numeric(x)))), ]
clean_df = clean_df[complete.cases(clean_df), ]


## Factorized
summary(clean_df)
clean_df$Attrition=factor(clean_df$Attrition,
                          levels = c("Voluntary Resignation","Current employee"),
                          labels = c("Voluntary_Resignation","Current_employee"))

cols=c("BusinessTravel","Department","Education","EducationField","EnvironmentSatisfaction",
       "Gender","JobLevel","JobRole","JobSatisfaction","MaritalStatus", "JobInvolvement",
       "OverTime","PerformanceRating","RelationshipSatisfaction","StockOptionLevel",
       "WorkLifeBalance")

clean_df[cols] <- lapply(clean_df[cols], factor)

dim(clean_df)
clean_df$HourlyRate <- as.numeric(clean_df$HourlyRate)
clean_df$MonthlyIncome <- as.numeric(clean_df$MonthlyIncome)
clean_df$DistanceFromHome <- as.numeric(clean_df$DistanceFromHome)
clean_df$PercentSalaryHike <- as.numeric(clean_df$PercentSalaryHike)
clean_df[clean_df == ""] <- NA
clean_df <- na.omit(clean_df)
summary(clean_df)

# Write the data frame to a CSV file
write.csv(clean_df, file = "cleaned_data.csv", row.names = FALSE)


## Divide the dataset into training and testing
library(tidyverse)
library(caret)

prop <- clean_df %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

set.seed(111)
trainIndex <- createDataPartition(clean_df$Attrition, p=0.8, 
                                  list=FALSE, times=1)
train.data <- clean_df[trainIndex,]
test.data<- clean_df[-trainIndex,]

write.csv(train.data, file="traindata.csv", row.names = FALSE)
dim(train.data)
write.csv(test.data, file="testdata.csv", row.names = FALSE)
dim(test.data)
