rm(list=objects())
library(tidyverse)
library(lubridate)
library(randomForest)
library(GGally)
library(caret)
library(bst)
library(plyr)
library(elasticnet)
source('Modèles linéaires/score.R')

Data = read_delim("Data/train.csv", delim=",")
Goal = read_delim("Data/test.csv", delim=",")
#####Feature transformation---------------------------
Data$WeekDays = as.factor(Data$WeekDays)
Data$BH = as.factor(Data$BH)
Data$WeekDays2 <- weekdays(Data$Date)

Data$WeekDays3 <- forcats::fct_recode(Data$WeekDays2, 'WorkDay'='Thursday' ,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday', 'WorkDay'='Monday')

Goal$WeekDays = as.factor(Goal$WeekDays)
Goal$BH = as.factor(Goal$BH)
Goal$WeekDays2 <- weekdays(Goal$Date)

Goal$WeekDays3 <- forcats::fct_recode(Goal$WeekDays2, 'WorkDay'='Thursday' ,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday', 'WorkDay'='Monday')

train.control <- trainControl(summaryFunction = PinballLoss, method = "cv", number = 10)
model <- train(Net_demand ~ Temp+Wind+Nebulosity + toy + WeekDays3 + BH + Net_demand.1 + Net_demand.7 + DLS, data = Data, method = "lm",
               trControl = train.control, metric = "PinballLoss")

summary(model)
####----------------------------------------------------------------------------
lm.forecast = predict(model, newdata = Goal)

submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Net_demand <- lm.forecast
write.table(submit, file="Data/Submission_randomforest.csv", quote=F, sep=",", dec='.',row.names = F)