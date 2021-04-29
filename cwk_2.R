library(tidyverse)
library(tseries)
library(readxl)
library(neuralnet)
library(quantmod)
library(xts)
library(funtimes)
library(dplyr)
library(MLmetrics)

Exchange_usd <- read_excel("D:/Accadamic Materials/IIT/02nd Year/Second Semester/Machine Learnig/CourseWork/ExchangeUSD.xlsx") 
Exchange_usd


  



nrow(Exchange_usd) # calculating row
ncol(Exchange_usd) # Calculating column

# data Explortion 

plot(Exchange_usd$`YYYY/MM/DD`)
plot(Exchange_usd$`USD/EUR`)

class(Exchange_usd)  # check the class of the data

Exchange_usd$`YYYY/MM/DD` <- as.Date(Exchange_usd$`YYYY/MM/DD`) # Convert character string column to date




data_ts <- xts(Exchange_usd$`USD/EUR`,Exchange_usd$`YYYY/MM/DD`) # convert excel file to time series
data_ts    # Print time series

# target and predictor features
rate_original <-(data_ts)
rate_leg <- stats::lag(rate_original,3)
rate_all <- cbind(rate_original,rate_leg)
colnames(rate_all) <- c('rate_original', 'rate_leg')
rate_all <- na.exclude(rate_all)

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

nomralizes_data <- as.data.frame(lapply(rate_all,normalize))
nomralizes_data


# Training and testing ranges
rate_train <- window(rate_all, end = '2013-05-21')
rate_test <- window(rate_all, start = '2013-05-21')

plot(rate_train)


set.seed(123)
# ANN Regression Fitting

annt <- neuralnet(rate_original~rate_leg, data=rate_train, hidden=1, act.fct= tanh) #ternage
annt$result.matrix

# Graphic Neural network
plot(annt) 



# Test the acuuracy of the model
temp_test <- subset(rate_test, select = 'rate_leg')
head(temp_test)
annt.results <- compute(annt, temp_test)
results <- data.frame(actual = rate_test$rate_original, predicted= annt.results$net.result)
results

predicted <- annt.results$net.result
actual <- rate_test$rate_original

dat <-data.frame(actual=actual, predicted = predicted)


RMSE(predicted,actual) # score 

plot(actual, predicted, col='red')







