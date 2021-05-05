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


# data Explortion 

plot(Exchange_usd$`YYYY/MM/DD`)  # Plotting the YYYY/MM/DD Column
plot(Exchange_usd$`USD/EUR`)   # Plotting the USD/EUR Column


Exchange_usd$`YYYY/MM/DD` <- as.Date(Exchange_usd$`YYYY/MM/DD`) # Convert character string column to date


timeseries_data <- xts(Exchange_usd$`USD/EUR`,Exchange_usd$`YYYY/MM/DD`) # convert excel file to time series
timeseries_data   # Print time series

# target and predictor features
rate_original <-(timeseries_data)
rate_leg <- stats::lag(rate_original,2)
rate_all <- cbind(rate_original,rate_leg)
colnames(rate_all) <- c('rate_original', 'rate_leg')
rate_all <- na.exclude(rate_all)

# Normalizing 
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

nomralizes_data <- as.data.frame(lapply(rate_all,normalize))

# Training and testing ranges
rate_train <- window(rate_all, end = '2013-05-21')
rate_test <- window(rate_all, start = '2013-05-21')

plot(rate_train) # plotting the train data 


set.seed(123)
# ANN Regression Fitting

nueral_fit <- neuralnet(rate_original~rate_leg, data=rate_train, hidden=1, act.fct= tanh) #ternage
nueral_fit$result.matrix

# Graphic Neural network
plot(nueral_fit) 



# Test the accuracy of the model
temp_test <- subset(rate_test, select = 'rate_leg')
head(temp_test)
nueral_fit.results <- compute(nueral_fit, temp_test)
results <- data.frame(actual = rate_test$rate_original, predicted= nueral_fit.results$net.result)
results

predicted <- nueral_fit.results$net.result
actual <- rate_test$rate_original

RMSE(predicted,actual) # score 
RMSE(predicted,actual)

plot(actual,main = "Real vs predicted", col='red')





