setwd("~/Desktop/Econometrics/ECON 474/Mask Project Data")

## install applicable packages
library(zoo)
library(sandwich)
library(lmtest)

## linear regression
data2 <- read.csv("data3.csv", header=TRUE)
attach(data2)
model1 <- lm(log(cases / pop) ~ treat + time + I(treat * time))
summary(model)
coeftest(model, vcov = sandwich)

## interpreting log transformation of dependent variable 
(exp(-6.37675) - 1) * 100
(exp(.62326) - 1) * 100
(exp(2.78885) - 1) * 100
(exp(-1.49837) - 1) * 100

## plots
data3 <- read.csv("treatdata.csv", header = TRUE)
data4 <- read.csv("controldata.csv", header = TRUE)
attach(data3)
attach(data4)
plot(data3$time, log(data3$cases/data3$pop), main = 'Indiana', col = 'blue')
plot(data4$time, log(data4$cases/data4$Pop), main = 'Florida', col = 'red')
plot(data3$time, log(data3$cases/data3$pop), main = 'Indiana vs Florida', xlab = 'Time',  ylab = 'Log(cases/population)', col = 'blue')
points(data4$time, log(data4$cases/data4$Pop), col = 'red')
legend('bottomright', c('Indiana', 'Florida'), fill = c('blue', 'red'))




