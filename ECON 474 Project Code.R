## working directory
setwd("~/Desktop/Econometrics/ECON 474/Mask Project")

## install applicable packages
library(zoo)
library(sandwich)
library(lmtest)

## linear regression
data2 <- read.csv("data3.csv", header=TRUE)
attach(data2)
model1 <- lm(log(cases / pop) ~ treat + time + I(treat * time))
summary(model1)
coeftest(model1, vcov = sandwich)

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

## placebo test
## t1 is 4/17/20, t2 is 6/17/20, treatment date = 5/17/20 (chosen at random)
data5 <- read.csv("placebodata.csv", header=TRUE)
attach(data5)
model2 <- lm(log(cases / pop) ~ treat + time + I(treat * time), data = data5)
summary(model2)
coeftest(model2, vcov = sandwich)

## interpreting log transformation of dependent variable 
(exp(-7.627320) - 1) * 100
(exp(.336364) - 1) * 100
(exp(1.690166) - 1) * 100
(exp(-.017098) - 1) * 100

## placebo plots
data6 <- read.csv("placebotreat.csv", header = TRUE)
data7 <- read.csv("placebocontrol.csv", header = TRUE)
attach(data6)
attach(data7)
plot(data6$time, log(data6$cases/data6$pop), main = 'Indiana', col = 'blue')
plot(data7$time, log(data7$cases/data7$pop), main = 'Florida', col = 'red')
plot(data6$time, log(data6$cases/data6$pop), main = 'Indiana vs Florida  (Placebo Treatment)', xlab = 'Time',  ylab = 'Log(cases/population)', col = 'blue')
points(data7$time, log(data7$cases/data7$pop), col = 'red')
legend('bottomright', c('Indiana', 'Florida'), fill = c('blue', 'red'))
