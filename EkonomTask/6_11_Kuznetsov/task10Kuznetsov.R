install.packages("lmtest")
library(lmtest)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)
setwd("C:/Users/mpapa/Documents/EkonomTask/6_11_Kuznetsov/")

data <- read.table("dannie10.txt", dec = ',', fileEncoding = 'utf-8', header = TRUE)
data
y=data$Y
y
x1=data$X
x1
pm <- lm(y~x1)
pm

vcov(pm)
