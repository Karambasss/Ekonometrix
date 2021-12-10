install.packages("lmtest")
library(lmtest)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)
install.packages("orcutt")
library(orcutt)
setwd("C:/Users/mpapa/Documents/EkonomTask/6_11_Kuznetsov/")

data <- read.table("dannie8.txt", dec = ',', fileEncoding = 'utf-8', header = TRUE)
data

y=data$Yt
y
x1=data$X1t
x1
x2=data$X2t
x2

pm <- lm(y~x1+x2)
pm

cochrane.orcutt(pm)
