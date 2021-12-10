install.packages("lmtest")
library(lmtest)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)
install.packages("car")
library(car)

setwd("C:/Users/mpapa/Documents/EkonomTask/30_10_Kuznetsov/task2/Season")
data <- read.table("data_fict.txt",dec = ',', fileEncoding = 'utf-8', header = TRUE)
data2 <- read.table("data_fict.txt",dec = ',', fileEncoding = 'utf-8', header = TRUE)
data3 <- read.table("data_season.txt",dec = ',', fileEncoding = 'utf-8', header = TRUE)
data

y <- data$IP
x <- data$t
cor(data)

p_many <- lm(y~x)
s_many <- summary(p_many)
e <- s_many$residuals
e
s_many

A_many <- sum(abs(s_many$residuals/y)) / length(y) * 100 # Апроксимация
A_many
error_many <- sqrt(deviance(p_many)/df.residual(p_many))

determ <- s_many$r.squared
adjust_determ <- s_many$adj.r.squared
st_error <- error_many
approx <- A_many
f_test <- s_many$fstatistic[1]

data['e'] = data.frame(e)
data['Остаток'] = 0

#Работа с устреденеием остатков
ost1 = sum(data[data$Q == 1,]$e)/10
ost1

ost2 = sum(data[data$Q == 2,]$e)/10
ost2

ost3 = sum(data[data$Q == 3,]$e)/10
ost3

ost4 = sum(data[data$Q == 4,]$e)/10
ost4

# Добавление столбца с сезонными составляющими-остатками
data[data$Q == 1,]$Остаток = ost1
data[data$Q == 2,]$Остаток = ost2
data[data$Q == 3,]$Остаток = ost3
data[data$Q == 4,]$Остаток = ost4

data$IP <- (data$IP - data$Остаток)
m2 = lm(IP~t, data=data2)

plot(data$IP, col="red") #график

#прогнозирование по модели 
predictIP = predict(m2, newdata = data3)
predictIP

data3['Прогнозирование'] = data.frame(predictIP)
data3

write.csv(data3,"./data_season_results.csv", row.names = TRUE)

