install.packages("MASS")
library(MASS)
install.packages("lmtest")
library(lmtest)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)
install.packages("orcutt")
library(orcutt)
install.packages("sandwich")
library(sandwich)
install.packages("psych")
library(psych)
install.packages("regclass")
library(regclass)
install.packages("mctest")
library(mctest)
install.packages("urca")
library(urca)

setwd('C:/Users/mpapa/Documents/EkonomTask/20_12_Kuznetsov_Pi19-4/')
data <- read.table('data.txt', sep = ' ', dec = ',', header = TRUE, encoding = 'UTF-8')

data

lag <- data.frame(
  y = data$y[3:length(data$y)],
  x0 = data$x[3:length(data$y)],
  x1 = data$x[2:(length(data$y) - 1)],
  x2 = data$x[1:(length(data$y) - 2)]
)

lag

train <- lag[1:(length(lag$y) - 1),]
train
test <- lag[(length(lag$y)):length(lag$y),]
test

m <- lm(y ~ x0 + x1 + x2, data = train)
cor(data)
m
spm = summary(m)
spm

determ <- spm$r.squared
determ

adjust_determ <- spm$adj.r.squared
adjust_determ

st_error <- sqrt(deviance(m)/df.residual(m))
st_error

approx <- sum(abs(spm$residuals/data$y)) / length(data$y) * 100
approx

f_test <- spm$fstatistic[1]
f_test

compare <- data.frame(
  Коэффициент_детерминации=determ,
  Скорректированный_коэффициент=adjust_determ,
  Стандартная_ошибка_модели=st_error,
  Ошибка_аппроксимации=approx,
  F_тест=f_test
)
print(compare)


gqtest(m, fraction = 0.3)
bptest(m)
dwtest(m)
bgtest(m, type = "F")

b <- coef(m)
b
b[2] * 0 + b[3] * 1 + b[4] * 2 # Средний лаг
b[2] # Краткосрочный мультипликатор
sum(b[2:length(b)]) # Долгосрочный мультипликатор
waldtest(lm(y ~ x0, data=train), m)

# Точечный прогноз
predict(m, newdata = test)
# Интервальный прогноз
predict(m, newdata = test, interval = "confidence")
predict(m, newdata = test, interval = "prediction")

