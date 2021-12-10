install.packages("lmtest")
library(lmtest)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)
install.packages("car")
library(car)

setwd("C:/Users/mpapa/Documents/EkonomTask/30_10_Kuznetsov/task2/Fict_var")
data <- read.table("data_fict.txt", dec = ',', fileEncoding = 'utf-8', header = TRUE)
data

y <- data$IP
x1 <- data$t
x2 <- data$d1
x3 <- data$d2
x4 <- data$d3

# Корреляционная матрица
cor(data)

# Диаграммы рассеяния
plot(y, x1, col = 'black')
plot(y, x2, col= 'red')
plot(y, x3, col = 'blue')
plot(y, x4, col = 'orange')

p_many <- lm(y~x1+x2+x3+x4)
s_many <- summary(p_many)
s_many

A_many <- sum(abs(s_many$residuals/y)) / length(y) * 100 # Апроксимация
A_many

error_many <- sqrt(deviance(p_many)/df.residual(p_many))

confint(p_many, level = 0.95) # Доверительные интервалы

determ <- s_many$r.squared
adjust_determ <- s_many$adj.r.squared
st_error <- error_many
approx <- A_many
f_test <- s_many$fstatistic[1]

# Проверьте значимость модели регрессии в целом и каждого коэффициента модели по отдельности.
compare <- data.frame(
  Коэффициент_детерминации=determ,
  Скорректированный_коэффициент=adjust_determ,
  Стандартная_ошибка_модели=st_error,
  Ошибка_аппроксимации=approx,
  F_тест=f_test
)
print(compare)


#прогнозирование по модели с фиктивными переменными
pred=data.frame(c(41,42,43,44,45,46,47,48),c(1,0,0,0,1,0,0,0),c(0,1,0,0,0,1,0,0),c(0,0,1,0,0,0,1,0))
colnames(pred)=c("x1","x2","x3","x4")
predictIP=predict(p_many, newdata = pred)
predictIP
pred

