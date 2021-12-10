install.packages("lmtest")
library(lmtest)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)
setwd("C:/Users/mpapa/Documents/EkonomTask/30_10_Kuznetsov/")

#Спецификация модели: yt=a=bt+c1d1+c2d2+c3d3+et

data <- read.table("dannie.txt", dec = ',', fileEncoding = 'utf-8', header = TRUE)
data
y=data$GDP
y
x1=data$t
x1
x2=data$df1
x2
x3=data$df2
x3
x4=data$df3
x4

# Корреляционная матрица
cor(data)

#Спецификация модели: yt=a=bt+c1d1+c2d2+c3d3+et

#Строим графики
plot(y, col = 'red')
plot(y, x1, col="orange")

# Множественная модель
pm <- lm(y~x1+x2+x3+x4)
pm
spm <- summary(pm)
spm
confint(pm, level = 0.95) # Доверительные интервалы

# Проверьте значимость модели регрессии в целом и каждого коэффициента модели по отдельности.
#работа с временным рядом GDP 

A2 <- sum(abs(spm$residuals/y)) / length(y) * 100 # Апроксимация
A2

error_many <- sqrt(deviance(pm)/df.residual(pm)) #Ст ошибка
error_many

determ <- spm$r.squared
adjust_determ <- spm$adj.r.squared
st_error <- error_many
approx <- A2
f_test <- spm$fstatistic[1]

compare <- data.frame(
  Коэффициент_детерминации=determ,
  Скорректированный_коэффициент=adjust_determ,
  Стандартная_ошибка_модели=st_error,
  Ошибка_аппроксимации=approx,
  F_тест=f_test
)
print(compare)

#прогнозирование по модели с фиктивными переменными
pred<-data.frame(c(41,42,43),c(1,0,0),c(0,1,0),c(0,0,1)) # Строим прогноз на новый 2019 год
pred
colnames(pred)<-c("x1","x2","x3","x4")
pred
predictGDP<-predict(pm,newdata = pred)
predictGDP

#Строим временной ряд
GDP<-c(y)
GDP
GDP1<-ts(data=GDP,start=c(2009), name="GDP", frequency = 4)
GDP1
plot(GDP1,col="red")

#Делаем тесты
dwtest(pm) # Тест Дарбина-Ватсона
bgtest(pm, order = 1, order.by = NULL, type = c("Chisq", "F")) # Тест Бреуша-Годфри
gqtest(pm, order.by = x1, fraction = 0.25) # Тест Голдфельда-Квандта для x1
bptest(pm, studentize = TRUE) # Тест Бреуша-Пагана

#Делаем PACF, ACF
ACF <- Acf(GDP1) # выборочная автокорреляция
ACF
PACF <- Pacf(GDP1) # частная автокорреляиця 
PACF

#ДЕЛАЕМ ADF тест (на стационарность)
adf.test(GDP1)

#Делаем Phillips-Perron Unit Root Test
PP.test(GDP1) 

#Делаем Box-Pierce test
Box.test(GDP1, lag=10, type=c("Box-Pierce", "Ljung-Box"))

#Делаем KPSS Test for Level Stationarity
kpss.test(GDP1)
