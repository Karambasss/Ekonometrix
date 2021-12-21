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


setwd("C:/Users/mpapa/Documents/EkonomTask/18_12_Kuznetsov_Seminar/")

data <- read.table("data.txt", dec = ',', fileEncoding = 'utf-8', header = TRUE)
data

dataNew = data[1:13,] #Обучающая выборка
dataNew

dataNew1 = data[14:14,] #контролирующая выборка
dataNew1

y = dataNew$Y
y
xt = dataNew$Xt
xt
xt1 = dataNew$XT.1
xt1
xt2=dataNew$Xt.2
xt2

#65.82+0,05461*xt
m = lm(y~xt)
m

sm = summary(m)
sm

#130.98 -0.07816*xt + 0,05820*xt-1 - 0,46392*xt-2
pm = lm(y~xt+xt1+xt2)
pm

cor(dataNew)

spm = summary(pm)
spm

determ <- spm$r.squared
determ

adjust_determ <- spm$adj.r.squared
adjust_determ

st_error <- sqrt(deviance(pm)/df.residual(pm))
st_error

approx <- sum(abs(spm$residuals/y)) / length(y) * 100
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

dw = dwtest(pm) # Тест Дарбина-Ватсона
dw
bg = bgtest(pm,order = 1)
bg
gqtest(pm, order.by = xt, fraction = 0.25) # Тест Голдфельда-Квандта для x
bptest(pm, studentize = TRUE) # Тест Бреуша-Пагана

yNew = dataNew1$Y
yNew
xtNew = dataNew1$Xt
xtNew
xt1New = dataNew1$XT.1
xt1New
xt2New = dataNew1$Xt.2
xt2New

#краткосрочный b1 
b1 = spm$coefficients[2]
b1 
############################

b2 = spm$coefficients[3]
b2

b3 = spm$coefficients[4]
b3
#долгосрочный b1
b1ALL = sum(spm$coefficients[2:4])
b1ALL
#bi вклад отдельного лага
bxt = b1/b1ALL
bxt

bxt1 = b2/b1ALL
bxt1

bxt2 = b3/b1ALL
bxt2

#среднего лага
sr = bxt*1+bxt1*2+bxt2*3
sr
