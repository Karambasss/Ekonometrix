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

# Получение данных
setwd('C:/Users/mpapa/Documents/EkonomTask/Kuznetsov_Lab2/')
data <- read.table('data.txt', sep = '\t', dec = ',', header = TRUE, encoding = 'UTF-8')
data
cor(data)


dataOb = data[1:39,] #Обучающая выборка
dataOb
cor(dataOb)

dataKontr = data[40:40,] #контролирующая выборка
dataKontr


#спецификация модели: GPDt = -3.238e+06 +  1.618e+03*t - -4.189e+03*d1 - 2.772e+03*d2 - 1.188e+03*d3 + et

gdp <- dataOb$GDP
gdp
t <- dataOb$t
t
d1 <- dataOb$d1
d1
d2 <- dataOb$d2
d2
d3 <- dataOb$d3
d3

pm <- lm(gdp ~ t + d1 + d2 + d3)
pm

# Тесты на значимость модели
spm = summary(pm)
spm

determ <- spm$r.squared
determ

adjust_determ <- spm$adj.r.squared
adjust_determ

st_error <- sqrt(deviance(pm)/df.residual(pm))
st_error

approx <- sum(abs(spm$residuals/gdp)) / length(gdp) * 100
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
#Проверка на значимость модели регрессии: 
#2.2e-16 < 0.05 -> модель регрессии в целом значима

#проверка на значимость параметров модели:
#2e-16 < 0.05 -> константа модели в целом значима
# 2e-16 < 0.05 -> параметр модели в целом значим
# 2.09e-10 < 0.05 -> параметр модели в целом значим
#1.19e-06 < 0.05 -> параметр модели в целом значим
#0.0164 < 0.05 -> параметр модели в целом значим


#2 пункт:Проверьте выполнение предпосылки МНК об отсутствии автокорреляции.
# При обнаружении автокорреляции предложите подход к ее устранению
# Тесты на автокорреляцию
# Дарбина Уотсона
# H0: нет автокорреляции
# Ha: есть автокорреляция 1-го порядка
dw = dwtest(pm) # Тест Дарбина-Ватсона
dw
#p-val < 0.05 - отвергаем гипотезу об отсутствии автокорреляции, принимаем гипотезу о существовании автокоррелции

# Бреуша-Годфри
# H0: нет автокорреляции
# Ha: есть автокорреляция n порядка
bg = bgtest(pm,order = 1)
bg
bgtest(pm, order = 1, order.by = NULL, type = c("Chisq", "F"))
bgtest(pm, order = 2, order.by = NULL, type = c("Chisq", "F"))
bgtest(pm, order = 3, order.by = NULL, type = c("Chisq", "F"))
bgtest(pm, order = 4, order.by = NULL, type = c("Chisq", "F"))
#p-val < 0.05 -> Ha: есть автокорреляция n порядка

#Устраняем автокорреляцию
p1 = 1-(dw$statistic/2)
p1

gdp1<-gdp[2:39]-p1*gdp[1:38]
gdp1

t1 <- t[2:39]-p1*t[1:38]
t1

d11 <- d1[2:39]-p1*d1[1:38]
d11

d22 <- d2[2:39]-p1*d2[1:38]
d22

d33 <- d3[2:39]-p1*d3[1:38]
d33

pm1 = lm(gdp1~t1+d11+d22+d33)
pm1
spm1 = summary(pm1)
spm1

# Дарбина Уотсона
# H0: нет автокорреляции
# Ha: есть автокорреляция 1-го порядка
dw = dwtest(pm1) # Тест Дарбина-Ватсона
dw
#p-val > 0.05 - принимаем гипотезу об отсутствии автокорреляции

# Бреуша-Годфри
# H0: нет автокорреляции
# Ha: есть автокорреляция n порядка
bg = bgtest(pm1,order = 1)
bg
bgtest(pm1, order = 1, order.by = NULL, type = c("Chisq", "F"))
bgtest(pm1, order = 2, order.by = NULL, type = c("Chisq", "F"))
bgtest(pm1, order = 3, order.by = NULL, type = c("Chisq", "F"))
bgtest(pm1, order = 4, order.by = NULL, type = c("Chisq", "F"))
#p-val > 0.05 -> H0: нет автокорреляции

#ВЫВОД: С ПОМОЩЬЮ МНК МЫ ИСПРАВИЛИ АВТОКОРРЕЛЯЦИЮ

#Пункт 3 Выполните точечное и интервальное прогнозирование на 4 квартал 2018 г  и на 1 квартал 2019 г.

#прогнозирование по модели с фиктивными переменными 
future <- data.frame(
  t1 = c(40, 41),
  d11 = c(1, 0),
  d22 = c(0, 1),
  d33 = c(0, 0)
)
future
#точечное прогнозирование
predictGDP = predict(pm1, newdata = future)
predictGDP

#интервальное прогнозирование 
predict(pm1, newdata=future, interval="prediction")
predict(pm1, newdata=future, interval="confidence")

#Проверяем на адекватность модели (на основе прогноза для 4 квартала 2018 г.) - то есть gdp за 40 = -816707.3: 
# на основе полученных данных gdp за 4 квартал = -816707.3, проверим входит ли в интервалы от: (-976314.6 -657099.9) и (-976308.1 -657106.4) 
# -816707.3 входит в интервал (-976314.6 -657099.9)
# -816707.3 входит в интервал (-976308.1 -657106.4)
#соответственно, -816707.3 входит в оба интервала -> получается, модель адекватна!