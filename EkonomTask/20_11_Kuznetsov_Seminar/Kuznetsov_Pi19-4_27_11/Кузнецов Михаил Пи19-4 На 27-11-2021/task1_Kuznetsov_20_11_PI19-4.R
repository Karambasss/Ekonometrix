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

setwd("C:/Users/mpapa/Documents/EkonomTask/20_11_Kuznetsov_Seminar/")

data <- read.table("tema7.txt", dec = ',', fileEncoding = 'utf-8', header = TRUE)
data

dataNew = data[1:20,] #Обучающая выборка
dataNew

dataNew1 = data[21:22,] #контролирующая выборка
dataNew1

y = dataNew$y
y
x1 = dataNew$x1
x1
x2 = dataNew$x2
x2
x3 = dataNew$x3
x3
x4 = dataNew$x4
x4

pm = lm(y~x1+x2+x3+x4)
pm

spm = summary(pm)
spm

#y = 14981.0221 + 0.4473*x1 + 36.6988 * x2 - 19.1364* x3 - 324.5281* x4

data_test = dataNew1[1,]
data_test
test_y = data_test$y
test_y
test_x1 = data_test$x1
test_x1
test_x2 = data_test$x2
test_x2
test_x3 = data_test$x3
test_x3
test_x4 = data_test$x4
test_x4

test_res = 14981.0221 + 0.4473 * test_x1 + 36.6988 * test_x2 - 19.1364 * test_x3 - 324.5281 * test_x4
test_res

predict(pm,newdata=dataNew1, interval="prediction") #Индивидуальный
#у больше чем интервал от lwr до upr
#модель не адекватна

predict(pm,newdata=dataNew1, interval="confidence") #Доверительный
#y с крышечкой = fit
#наша у больше чем интервал от lwr до upr
#модель не адекватна

spm

cor(dataNew)#исключаем x2 так как cor с у маленький

pmNew = lm(y~x1+x3+x4)
pmNew

spmNew = summary(pmNew)
spmNew

#интервалы
predict(pmNew,newdata=dataNew1, interval="prediction") #Индивидуальный

predict(pmNew,newdata=dataNew1, interval="confidence") #Доверительный


#БЕз x2 и x3
pmNew2 = lm(y~x1+x4)
pmNew2

spmNew2 = summary(pmNew2)
spmNew2

#интервалы
predict(pmNew2,newdata=dataNew1, interval="prediction") #Индивидуальный

predict(pmNew2,newdata=dataNew1, interval="confidence") #Доверительный


#Строим парную модель
m = lm(y~x4)
m
sm = summary(m)
sm

#Интервалы
predict(m,newdata=dataNew1, interval="prediction") #Индивидуальный

predict(m,newdata=dataNew1, interval="confidence") #Доверительный

#Делаем тест Чоу
#берем модель со всеми 4 факторами
data2=data[1:10,] #разбиваем на 2 равнозначные выборки
data2
data3=data[11:20,]
data3
#Строим 2 модели по новым data2
m1 = lm(y~x1+x2+x3+x4, data = data2)
m1
m2 = lm(y~x1+x2+x3+x4, data = data3)
m2
sm1 = summary(m1)
sm1

sm2 = summary(m2)
sm2
#Проводим тест чоу
rss = sum(pm$residuals^2) #rss (общая модель по всем наблюдениям с 1 по 20 наблюдения)
rss

rss1 = sum(m1$residuals^2) #rss1  (для первой модели с (1 по 10 наблюдениям))
rss1

rss2 = sum(m2$residuals^2) #rss2 ( для второй модели ( с 11 по 20 наблюдениям))
rss2

f = (rss - (rss1 + rss2)) / (rss1 + rss2) * (10 + 10 - 2 * (4 + 1)) / (4 + 1) #ф наблюдаемое
f


#qf(0.95,k+1,n1+n2-2*(k+1))


F_kr = qf(0.95,4+1,10+10-2*(4+1)) #Ф кр по фишеру
F_kr
#H0 гипотеза: Выборка является однородной, нет структурного сдвига
#H1 гипотеза; ВЫборка не является однородной, есть структурный сдвиг

# у нас F < Fкр -> h0 не отвергаем, соответственно, выборка однородна, нет структурного сдвига

#Строим с самым сильным фактором (по матрице парных коэф) y с x
cor(dataNew) #x4 с y имеют самую тесную взаимосвязь

#проверяем остатки на нормальность
ostatki = spm$residuals
ostatki

jarque.bera.test(ostatki)
#h0- остатки нормально распределены
#h1-остатки ненормально распределены

#P-val > alpha; h0 принимаем, остатки норм распределены

cor(data)

#строим парную модель по y и x4
m
sm
#Проводим тест вальда
waldtest(pm, m)
#H0: короткая модель
#Н1: длинная модель
qf(0.95,3,20-5)
#Fнабл > Fкр -> h1, длинная модель

#ПРОВОДИМ ТЕСТ РАМСЕЯ

y_m = predict(pm)
y_m

y_m2 = y_m^2
y_m2

y_m3 = y_m^3
y_m3

pm_NEW3 = lm(y~x1+x2+x3+x4+y_m2+y_m3)
pm_NEW3

spmN3 = summary(pm_NEW3)
spmN3

#Проводим тест вальда для Новой модели с 6 факторами и старой модели с 4 факторами
waldtest(pm_NEW3, pm)

resettest(pm_NEW3)

resettest(pm)
