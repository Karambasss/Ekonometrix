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

setwd("C:/Users/mpapa/Documents/EkonomTask/")

data <- read.table("taskfor1.txt", dec = ',', fileEncoding = 'utf-8', header = TRUE)
data

x = data$X
x
y=data$Y
y

sm = lm(y~x)
sm

dw = dwtest(sm) # Тест Дарбина-Ватсона

cochrane.orcutt(sm)  #в данном случае невозможно использование данного метода

#1 путь устранить, используя взвешенные мнк
#2 путь устранить, используя обобщенные мнк

#3 путь расчет рабастных корреляционных матриц
vcovHAC(sm)

bg = bgtest(sm,order = 1)
bg #вывод: автокорреляция 1 порядка присутствует

gqtest(sm, order.by = x, fraction = 0.25) # Тест Голдфельда-Квандта для x
#в гольф кванта присутствует проблема гомоскедастичности

bptest(sm, studentize = TRUE) # Тест Бреуша-Пагана
#в бреуш присутствует проблема гетероскедастичности

#Проблема гетероскедастичности присутствует

# устраняем автокоррреляцию
#dw = 2(1-p1)

#p1 = 1-(dw/2)
p1 = 1-(dw$statistic/2)
p1

y1<-y[2:55]-p1*y[1:54]
y1

x1 <- x[2:55]-p1*x[1:54]
x1

nm = lm(y1~x1)
nm

snm= summary(nm)
snm

#y= a+bx

#y*=z0+z1x*
#z1=b

#a=z0/(1-p)

b = nm$coefficients[2]
b

a=nm$coefficients[1]/(1-p1)
a

dw = dwtest(nm) # Тест Дарбина-Ватсона
dw
bg = bgtest(nm,order = 1)
bg
gqtest(nm, order.by = x1, fraction = 0.25) # Тест Голдфельда-Квандта для x
bptest(nm, studentize = TRUE) # Тест Бреуша-Пагана

