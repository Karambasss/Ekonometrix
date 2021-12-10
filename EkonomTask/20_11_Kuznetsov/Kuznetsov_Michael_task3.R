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

setwd("C:/Users/mpapa/Documents/EkonomTask/20_11_Kuznetsov/")

data <- read.table("taskfor3.txt", dec = ',', fileEncoding = 'utf-8', header = TRUE)
data

y <- data$Площадь
y
x <- data$Цена
x

cor(data) 

m <- lm(x~y)
m

sm <- summary(m)
sm

# Строю диаграммы рассеяния
plot(y, x, col = 'red')

#Строим 95% интервал
confint(m, level = 0.95) # Доверительные интервалы

# Проверьте значимость модели регрессии в целом и каждого коэффициента модели по отдельности.
A <- sum(abs(sm$residuals/y)) / length(y) * 100 # Апроксимация
A
error_many <- sqrt(deviance(m)/df.residual(m)) #Ст ошибка
error_many

determ <- sm$r.squared
adjust_determ <- sm$adj.r.squared
st_error <- error_many
approx <- A
f_test <- sm$fstatistic[1]

compare <- data.frame(
  Коэффициент_детерминации=determ,
  Скорректированный_коэффициент=adjust_determ,
  Стандартная_ошибка_модели=st_error,
  Ошибка_аппроксимации=approx,
  F_тест=f_test
)
print(compare)

#Выводы о качестве модели
"
модель приемлима, R2 не идеален

"

#Выполняем тесты
gqtest(m, order.by = x, fraction = 0.25) # Тест Голдфельда-Квандта для x

bptest(m, studentize = TRUE) # Тест Бреуша-Пагана

dw <- dwtest(m) # Тест Дарбина-Ватсона
dw

bgtest(m, order = 1, order.by = NULL, type = c("Chisq", "F")) # Тест Бреуша-Годфри
bgtest(m, order = 2, order.by = NULL, type = c("Chisq", "F")) # Тест Бреуша-Годфри
bgtest(m, order = 3, order.by = NULL, type = c("Chisq", "F")) # Тест Бреуша-Годфри

#Устраняем проблему гетероскедастичности

y2<-y/predict(m)
y2
x2<-x/predict(m)
x2
m2<-lm(y2~x2)
m2
sm2<-summary(m2)
sm2

gqtest(m2, order.by = x2, fraction = 0.25) # Тест Голдфельда-Квандта для x2

bptest(m2, studentize = TRUE) # Тест Бреуша-Пагана
