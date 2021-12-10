install.packages('car')
library(lmtest)
library(car)

setwd('C:/Users/mpapa/Documents/EkonomTask/')
data <- read.table('KuznetsovData.txt', dec = '.', sep = '\t', header = TRUE, encoding = 'utf-8')
data

y <- data$y
x1 <- data$x1
x2 <- data$x2
x3 <- data$x3
y
x1
x2
x3
cor(data) # Корреляционная матрица
#Ранжировка: возьмем только те параметры, где корреляции наиболее сильна, это x1(0.8178), x3(-0.6332)


# Строю диаграммы рассеяния
plot(y, x1, col = 'red')
plot(y, x3, col = 'green')



# Парная модель
m <- lm(y~x1)
sm <- summary(m)
sm

A1 <- sum(abs(sm$residuals/y)) / length(y) * 100 # Апроксимация
A1

confint(m, level = 0.95) # Доверительные интервалы

beta1 <- 0.8271 # Бета коэффициент для x1
beta1
delta1 <- cor(y, x1) * beta1 / 0.669 # Дельта коэффициент для x1
delta1
elastic1 <- beta1 * mean(x1) / mean(y) # Коэффициент эластичности для x1
elastic1


dwtest(m) # Тест Дарбина-Ватсона
bgtest(m, order = 1, order.by = NULL, type = c("Chisq", "F")) # Тест Бреуша-Годфри
gqtest(m, order.by = x1, fraction = 0.25) # Тест Голдфельда-Квандта для x1
bptest(m, studentize = TRUE) # Тест Бреуша-Пагана


# Множественная модель
pm <- lm(y~x1+x2+x3)
spm <- summary(pm)
spm

A2 <- sum(abs(spm$residuals/y)) / length(y) * 100 # Апроксимация
A2

confint(pm, level = 0.95) # Доверительные интервалы

#если эластичность меньше 1 то зависимая переменная не эластичная по x1
beta2 <- 0.8113 # Бета коэффициент для x1
beta2
delta2 <- cor(y, x1) * beta2 / 0.777 # Дельта коэффициент для x1
delta2
elastic2 <- beta2 * mean(x1) / mean(y) # Коэффициент эластичности для x1
elastic2


beta2_1 <- -0.1703 # Бета коэффициент для x2
delta2_1 <- cor(y,x2) * beta2_1 / 0.777 # Дельта коэффициент для x2
delta2_1
elastic2_1 <- beta2_1 * mean(x2) / mean(y) # Коэффициент эластичности для x2
elastic2_1

beta3 <- -0.3004 # Бета коэффициент для x3
beta3
delta3 <- cor(y, x3) * beta3 / 0.777 # Дельта коэффициент для x3
delta3
elastic3 <- beta3 * mean(x3) / mean(y) # Коэффициент эластичности для x3
elastic3


dwtest(pm) # Тест Дарбина-Ватсона
bgtest(pm, order = 1, order.by = NULL, type = c("Chisq", "F")) # Тест Бреуша-Годфри
gqtest(pm, order.by = x1, fraction = 0.25) # Тест Голдфельда-Квандта для x1
bptest(pm, studentize = TRUE) # Тест Бреуша-Пагана


# Сравнение моделей
determ <- c(0.669, 0.777)
adjust_determ <- c(0.668, 0.774)
st_error <- c(0.606, 0.499)
approx <- c(0.2332, 0.1895)
f_test <- c(602, 343)

compare <- data.frame(
  Determinate=determ,
  Adjust_determinate=adjust_determ,
  Standart_error=st_error,
  Approximation=approx,
  F_test=f_test
)
#Стандартная ошибка - чем меньше тем лучше
#Детерминация чем больше тем лучше
#Adjust - меньше лучше
#Апроксимация лучше когда меньше
#F тест чем больше тем лучше
compare <- rbind(compare, list("plural model", "plural model", "plural model", "plural model", "twin model"))
compare
print('Выигрывает множественная модель по 4 из 5 пунктов')