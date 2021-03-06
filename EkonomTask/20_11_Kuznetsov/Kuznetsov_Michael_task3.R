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

y <- data$�������
y
x <- data$����
x

cor(data) 

m <- lm(x~y)
m

sm <- summary(m)
sm

# ����� ��������� ���������
plot(y, x, col = 'red')

#������ 95% ��������
confint(m, level = 0.95) # ������������� ���������

# ��������� ���������� ������ ��������� � ����� � ������� ������������ ������ �� �����������.
A <- sum(abs(sm$residuals/y)) / length(y) * 100 # ������������
A
error_many <- sqrt(deviance(m)/df.residual(m)) #�� ������
error_many

determ <- sm$r.squared
adjust_determ <- sm$adj.r.squared
st_error <- error_many
approx <- A
f_test <- sm$fstatistic[1]

compare <- data.frame(
  �����������_������������=determ,
  �����������������_�����������=adjust_determ,
  �����������_������_������=st_error,
  ������_�������������=approx,
  F_����=f_test
)
print(compare)

#������ � �������� ������
"
������ ���������, R2 �� �������

"

#��������� �����
gqtest(m, order.by = x, fraction = 0.25) # ���� ����������-������� ��� x

bptest(m, studentize = TRUE) # ���� ������-������

dw <- dwtest(m) # ���� �������-�������
dw

bgtest(m, order = 1, order.by = NULL, type = c("Chisq", "F")) # ���� ������-������
bgtest(m, order = 2, order.by = NULL, type = c("Chisq", "F")) # ���� ������-������
bgtest(m, order = 3, order.by = NULL, type = c("Chisq", "F")) # ���� ������-������

#��������� �������� ��������������������

y2<-y/predict(m)
y2
x2<-x/predict(m)
x2
m2<-lm(y2~x2)
m2
sm2<-summary(m2)
sm2

gqtest(m2, order.by = x2, fraction = 0.25) # ���� ����������-������� ��� x2

bptest(m2, studentize = TRUE) # ���� ������-������
