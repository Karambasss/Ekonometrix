install.packages("MASS")

library(MASS)

options(digits=4)

set.seed(140)

sigma<-matrix(c(1,0.8,0.4,-0.6,
                
                0.8,1,0.7,-0.4,
                
                0.4,0.7,1,-0.1,
                
                -0.6, -0.4, -0.1, 1),
              
              nrow=4,ncol=4)

mean<-c(210, 196, 34, 46)

mydata<-mvrnorm(300,mean,sigma)

mydata<-as.data.frame(mydata)

names(mydata)<-c("y","x1","x2","x3")

#Проверьте, полученный набор данных

head(mydata,n=5)

#Далее можете сохранить свой набор данных

write.table(mydata, file = "/Users/mpapa/Documents/EkonomTask/KuznetsovData.txt", sep = "\t")

