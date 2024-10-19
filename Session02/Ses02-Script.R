##Stats Course
#Instructor: Majid Saberi
#Session 2
#Feb 24th, 2024

#setting path
setwd("/Users/majid/Projects/StatsCourse/Session2/")

#importing csv table
data <- read.csv("Scores.csv",sep = ",",row.names = NULL)

#exploring data format
class(data)

dim(data)

field <- data$Field
as.factor(field)
tabulate(as.factor(field))

#subset selection

data1 <- data[c(1,2,10:20),]

data2 <- data[-c(1,2,10:20),]

data3 <- data[,c(1,2,3)]

data3 <- data[,c("Stud","Field","Course1")]

data4 <- subset(data,data$Field == "Psych")

data5 <- subset(data,data$Course1 > 15 )

data6 <- subset(data,data$Course1 > 15 & data$Course3 < 10 )

data7 <- subset(data,is.na(data$Course2) )

write.csv(data7,"data7.csv")

#NA handeling
sum( is.na(data) )

which( is.na(data) )

which( is.na(data) ,arr.ind = T)

cond <- which( is.na(data) ,arr.ind = T)

data1 <- data[-cond,]

data2 <- data
data2[ is.na(data2) ] <- 0

#histogram inspection
hist(data$Course1)

hist(data$Course2, breaks = 20)

#descriptive statistics
hist(data$Course1)

mean(data$Course1)

mean(data$Course1,na.rm = T)

#sum(is.na(data$Course1))
#sum(data$Course1,na.rm = T) / 113

median(data$Course1,na.rm = T)

sort(data$Course1)

hist(data$Course1)
mean(data$Course1,na.rm = T)
median(data$Course1,na.rm = T)

hist(data$Course2)
mean(data$Course2,na.rm = T)
median(data$Course2,na.rm = T)
abline(v=mean(data$Course2,na.rm = T),col="red")
abline(v=median(data$Course2,na.rm = T),col="blue")

skewness(data$Course2,na.rm = T)

library(moments)
skewness(data$Course2,na.rm = T)

#
hist(data$Course1)

var(data$Course1,na.rm = T)

#mu <- mean(data$Course1,na.rm = T) 
#sum((data$Course1 - mu)^2,na.rm = T) / 112

sd(data$Course1,na.rm = T)

#mu <- mean(data$Course1,na.rm = T) 
#sqrt(sum((data$Course1 - mu)^2,na.rm = T)) / sqrt(112)

range(data$Course1,na.rm = T)

sort(data$Course1)

quantile(data$Course1,0.5,na.rm = T)
median(data$Course1,na.rm = T)
quantile(data$Course1,0.25,na.rm = T)
quantile(data$Course1,0.75,na.rm = T)

hist(data$Course1)
abline(v=median(data$Course1,na.rm = T))
abline(v=quantile(data$Course1,0.25,na.rm = T),na.rm = T,col="blue")
abline(v=quantile(data$Course1,0.75,na.rm = T),na.rm = T,col="red")

IQR(data$Course1,na.rm = T)

kurtosis(data$Course5,na.rm = T)

