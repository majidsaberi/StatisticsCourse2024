##Stats Course
#Instructor: Majid Saberi
#Session 2
#Feb 24th, 2024

#setting path
setwd("/Users/majid/Projects/StatsCourse/Session2") #change it

#importing csv table
data <- read.csv("Scores.csv",sep = ",",row.names = NULL) #error

#exploring data format
class(data)

dim(data)

field <- data$Field 
as.factor(field)
tabulate(as.factor(field))

#subset selection

data1 <- data[c(1,2,10:20),] #change it

data2 <- data[-c(1,2,10:20),] #change it

data3 <- data[,c(1,2,3)] #change it

data3 <- data[,c("Stud","Field","Course1")] #change it

data4 <- subset(data,data$Field == "Bio") #change it

data5 <- subset(data,data$Course1 > 15 ) #change it

data6 <- subset(data,data$Course1 > 15 & data$Course3 < 10 ) #change it

data7 <- subset(data,!is.na(data$Course3) ) #change it

write.csv(data4,"data4.csv",sep=";") #change it

#NA handeling
sum( is.na(data) )

which( is.na(data) )

which( is.na(data) ,arr.ind = T)

cond <- which( is.na(data) ,arr.ind = T) #error

data1 <- data[-cond[,1],]

data2 <- data
data2[ is.na(data2) ] <- 0 #change it

#histogram inspection
hist(data$Course1)

hist(data$Course5, breaks = 20) #change it

#descriptive statistics
hist(data$Course1)

mean(data$Course1)

mean(data$Course1,na.rm = T) #error

#sum(is.na(data$Course1))
#sum(data$Course1,na.rm = T) / 113

median(data$Course4,na.rm = T) 

sort(data$Course4)

hist(data$Course4) 
mean(data$Course4,na.rm = T)
median(data$Course4,na.rm = T)

hist(data$Course4) #change it
mean(data$Course4,na.rm = T) #change it
median(data$Course4,na.rm = T) #change it
abline(v=mean(data$Course4,na.rm = T),col="red") #change it
abline(v=median(data$Course4,na.rm = T),col="blue") #change it

skewness(data$Course2,na.rm = T)

library(moments)

skewness(data$Course4,na.rm = T)

#
hist(data$Course1) #change it

var(data$Course1,na.rm = T) #change it

#mu <- mean(data$Course1,na.rm = T) 
#sum((data$Course1 - mu)^2,na.rm = T) / 112

sd(data$Course1,na.rm = T) #change it

#mu <- mean(data$Course1,na.rm = T) 
#sqrt(sum((data$Course1 - mu)^2,na.rm = T)) / sqrt(112)

range(data$Course1,na.rm = T) #change it

sort(data$Course1)

quantile(data$Course1,0.5,na.rm = T)
median(data$Course1,na.rm = T)
quantile(data$Course1,0.25,na.rm = T)
quantile(data$Course1,0.75,na.rm = T)

hist(data$Course1) #change it
abline(v=median(data$Course1,na.rm = T)) #change it
abline(v=quantile(data$Course1,0.25,na.rm = T),na.rm = T,col="blue") #change it
abline(v=quantile(data$Course1,0.75,na.rm = T),na.rm = T,col="red") #change it

IQR(data$Course1,na.rm = T) #change it

hist(data$Course5)
kurtosis(data$Course5,na.rm = T) #change it

