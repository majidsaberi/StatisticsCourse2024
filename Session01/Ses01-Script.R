##Stats Course
#Instructor: Majid Saberi
#Session 1
#Feb 17th, 2024

##making an intelligent agent

days <- 170
daysofweek <- 7

est_weeks <- days/daysofweek

est_weeks <- function(days){
  daysofweek <- 7
  est_weeks <- days/daysofweek
  return(est_weeks)
}

est_weeks(1500)
est_weeks(167)
print(est_weeks(167))

##data types
#numeric
age <- 10

#character
name <- "ali"

#logical
passed <- TRUE
passed <- FALSE

#vector and dataframe
students <- c("negin","ali","fatemeh","mahsa","sina","parviz")
scores <- c(16.75,12,14.25,8,3,15)
pass <- c(T,T,T,F,F,T)
field <- c("bio","psych","psych","psych","bio","bio")
field <- factor(c("bio","psych","psych","psych","bio","bio"))
tabulate(field)

print(students)
print(scores)
print(pass)
print(field)

class(students)
class(pass)

length(students)

data <- data.frame(students,scores,pass,field)
print(data)

class(data)
dim(data)

##selection

students
students[1:3]
students[c(1,3,4)]
girls <- students[c(1,3,4)]
girls

students
students[students == "ali"]
students[students != "ali"]

scores
scores[scores > 15]

students
students[scores > 15]

pass
students
students[pass == "TRUE"]
students[!pass]

data
data$students
data$scores

data
data[,2]
data[3,]

data
data[,3:4]
data[c(1,2:4),c(1,3,4)]

data
data[data$field == "bio",]
data[data$field == "bio" & data$pass == TRUE,]

biopassed <- data[data$field == "bio" & data$pass == TRUE,]

#data modification

data
data[2,1] <- "amirali"

data
data[,2] <- data[,2] + 1

data
data$scores <- data$scores + 1.5

data
data$scores[data$pass == FALSE] <- 9.99 

data

#NA handeling

data
data$scores[data$scores < 10] <- NA

data
nadata <- data[is.na(data$scores),]
nadata

data
avadata <- data[!is.na(data$scores),]
avadata

data
data$scores[is.na(data$scores)] <- 9

data

#other variable types

mat <- matrix(nrow = 4, ncol = 5)
mat
mat[1:2,2:3]

list1 <- list(students,scores,data)
list1
list1[[1]]
list1[[3]]
