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
students <- c("negin","ali","fatemeh","mahsa","sina","parviz") #error
scores <- c(16.75 ,12,14.25,8,3 ,15 ) #error
pass <- c(T,T,T,F,F,T) #error
field <- c("bio","psych","psych","psych","bio","bio")
field <-  factor(c("bio" ,"psych","psych","psych","bio","bio")) #error
tabulate(field)

print( students )
print(scores)
print(pass)
print(field)

class(students)
class(pass) #error

length(students) #error

data <- data.frame(students,scores,pass,field) #error
print(data) #error

class(data)
dim(data)

##selection

students
students[1:3] #error
students[c(1,3,4)] #error
girls <- students[c(1,3,4)] #error
girls

students
students[students == "ali"] #error
students[students != "ali"] #error

scores
scores[scores > 10] #error

students
students[scores < 10] #error

pass
students
students[pass == "FALSE"] #error?
students[!(pass)] #error?

data
data$students #error
data$scores

data
data[,2]
data[3] #error

data
data[1:2,3:4]
data[c(1,2,4),3:4] #error

data
data[data$field == "bio",] #error
data[data$field == "bio" & data$pass == TRUE,]

biopassed <- data[data$field == "bio" & data$pass == T,] #error

#data modification

data
data[2,1] <- amirali #error

data
data[,2] = data[,2] + 1 #error?

data
data$scores <- data$scores + "1.5" #error

data
data$scores[data$pass == FALSE] <- 9.99 

data

#NA handeling

data
data$scores[data$scores < 10] <- na #error

data
nadata <- data[isna(data$scores),] #error
nadata

data
avadata <- data[!is.na(data.scores),] #error
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
