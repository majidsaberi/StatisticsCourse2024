##Stats Course
#Instructor: Majid Saberi
#Session 3
#Mar 2nd, 2024

##setting path
rm(list = ls())

setwd("/Users/majid/Projects/StatsCourse/Session3/") #set to your path

##importing csv table
data <- read.csv("Session3-Scores.csv",sep = ",",row.names = NULL)

View(data)

##histogram

hist(data$Course2)

hist(data$Course2,breaks = 20)

hist(data$Course2,breaks = 20,probability = T)

hist(data$Course2,breaks = 20,probability = T,col = "blue")

hist(data$Course2,breaks = 20,probability = T,col = "lightblue"
     ,border = "darkblue",main = "Course2")

hist(data$Course2,breaks = 20,probability = T,col = "lightblue",
     border = "darkblue",main = "Course2",xlab = "Score")

hist(data$Course2,breaks = 20,probability = T,col = "lightblue",
     border = "darkblue",main = "Course2",xlab = "Score",ylab = "Density of Scores")

hist(data$Course2,breaks = 20,probability = T,col = "lightblue",
     border = "darkblue", main = "Course2",xlab = "Score",ylab = "Density of Scores",
     ylim = c(0,0.8))

#
abline(v=9)

abline(v=9.5,col="red")

abline(v=10,col="red",lwd=2)

abline(v=10.5,col="red",lwd=2,lty=3)

abline(h=0.5,col="violet",lwd=2,lty=3)

#assignment1: plot histogram of scores per courses indicating mean, Q1, and Q3 as vertical lines   

##scatter plot

plot(x = data$Course1, y = data$Course6)

plot(x = data$Course1, y = data$Course6,col="darkgreen")

plot(x = data$Course1, y = data$Course6,col="darkgreen",pch=4)

plot(x = data$Course1, y = data$Course6,col="darkgreen",pch=4,cex=1.5)

plot(x = data$Course1, y = data$Course6,col="darkgreen",pch=4,cex=1.5,
     main="Course Association",xlab = "Course1",ylab = "Course2")

abline(line(x = data$Course1, y = data$Course6))

abline(line(x = data$Course1, y = data$Course6),
       col="violet",lwd=5,lty=3)


#
plot(x = data$Course1[data$Field == "Bio"], 
     y = data$Course6[data$Field == "Bio"]
     ,col="darkgreen",pch=4,
     main="Course Association",xlab = "Course1",ylab = "Course6")

points(x = data$Course1[data$Field == "Psych"], 
     y = data$Course6[data$Field == "Psych"]
     ,col="darkred",pch=3)

legend("topleft",legend = c("Bio","Psych"),
       pch = c(4,3),col = c("darkgreen","darkred"))

abline(line(x = data$Course1[data$Field == "Psych"],
            y = data$Course6[data$Field == "Psych"]),
       col="red")

abline(line(x = data$Course1[data$Field == "Bio"],
            y = data$Course6[data$Field == "Bio"]),
       col="green")

#assignment2: scatter plot course1 versus the rest of them separately highlighting field differences

##pie chart
data$Field
as.factor(data$Field)
tabulate(as.factor(data$Field))

pie(x = c(56,59),labels = c("Bio","Psych"))

pie(x = c(56,59),labels = c("Bio","Psych"),col=c("violet","orange"))

##barplot
data$Field
as.factor(data$Field)
tabulate(as.factor(data$Field))

barplot(height = c(56,59))

barplot(height = c(56,59),names.arg = c("Bio","Psych"))

barplot(height = c(56,59),names.arg = c("Bio","Psych"),
        col=c("violet","orange"))

barplot(height = c(56,59),names.arg = c("Bio","Psych"),
        col=c("violet","orange"),
        main = "Field",ylim = c(0,70),ylab = "Number of Students")

#
mean(data$Course2[data$Field=="Bio"],na.rm = T)
mean(data$Course2[data$Field=="Psych"],na.rm = T)

barplot(height = c(9.9,9.8),names.arg = c("Bio","Psych"),
        col=c("violet","orange"),
        main = "Course2",ylim = c(0,20),ylab = "Mean of Scores")

#assignment3: provide field based mean score barplot for each course individually

##boxplot

colnames(data)

boxplot(Course1 ~ Field, data = data)

boxplot(Course1 ~ Field, data, col="red")

boxplot(Course1 ~ Field, data, col=c("violet","orange"))

boxplot(Course1 ~ Field, data, col=c("violet","orange"),ylim=c(10,18),
        main="Course1",xlab = "",ylab = "Score")

boxplot(Course1 ~ Field, data, col=c("violet","orange"),
        main="Course1",xlab = "",ylab = "Score")

#assignment4: provide field based boxplot for each course individually

##exporting graphical devices

pdf("Boxplot.pdf")
boxplot(Course1 ~ Field, data, col=c("violet","orange"),
        main="Course1",xlab = "",ylab = "Score")
dev.off()

pdf("Boxplot.pdf",width = 4 ,height = 4)
boxplot(Course1 ~ Field, data)
boxplot(Course1 ~ Field, data, col=c("violet","orange"),
        main="Course1",xlab = "",ylab = "Score")
boxplot(Course3 ~ Field, data, col=c("violet","orange"),
        main="Course1",xlab = "",ylab = "Score")
dev.off()

#generate assignment figures as pdf files
