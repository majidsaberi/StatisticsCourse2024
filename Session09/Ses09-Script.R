##Stats Course
#Instructor: Majid Saberi
#Session 9
#May 18th, 2024

##Clearing workspace
rm(list = ls())

##Loading required packages
library(ggplot2)
library(GGally)

##Setting the path
setwd("/Users/majid/Projects/StatsCourse/Session9/") #set to your path

##ASD subjects
pheno <- read.csv("Phenotypic.csv")
pheno[pheno == "-9999"] <- NA
pheno <- pheno[pheno$DX_GROUP == 1,]

#Selecting interesting variables
x1 <- pheno$AGE_AT_SCAN
x2 <- pheno$FIQ
x3 <- pheno$ADOS_TOTAL
x4 <- as.factor(pheno$SEX)

#Covariance analysis
cov(x1,x2)
cov(x1,x2,use = "na.or.complete")
cor(x1,x3,use = "na.or.complete")

plot(x1,x2)
plot(x2,x3)
cor(x2,x3,use = "na.or.complete")

cor(x2,x3,use = "na.or.complete",method = "pearson")

cor(x2,x3,use = "na.or.complete",method = "spearman")

hist(x1)
shapiro.test(x1)

hist(x2)
shapiro.test(x2)

#
#Regression analysis
data <- data.frame(x1,x2,x3,x4)
colnames(data) <- c("AGE","FIQ","ADOS","SEX")

model <- lm(FIQ ~ AGE , data) 
summary(model)

ggplot(data, aes(x = AGE, y = FIQ)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, col = "blue") +  
  labs(x = "Age",
       y = "Full IQ") +
  theme_minimal()

model <- lm(ADOS ~ AGE , data) 
summary(model)

ggplot(data, aes(x = AGE, y = ADOS)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, col = "blue") +  
  labs(x = "Age",
       y = "ADOS") +
  theme_minimal()

#General linear modeling
model <- lm(ADOS ~ AGE + FIQ , data) 
summary(model)

ggpairs(data[,1:3])

ggpairs(data, 
        mapping = aes(color = SEX),  
        upper = list(continuous = wrap("cor", size = 4)),  
        lower = list(continuous = wrap("smooth", method = "lm", se = TRUE)) 
)

#Interaction modeling
model <- lm(ADOS ~ AGE * FIQ , data) 
summary(model)

model <- lm(ADOS ~ AGE:FIQ , data) 
summary(model)

data <- data.frame(x1,x2,x3,x2*x3,x4)
colnames(data) <- c("AGE","FIQ","ADOS","FIQ*ADOS","SEX")

ggpairs(data, 
        mapping = aes(color = SEX),  
        upper = list(continuous = wrap("cor", size = 4)),  
        lower = list(continuous = wrap("smooth", method = "lm", se = TRUE)) 
)

#Analysis of covariance

model <- lm(ADOS ~ AGE , data) 
summary(model)

model <- lm(ADOS ~ AGE * SEX , data) 
summary(model)

p <- ggplot(data, aes(y = ADOS, x = AGE, color = SEX)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  
  labs(y = "ADOS", x = "AGE") +
  theme_minimal()
print(p)

##Assignments:
#Utilize Sex and three subtype of ADOS(social, communication, stereo behavior)
#Run correlation analysis between the subtypes with simple plots
#Estimate FIQ separately with general linear model versus subtypes
#Plot the result of GLM per sex
#Run ANCOVA analysis for FIQ vs. each subtype with SEX as covariate
#Plot Ancova results 
#run 
