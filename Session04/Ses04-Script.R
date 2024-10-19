##Stats Course
#Instructor: Majid Saberi
#Session 4
#Mar 16nd, 2024

##setting path
rm(list = ls())

setwd("/Users/majid/Projects/StatsCourse/Session4/") #set to your path

##IQ Population
iq <- dnorm(1:200,mean = 100,sd = 15)
plot( iq , type = "l" )

plot( iq , type = "l" , xlab = "IQ" ,  ylab = "Density" , main = "Population IQ") #edit

q50 <- qnorm(.5, mean = 100,sd = 15)

q84 <- qnorm(.5 + .34, mean = 100,sd = 15)

abline(v=q50)

abline(v=q84,col="red",lty=5,lwd=2) #set line type and line color

#plot again and include q25, q50, and q75
plot( iq , type = "l" , xlab = "IQ" ,  ylab = "Density" , main = "Population IQ") #edit

q25 <- qnorm(.25, mean = 100,sd = 15)
q50 <- qnorm(.50, mean = 100,sd = 15) #edit
q75 <- qnorm(.75, mean = 100,sd = 15) #edit

abline(v=q25 ,col="red",lty=2)
abline(v=q50 ,col="darkred",lty=1 , lwd=3)
abline(v=q75 ,col="red",lty=2)

##IQ Sample
pheno <- read.csv("Phenotypic.csv")
pheno[pheno == "-9999"] <- NA
#FQI for autistic subjects collected at USM site 
pheno_ASD_USM <- pheno[pheno$SITE_ID == "USM" & pheno$DX_GROUP == "1",]

FIQ <- pheno_ASD_USM$FIQ

hist(FIQ,probability = T)

FIQ_mean <- mean(FIQ)
FIQ_sd <- sd(FIQ)

lines(dnorm(1:200,mean = FIQ_mean,sd = FIQ_sd),
       xlim = c(min(FIQ),max(FIQ)) )

#
hist(FIQ,probability = T,col = "lightblue",main = "USM (Autistics)")

lines(dnorm(1:200,mean = FIQ_mean,sd = FIQ_sd),
      xlim = c(min(FIQ),max(FIQ)) ,
      col="blue",lwd=2,lty=3)

q25 <- quantile(FIQ,.25)
q50 <- quantile(FIQ,.5)
q75 <- quantile(FIQ,.75)

abline(v=q50,col="darkgreen",lwd=4)
abline(v=q25,col="grey",lwd=4,lty=2)
abline(v=q75,col="grey",lwd=4,lty=2)

#Assignment 1: plot FIQ figure for NYU site, include a normal density function and quartiles
pheno_ASD_NYU <- pheno[pheno$SITE_ID == "" & pheno$DX_GROUP == "",] #edit

##Population Parameter Estimation

IQ_class <- readLines("IQclass.txt")
IQ_level <- readLines("IQlevel.txt")
IQ_school <- readLines("IQschool.txt")

hist(IQ_class) #?

IQ_class <- as.numeric(IQ_class)
IQ_level <- as.numeric(IQ_level)
IQ_school <- as.numeric(IQ_school)

hist(IQ_class,breaks = seq(40,160,10))
hist(IQ_level,breaks = seq(40,160,10))
hist(IQ_school,breaks = seq(40,160,10))

#
library(sciplot)
library(gplots)
library(lsr)

pheno_ASD_NYU <- pheno[pheno$SITE_ID == "NYU" & pheno$DX_GROUP == "1",]

bargraph.CI(x.factor = SEX, response = FIQ ,data = pheno_ASD_NYU)

bargraph.CI(x.factor = SEX, response = FIQ ,data = pheno_ASD_NYU,
            col = c("pink","lightblue") ,main = "NYU (Autistics)")

#Assignment 2: Barplot denoting SEM FIQ per sex for each site separatly across autistic subjects   

#Assignment 3: Barplot denoting SEM FIQ per site across autistic subjects    

#Assignment 4: Barplot denoting SEM FIQ per site just for males across autistic subjects    
