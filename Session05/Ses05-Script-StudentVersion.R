##Stats Course
#Instructor: Majid Saberi
#Session 5
#Mar 30th, 2024

##Clearing workspace
rm(list = ls())

##Loading required packages
library(TeachingDemos)
library(effsize)

##Setting the path
setwd("/Users/majid/Projects/StatsCourse/Session5/") #set to your path

##IQ Sample
pheno <- read.csv("Phenotypic.csv")
pheno[pheno == "-9999"] <- NA

#FQI for Asperger's syndrome 
FIQ_asperger <- pheno$FIQ[pheno$DSM_IV_TR == 2] #edit it

#Emitting NA values
FIQ_asperger <- FIQ_asperger[FIQ_asperger != NA] #edit it
FIQ_asperger <- FIQ_asperger[!is.na(FIQ_asperger)] #edit it

#Estimating sample average
mean(FIQ_asperger) #edit it

#Examination of Z test 
z.test(FIQ_asperger,stdev = 15,mu = 100,alternative = "greater")

#Examination of Cohen's d effect size 
cohen.d(FIQ_asperger,mu = 100,f=NA)

# Assignment: For each DSM-5 group, define the null and alternative hypotheses. 
#             Examine Z statistics and corresponding effect sizes.

z.test(FIQ_asperger,stdev = 15,mu = 100,alternative = "two.sided")

z.test(FIQ_asperger,stdev = 15,mu = 100,alternative = "greater")

