##Stats Course
#Instructor: Majid Saberi
#Session 6
#Apr 13th, 2024

##Clearing workspace
rm(list = ls())

##Loading required packages

##Setting the path
setwd("/Users/majid/Projects/StatsCourse/Session6/") #set to your path

##IQ Sample
pheno <- read.csv("Phenotypic.csv")
pheno[pheno == "-9999"] <- NA

####NYU site
pheno_NYU <- pheno[pheno$SITE_ID == "NYU"  ,]
tabulate(as.factor(pheno_NYU$DX_GROUP))

###FQI for NYU site 
FIQ_Control <- pheno_NYU$FIQ[pheno_NYU$DX_GROUP == 2] #edit it
FIQ_ASD <- pheno_NYU$FIQ[pheno_NYU$DX_GROUP == 1] #edit it

##One sample comparison with the normal population
#Normality test
shapiro.test(FIQ_ASD)

iqN <- rnorm(100,mean(FIQ_ASD),sd(FIQ_ASD))
qqplot(iqN,FIQ_ASD)

#Main test
t.test(FIQ_ASD,mu=100,alternative = "greater")

#Two sample independent comparison between ASD and Control
#Normality test
shapiro.test(FIQ_ASD)
shapiro.test(FIQ_Control)

#Variance equality test
var.test(FIQ_Control,FIQ_ASD)

#Main test
t.test(FIQ_Control,FIQ_ASD,var.equal = F)

#Two sample dependent comparison between PIQ and VIQ for ASD subjects
VIQ_ASD <- pheno_NYU$VIQ[pheno_NYU$DX_GROUP == 1]
PIQ_ASD <- pheno_NYU$PIQ[pheno_NYU$DX_GROUP == 1]

#Normality test
shapiro.test(VIQ_ASD)
shapiro.test(PIQ_ASD)

#Variance equality test
var.test(VIQ_ASD,PIQ_ASD)

#Main test
t.test(VIQ_ASD,PIQ_ASD,var.equal = T,paired = T,alternative = "less")

###ADOS for NYU site 
ADOS_Asperger <- pheno_NYU$ADOS_GOTHAM_TOTAL[pheno_NYU$DSM_IV_TR == 2]
ADOS_Autism <- pheno_NYU$ADOS_GOTHAM_TOTAL[pheno_NYU$DSM_IV_TR == 1]
##One sample comparison with the normal population
#Normality test
shapiro.test(ADOS_Autism)
#Main test
wilcox.test(ADOS_Autism,mu = 0)
median(ADOS_Autism,na.rm = T)
#Two sample independent comparison between Asperger and Autism
#Normality test
shapiro.test(ADOS_Autism)
shapiro.test(ADOS_Asperger)
#Main test
wilcox.test(ADOS_Autism,ADOS_Asperger,alternative = "less")

#Assignment 1: For ASD group, compare VIQ of males and females
#Assignment 2: For ASD group, compare PIQ of males and females
#Assignment 3: For ASD group, compare PIQ and VIQ of males
#Assignment 4: For ASD group, compare PIQ and VIQ of females
#Assignment 5: For female ASD group, compare ADOS_Total between Autism and Asperger
#Assignment 6: For male ASD group, compare ADOS_Total between Autism and Asperger
#Assignment 7: For ASD group, exploratory find any significant differences in clinical measurements of females and males