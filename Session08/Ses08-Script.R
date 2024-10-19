##Stats Course
#Instructor: Majid Saberi
#Session 8
#May 11th, 2024

##Clearing workspace
rm(list = ls())

##Loading required packages
library(ggplot2)

##Setting the path
setwd("/Users/majid/Projects/StatsCourse/Session8/") #set to your path

##IQ Sample
pheno <- read.csv("Phenotypic.csv")
pheno[pheno == "-9999"] <- NA

####NYU site
pheno_NYU <- pheno[pheno$SITE_ID == "NYU"  ,]
tabulate(as.factor(pheno_NYU$DX_GROUP))

###FQI for NYU site 
VIQ_Control <- pheno_NYU$VIQ[pheno_NYU$DX_GROUP == 2] 
VIQ_ASD <- pheno_NYU$VIQ[pheno_NYU$DX_GROUP == 1] 
PIQ_Control <- pheno_NYU$PIQ[pheno_NYU$DX_GROUP == 2] 
PIQ_ASD <- pheno_NYU$PIQ[pheno_NYU$DX_GROUP == 1] 

data <- data.frame(c(pheno_NYU$SUB_ID,pheno_NYU$SUB_ID),
                   c(rep("VIQ",184),rep("PIQ",184)),
                   c(rep("Cont",105),rep("ASD",79),rep("Cont",105),rep("ASD",79)),
                   c(VIQ_Control,VIQ_ASD,PIQ_Control,PIQ_ASD))
colnames(data) <- c("ID","IQTYPE","GROUP","IQ")

##One way repeated measure test

#Parametric 
model <- aov(IQ ~ IQTYPE , data = data)
summary(model)

model <- aov(IQ ~ IQTYPE + Error(ID/IQTYPE), data = data)
summary(model)

#Non-parametric 
model <- friedman.test(IQ ~ IQTYPE | ID, data = data)
model

#Plotting
ggplot(data, aes(x=IQTYPE, y=IQ, group=ID)) +
  geom_line(alpha=.5) +
  geom_point(size=3,alpha=.5 ) +
  labs(title="One-Way Repeated Measure ANOVA",
       x="Condition",
       y="Score") 

ggplot(data, aes(x=IQTYPE, y=IQ, group=ID, color=GROUP)) +
  geom_line(alpha=.5) +
  geom_point(size=3,alpha=.5 ) +
  labs(title="One-Way Repeated Measure ANOVA",
       x="Condition",
       y="Score") 

##Interaction effects

#Main effects
model <- aov(IQ ~ IQTYPE + GROUP , data = data)
summary(model)

#Main effects + interaction effect
model <- aov(IQ ~ IQTYPE * GROUP , data = data)
summary(model)

model <- aov(IQ ~ IQTYPE * GROUP +  IQTYPE:GROUP, data = data)
summary(model)

#Interaction effect
model <- aov(IQ ~ IQTYPE:GROUP, data = data)
summary(model)

#Two-way repeated measure
model <- aov(IQ ~ IQTYPE * GROUP + Error(ID/IQTYPE * GROUP), data = data)
summary(model)

#Ploting
interaction.plot(x.factor=data$IQTYPE, trace.factor=data$GROUP, response=data$IQ,
                 type="b",
                 pch=19,
                 col=c("red", "blue"),
                 xlab="Factor A Levels", ylab="Response", legend=TRUE)

