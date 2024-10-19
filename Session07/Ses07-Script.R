##Stats Course
#Instructor: Majid Saberi
#Session 7
#Apr 27th, 2024

##Clearing workspace
rm(list = ls())

##Loading required packages
library(ggplot2)
library(FSA)
library(coin)

##Setting the path
setwd("/Users/majid/Projects/StatsCourse/Session7/") #set to your path

##IQ Sample
pheno <- read.csv("Phenotypic.csv")
pheno[pheno == "-9999"] <- NA

####NYU site
pheno_NYU <- pheno[pheno$SITE_ID == "NYU"  ,]
tabulate(as.factor(pheno_NYU$DX_GROUP))

###FQI for NYU site 
FIQ_Control <- pheno_NYU$FIQ[pheno_NYU$DX_GROUP == 2] 
FIQ_ASD <- pheno_NYU$FIQ[pheno_NYU$DX_GROUP == 1] 

##Two group comparison

#Normality test
shapiro.test(FIQ_ASD)
shapiro.test(FIQ_Control)

#Statistical test
t.test(FIQ_Control,FIQ_ASD)

#Barplot
group_ <- c("Control","ASD")
mu_ <- c(mean(FIQ_Control),mean(FIQ_ASD))
sd_ <- c(sd(FIQ_Control),sd(FIQ_ASD))
se_ <- c(sd_[1]/sqrt(length(FIQ_Control)),sd_[2]/sqrt(length(FIQ_ASD)))
data <- data.frame(group_,mu_,sd_,se_)
# Standard deviation of the mean as error bar
p <- ggplot(data, aes(x=group_, y=mu_ , fill=group_)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mu_-se_, ymax=mu_+se_), width=.2,
                position=position_dodge(.9))

p <- p + 
  xlab("Group") +
  ylab("IQ") 

p

###ADOS for NYU site 
ADOS_Asperger <- pheno_NYU$ADOS_GOTHAM_TOTAL[pheno_NYU$DSM_IV_TR == 2]
ADOS_Autism <- pheno_NYU$ADOS_GOTHAM_TOTAL[pheno_NYU$DSM_IV_TR == 1]

#NA removing
ADOS_Asperger <- ADOS_Asperger[!is.na(ADOS_Asperger)]
ADOS_Autism <- ADOS_Autism[!is.na(ADOS_Autism)]

#Normality test
shapiro.test(ADOS_Autism)
shapiro.test(ADOS_Asperger)

#Statistical test
wilcox.test(ADOS_Autism,ADOS_Asperger)

#Boxplot
group_ <- c(rep("Autism",length(ADOS_Autism)),
            rep("Asperger",length(ADOS_Asperger)))
ados_ <- c(ADOS_Autism,ADOS_Asperger)
data <- data.frame(group_,ados_)

p<-ggplot(data, aes(x=group_, y=ados_, fill=group_)) +
  geom_boxplot(notch=TRUE) +
  xlab("") + 
  ylab("ADOS")
  
p

#####FIQ for three different sites
pheno3 <- pheno[ pheno$SITE_ID %in% c("CALTECH","KKI","YALE"),]
pheno3 <- data.frame(factor(pheno3$SITE_ID),pheno3$SUB_ID,factor(pheno3$DX_GROUP),pheno3$FIQ)
colnames(pheno3) <- c("SITE","ID","GROUP","FIQ")
pheno3 <- pheno3[!is.na(pheno3$FIQ),]
###Multiple group comparison

#Normality testing
shapiro.test(pheno3$FIQ[pheno3$SITE == "CALTECH"])
shapiro.test(pheno3$FIQ[pheno3$SITE == "KKI"])
shapiro.test(pheno3$FIQ[pheno3$SITE == "YALE"])

#Parametric testing
test <-aov(FIQ~SITE, pheno3) 
anova(test)

#Post-hoc evaluation
TukeyHSD(test)

#Non-parametric testing
test <-kruskal.test(FIQ~SITE, pheno3) 
test

#Post-hoc evaluation
dunnTest(FIQ~SITE, pheno3)

#Barplot
p<-ggplot(pheno3, aes(x=SITE, y=FIQ, fill=SITE)) +
  geom_boxplot(notch=TRUE) +
  xlab("") + 
  ylab("ADOS")

p

##Two way group comparison

#Parametric statistics
#Two way anova
test <-aov(FIQ~SITE+GROUP, pheno3) 
anova(test)

#One way anova (main effects) and post-hoc tests
test <-aov(FIQ~SITE, pheno3) 
anova(test)
TukeyHSD(test)

test <-aov(FIQ~GROUP, pheno3) 
anova(test)
TukeyHSD(test)

#Barplot
group_ <- c("CALTECH","KKI","YALE")
mu_ <- c(mean(pheno3$FIQ[pheno3$SITE=="CALTECH"])
          ,mean(pheno3$FIQ[pheno3$SITE=="KKI"])
          ,mean(pheno3$FIQ[pheno3$SITE=="YALE"]))
sd_ <- c(sd(pheno3$FIQ[pheno3$SITE=="CALTECH"])
         ,sd(pheno3$FIQ[pheno3$SITE=="KKI"])
         ,sd(pheno3$FIQ[pheno3$SITE=="YALE"]))
se_ <- c(sd_[1]/sqrt(sum(pheno3$SITE=="CALTECH"))
         ,sd_[2]/sqrt(sum(pheno3$SITE=="CALTECH"))
         ,sd_[3]/sqrt(sum(pheno3$SITE=="CALTECH")))
data <- data.frame(group_,mu_,sd_,se_)

p <- ggplot(data, aes(x=group_, y=mu_ , fill=group_)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mu_-se_, ymax=mu_+se_), width=.2,
                position=position_dodge(.9)) + 
  xlab("Group") +
  ylab("IQ") 

p 


group_ <- c("Control","ASD")
mu_ <- c(mean(pheno3$FIQ[pheno3$GROUP=="2"])
         ,mean(pheno3$FIQ[pheno3$GROUP=="1"]))
sd_ <- c(sd(pheno3$FIQ[pheno3$GROUP=="2"])
         ,sd(pheno3$FIQ[pheno3$GROUP=="1"]))
se_ <- c(sd_[1]/sqrt(sum(pheno3$GROUP=="2"))
         ,sd_[2]/sqrt(sum(pheno3$GROUP=="1")))
data <- data.frame(group_,mu_,sd_,se_)

p <- ggplot(data, aes(x=group_, y=mu_ , fill=group_)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mu_-se_, ymax=mu_+se_), width=.2,
                position=position_dodge(.9)) + 
  xlab("Group") +
  ylab("IQ") 

p 


#Non-parametric statistics
#Two way test
test <- independence_test(FIQ~SITE+GROUP, pheno3) 

#One way KS test (main effects) and post-hoc evaluations
test <-kruskal.test(FIQ~SITE, pheno3) 
test

dunnTest(FIQ~SITE, pheno3)

test <-kruskal.test(FIQ~GROUP, pheno3) 
test

dunnTest(FIQ~GROUP, pheno3)

#Boxplot
p<-ggplot(pheno3, aes(x=SITE, y=FIQ, fill=SITE)) +
  geom_boxplot(notch=TRUE) +
  xlab("") + 
  ylab("FIQ")

p

p<-ggplot(pheno3, aes(x=GROUP, y=FIQ, fill=GROUP)) +
  geom_boxplot(notch=TRUE) +
  xlab("") + 
  ylab("FIQ")

p
