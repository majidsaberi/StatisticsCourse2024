##Stats Course
#Instructor: Majid Saberi
#Session 10
#June 8th, 2024

##Clearing workspace
rm(list = ls())

##Loading required packages
#ploting
library(lattice)
#ICA running
library(fastICA)
#Machine learning
library(caret)
library(cluster)

##Setting the path
setwd("/Users/majid/Projects/StatsCourse/Session10/") #set to your path

##Autism and Aspergers
pheno <- read.csv("Phenotypic.csv")
pheno[pheno == "-9999"] <- NA
pheno <- pheno[pheno$DX_GROUP == 1,]
pheno <- pheno[pheno$DSM_IV_TR %in% c(1,2),]


##Non-linear regression
#Selecting interesting variables
x <- pheno$WISC_IV_PSI
y <- pheno$ADOS_TOTAL

#Dataset making
data <- data.frame(x,y)
#Removing NAs
data <- data[!is.na(data$x) & !is.na(data$y) , ]
#Ordering rows versus X
data <- data[order(data$x),]

#Scatter plot
plot(data$x, data$y, xlab = "WISC_PSI", ylab = "ADOS_TOTAL")

#Fitting linear model
model1 <- lm(y~x , data)
summary(model1)

#Fitted model
pred1 <- predict(model1,data)

plot(data$x, data$y, xlab = "WISC_PSI", ylab = "ADOS_TOTAL")
lines(data$x,pred1)

#Fitting quadratic model
model2 <- nls(y ~ a + b * x + c * I(x^2), data = data,
              start = list(a = 1, b = 1, c = 1))
summary(model2)

#Fitted model
pred2 <- predict(model2,data)

plot(data$x, data$y, xlab = "WISC_PSI", ylab = "ADOS_TOTAL")
lines(data$x,pred2)

plot(data$x, data$y, xlab = "WISC_PSI", ylab = "ADOS_TOTAL")
lines(data$x,pred1,col="blue")
lines(data$x,pred2,col="red")

#Comparing models
AIC(model1)
AIC(model2)

anova(model2,model1)

#Assignment1: Fit a linear and a quadratic model between WISC_IV_PSI and FIQ
# and check which model is the optimized one

##Dimensional reduction
colnames(pheno)

#ADOS sub-measures
ADOS <- pheno[,c(22:24,26:29)]
rownames(ADOS) <- pheno$SUB_ID
#Selecting those subjects with all measures
ADOS <- ADOS[apply(ADOS,1,function(x) all(!is.na(x))),]

##PCA analysis
dim(ADOS)
pca <- prcomp(ADOS)

#SD of PCs
plot(pca$sdev)

#Rotation matrix
rotmat <- pca$rotation
plot(rotmat[,1])

#Transformed data
newdata <- pca$x
plot(newdata[,1])

levelplot(rotmat,xlab="",ylab="",
          scales = list(x = list(rot = 90)),
          at = seq(-1,1,0.1),
          col.regions = colorRampPalette(c("blue", "white", "red"))
          )

range(newdata)
levelplot(newdata,xlab="",ylab="",
          scales = list(x = list(rot = 90,cex=.5)),
          at = seq(-20,20),
          col.regions = colorRampPalette(c("blue", "white", "red"))
          )

#Assignment 2: Explain the role of scale. argument and 
# plot rotation matrix and newdata matrix by activating this option 
#Assignment 3: Explain the role of center argument and 
# plot rotation matrix and newdata matrix by deactivating this option 

##ICA analysis
ica <- fastICA(ADOS,n.comp = 4)

#Mixing matrix
mixmat <- ica$A
colnames(mixmat) <- colnames(ADOS)
rownames(mixmat) <- paste("IC",seq(4),sep="")

#Sources (Transformed data)
newdata <- ica$S
colnames(newdata) <- paste("IC",seq(4),sep="")

range(mixmat)
levelplot(mixmat,xlab="",ylab="",
          scales = list(x = list(rot = 90)),
          at = seq(-5,5,0.1),
          col.regions = colorRampPalette(c("blue", "white", "red"))
)

range(newdata)
levelplot(newdata,xlab="",ylab="",
          scales = list(x = list(rot = 90,cex=.5)),
          at = seq(-5,5),
          col.regions = colorRampPalette(c("blue", "white", "red"))
)

#Assignment 4: Plot mixing and newdata matrices for 2-4 number of components,
  #propose a solution for selecting the optimized number of comps and implement it

##Machine learning: classification
#Data set making
ADOS <- pheno[,c(22:24,26:29)]
data <- data.frame(ADOS,as.factor(pheno$SEX) )

colnames(data)[8] <- "SEX"
data <- data[apply(data,1,function(x) all(!is.na(x))),]

#Setting training parameters
train_control <- trainControl(method = "cv", number = 10)

#Model making
model <- train(SEX ~ ., data = data, method = "glmnet",
               trControl = train_control)
print(model)

model <- train(SEX ~ ., data = data, method = "pls",
               trControl = train_control)
print(model)

model <- train(SEX ~ ., data = data, method = "svmLinear",
               trControl = train_control)
print(model)

model <- train(SEX ~ ., data = data, method = "rf",
               trControl = train_control)
print(model)

#List of available machine learning algorithms
#https://rdrr.io/cran/caret/man/models.html

#Assignment 5: Classify subjects based on DSM-IV diagnostics categories, 
 #plot performances of classifiers as a bar plot

##Machine learning: regression
#Data set making
ADOS <- pheno[,c(22:24,26:29)]
data <- data.frame(ADOS,pheno$AGE_AT_SCAN )

colnames(data)[8] <- "AGE"
data <- data[apply(data,1,function(x) all(!is.na(x))),]

#Setting training parameters
train_control <- trainControl(method = "cv", number = 10)

#Model making
model <- train(AGE ~ ., data = data, method = "glmnet",
               trControl = train_control)
print(model)

model <- train(AGE ~ ., data = data, method = "pls",
               trControl = train_control)
print(model)

model <- train(AGE ~ ., data = data, method = "svmLinear",
               trControl = train_control)
print(model)

model <- train(AGE ~ ., data = data, method = "rf",
               trControl = train_control)
print(model)

#Assignment 6: Classify subjects based on social communication questionnaire, 
#plot performances of classifiers as a bar plot


##Machine learning: clustering
data <- pheno[,c(22:24,26:29)]
rownames(data) <- pheno$SUB_ID
data <- data[apply(data,1,function(x) all(!is.na(x))),]

#Hierarchical clustering
hc <- hclust(dist(data), method = "ward.D2")

# Plot the dendrogram
plot(hc, cex = 0.6, hang = -1)

#Cutting the dendrogram into a specified number of clusters
rect.hclust(hc, k = 2)
#Labeling subjects
clusters <- cutree(hc, k = 2)
clusters
#Clustering quality assessment
sil <- silhouette(clusters, dist(data))
mean(sil[,3])

#Assignment 7: Run clustering with different methods and K=2-3,
# make a table containing silhouette means
# plot the best clustering with the largest silhouette mean
# qualitatively explore any relevance between clustering labels and phenotypical data
