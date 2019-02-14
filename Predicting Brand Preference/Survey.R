rm(list = ls())
getwd()
setwd("/Users/sergiorobledo/Desktop")
dir()

install.packages("caret")
install.packages("crrplot")
install.packages("readr")
library(caret)
library(corrplot)
library(readr)

install.packages("doMC")
library(doMC)
library(mlbench)
detectCores()
registerDoMC(cores=4)



# Load Data #
Complete <- read.csv("CompleteResponses.csv", stringsAsFactors = FALSE)
class(Complete)
str(Complete)
Incomplete <- read.csv("SurveyIncomplete.csv", stringsAsFactors = FALSE)



# Evaluate Data #
str(Complete)
summary(Complete)
head(Complete)
tail(Complete)
names(Complete)
attributes(Complete)
hist(Complete$brand)
plot(Complete$salary,Complete$brand)
qqnorm(Complete$brand)
anyNA(Complete)
is.na(Complete)


# Preprocess Data #
Complete$elevel<-as.factor(Complete$elevel)
Complete$car<-as.factor(Complete$car)
Complete$zipcode<-as.factor(Complete$zipcode)
Complete$brand<-as.factor(Complete$brand)

Incomplete$elevel<-as.factor(Incomplete$elevel)
Incomplete$car<-as.factor(Incomplete$car)
Incomplete$zipcode<-as.factor(Incomplete$zipcode)
Incomplete$brand<-as.factor(Incomplete$brand)


### No Features Removed ###


# Sampling #
set.seed(123) # set random seed
sample_complete <- Complete[sample(1:nrow(Complete), round(nrow(Complete)*.2),replace=FALSE),]
nrow(sample_complete)
head(sample_complete) # ensure randomness


# Train & Test Sets #
set.seed(123)
inTraining <- createDataPartition(sample_complete$brand,p=0.750,list = FALSE)
trainSet <- sample_complete[inTraining,]
testSet <- sample_complete[-inTraining,]
str(trainSet)
str(testSet)

# Load Necessary Packages for Models #
install.packages("inum")
library(inum)
install.packages("e1071")
library(e1071)
install.packages("randomForest")
library(randomForest)


# Train Control (10 Fold Cross Validation) #
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

### Train the Models ###

# C5.0 #
#set.seed(123)
#C5_Fit <- train(brand~.,data = trainSet,method="C5.0",trControl=fitControl,tuneLength = 2)
#C5_Fit
#varImp(C5_Fit)

# RF #
set.seed(123)
rdGrid <- expand.grid(mtry=c(2,4,8,16,32))
rfFit <- train(brand~.,data = trainSet,method="rf",importance = T ,trControl=fitControl, tuneGrid=rdGrid)
rfFit
varImp(rfFit)

# Evaluate Models #
ModelFitResults <- resamples(list(C5.0=C5_Fit,rf=rfFit))
summary(ModelFitResults)

# Save Top Model #
saveRDS(rfFit,"RFmod.rds")
RFfit14 <- readRDS("RFmod.rds")

# Prediction
rfBrand <- predict(RFfit14,Incomplete)
postResample(rfBrand,testSet$brand)
summary(rfBrand)
rfBrand
write.csv(rfBrand, "rfBrand.csv")
