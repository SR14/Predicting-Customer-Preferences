# Summary: 
# Will use regression models to predict customer 
# preference based on product type.


# Summarize Top Model:


rm(list = ls())
getwd()
setwd("/Users/sergiorobledo/Desktop")
dir()


install.packages("caret")
install.packages("corrplot")
install.packages("readr")
install.packages("doMC")
library(caret)
library(corrplot)
library(doMC)
library(mlbench)
library(readr)
detectCores()
registerDoMC(cores=4)
install.packages("corrplot")
install.packages("e1071")
library(e1071)
install.packages("gbm")
library(gbm)
install.packages("randomForest")
library(randomForest)
library(corrplot)

Existing <- read.csv("existing.csv", stringsAsFactors = FALSE, header = T)
class(Existing)
str(Existing)
summary(Existing)
head(Existing)
tail(Existing)
names(Existing)
attributes(Existing)

#dummify the data
dmy <- dummyVars("~.",data=Existing)
readyData <- data.frame(predict(dmy, newdata = Existing))
str(readyData)
summary(readyData)
readyData$BestSellersRank <- NULL

corrData <- cor(readyData)
corrData
corrplot(corrData)

featuredData <- readyData
featuredData$ProductNum <- NULL
featuredData$ProductTypeAccessories <- NULL
featuredData$ProductDepth <- NULL
featuredData$ProductTypeDisplay <- NULL
featuredData$ProductTypeExtendedWarranty <- NULL
featuredData$ProductTypeGameConsole <- NULL
featuredData$ProductTypePrinter <- NULL
featuredData$ProductTypePrinterSupplies <- NULL
featuredData$ProductTypeSoftware <- NULL
featuredData$ProductTypeTablet <-NULL
featuredData$ShippingWeight <- NULL
featuredData$ProductWidth <- NULL
featuredData$ProductHeight <- NULL
featuredData$ProfitMargin <- NULL
str(featuredData)

featuredData2 <- featuredData[sample(1:nrow(featuredData),40,replace = FALSE),]
head(featuredData2)
nrow(featuredData2)

set.seed(123)
inTraining <- createDataPartition(featuredData2$Volume, p=0.75, list = FALSE)
trainSet <- featuredData2[inTraining,]
testSet <- featuredData2[-inTraining,]
nrow(trainSet)
nrow(testSet)

fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)

set.seed(123)
lmFit1 <- train(Volume~., data = trainSet, method="leapSeq", trControl=fitControl)
predictors(lmFit1)
lmFit1
set.seed(123)
rfFit1 <- randomForest(Volume~., data = trainSet, trControl=fitControl)
rfFit1
set.seed(123)
gbmFit1 <- gbm(Volume~.,data = trainSet,n.trees = 100)
gbmFit1

lmPred1 <- predict(lmFit1,testSet)
postResample(lmPred1,testSet$Volume)
lmPred1
rfPred1 <- predict(rfFit1,testSet)
postResample(rfPred1,testSet$Volume)
rfPred1
gbmPred1 <- predict(gbmFit1,testSet, n.trees = 100)
postResample(gbmPred1,testSet$Volume)
gbmPred1

### New Product Predictions Begin ###

Predictions <- read.csv("newproducts.csv", stringsAsFactors = FALSE, header = T)
class(Predictions)
str(Predictions)
summary(Predictions)
head(Predictions)
tail(Predictions)
names(Predictions)
attributes(Predictions)

#dummify the data
dmy2 <- dummyVars("~.",data=Predictions)
readyData2 <- data.frame(predict(dmy2, newdata = Predictions))

str(readyData2)
summary(readyData2)
readyData2$BestSellersRank <- NULL

featuredData3 <- readyData2
featuredData3$ProductNum <- NULL
featuredData3$ProductTypeAccessories <- NULL
featuredData3$ProductDepth <- NULL
featuredData3$ProductTypeDisplay <- NULL
featuredData3$ProductTypeExtendedWarranty <- NULL
featuredData3$ProductTypeGameConsole <- NULL
featuredData3$ProductTypePrinter <- NULL
featuredData3$ProductTypePrinterSupplies <- NULL
featuredData3$ProductTypeSoftware <- NULL
featuredData3$ProductTypeTablet <-NULL
featuredData3$ShippingWeight <- NULL
featuredData3$ProductWidth <- NULL
featuredData3$ProductHeight <- NULL
featuredData3$ProfitMargin <- NULL
str(featuredData3)

finalPred <- predict(rfFit1,featuredData3)
finalPred

output <- Predictions
output$Volume <- finalPred

write.csv(output, file="C2.T3output.csv",row.names = TRUE)
