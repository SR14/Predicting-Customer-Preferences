rm(list = ls())

install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("doMC")
library(doMC)

getwd()
setwd("/Users/sergiorobledo/Desktop")
dir()

detectCores()
registerDoMC(cores = 4)

Transactions <- read.transactions("ElectronidexTransactions.csv",format = "basket",sep = ",",rm.duplicates = TRUE)
summary(Transactions)
inspect(Transactions)
head(Transactions)
length(Transactions)
size(Transactions)
LIST(Transactions)

itemFrequencyPlot(Transactions,type="relative")
itemdetails <- itemFrequency(Transactions, type= "absolute")
summary(itemdetails)

image(sample(Transactions, 125))

RulesName <- apriori(Transactions,parameter = list(supp=0.01,conf=0.5,minlen=2))
summary(RulesName)
inspect(RulesName)
is.redundant(RulesName)
topConfidence <- sort(RulesName,decreasing=TRUE,by="confidence")
inspect(topConfidence)
topSupport <- sort(RulesName,decreasing=TRUE,by="support")
inspect(topSupport)
topLift <- sort(RulesName,decreasing=TRUE,by="lift")
inspect(topLift)

iMacRules <- subset(RulesName,items %in% "iMac")
inspect(iMacRules)
HPLaptopRules <- subset(RulesName,items %in% "HP Laptop")
inspect(HPLaptopRules)
CyberRules <- subset(RulesName,items %in% "CYBERPOWER Gamer Desktop")
inspect(CyberRules)

plot(RulesName)
plot(topLift[1:10],method="graph",control = list(type="items"))
