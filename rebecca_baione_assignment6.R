library(tidyr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(devtools)
library(pastecs)
library(reshape)
library(reshape2)
library(tidyverse)

rm(list=ls())
FRB_data <- read.csv("C:/Users/baionr/Downloads/FRB_H8.csv", header = TRUE)
attach(FRB_data)
tf<-is.na(F) #Filters out NULL values, there were none so FRB and F are equivalent
F <- FRB_data[!tf] #tf is FALSE
summary(FRB_data)

#ASSETS/LIABILITIES - CURRENT RATIO - LIQUIDITY
summary(Total.assets)
summary(Total.liabilities)
boxplot(Total.assets,Total.liabilities)
plot(ecdf(Total.assets),do.points=FALSE,verticals=TRUE)
plot(ecdf(Total.liabilities),do.points=FALSE,verticals=TRUE)

hist(Total.assets) 
lines(density(Total.assets,na.rm=TRUE,bw="SJ"))
rug(Total.assets)
hist(Total.liabilities)
lines(density(Total.liabilities,na.rm=TRUE,bw="SJ"))
rug(Total.liabilities)

CR <- FRB_data$Total.assets/FRB_data$Total.liabilities
summary(CR)
plot(CR)

#LIABILITY/BANKCREDIT - DEBT-EQUITY - SOLVENCY
DE<- FRB_data$Total.liabilities/FRB_data$Bank.credit
summary(DE)
plot(DE)

#BANK CREDIT - NET WORTH - PROFITABILITY
hist(Bank.credit) 
summary(Bank.credit)
fivenum(Bank.credit)
stem(Bank.credit)
plot(ecdf(Bank.credit),do.points=FALSE, verticals =TRUE)
qqnorm(Bank.credit)
qqplot(qt(ppoints(250), df = 100),Bank.credit,xlab="Q-Q plot for Bank Credit")
qqline(Bank.credit)

#TIME-SERIES MODELS
FRB_ts <- ts(FRB_data, frequency = 51)
require(graphics)
ts.plot(FRB_ts, gpars = list(xlab="Week",main="FRB_ts"))
ts.plot(Total.assets, gpars=list(xlab="Week",ylab="Assets", main="Assets"))
ts.plot(Total.liabilities, gpars = list(xlab="Week", ylab="Liabilities", main="Liabilities"))
ts.plot(CR,gpars = list(xlab="week",ylab="Current Ratio", main="Current Ratio"))
ts.plot(DE, gpars = list(xlab="week",ylab="Debt-Equity Ratio", main="Debt-Equity Ratio"))
ts.plot(Bank.credit, gpars = list(xlab="Week",ylab="Net Worth", main="Net Worth"))
  #stationary test
library(tseries)
adf.test(Total.assets)
kpss.test(Total.assets)
adf.test(Total.assets, alternative = c("stationary","explosive"), k=trunc((length(Total.assets)-1)^(1/3)))
adf.test(Total.liabilities)
kpss.test(Total.liabilities)
adf.test(CR)
kpss.test(CR)
adf.test(DE)
kpss.test(DE)
adf.test(Bank.credit)
kpss.test(Bank.credit)
  #Exponential Smoothing Forecast
library("forecast")
Assetforecast <- HoltWinters(Total.assets, beta=FALSE, gamma=FALSE)
Assetforecast
plot(Assetforecast)
Assetforecast2 <- forecast(Total.assets,h=10)
Assetforecast2
plot(Assetforecast2)

Liabilityforecast <- HoltWinters(Total.liabilities, beta=FALSE, gamma=FALSE)
Liabilityforecast
plot(Liabilityforecast)
liabilityforecast2 <- forecast(Total.liabilities,h=10)
liabilityforecast2
plot(liabilityforecast2)

CRforecast <- HoltWinters(CR, beta=FALSE, gamma=FALSE)
CRforecast
plot(CRforecast)
CRforecast2 <- forecast(CR,h=10)
CRforecast2
plot(CRforecast2)

DEforecast <- HoltWinters(DE, beta=FALSE, gamma=FALSE)
DEforecast
plot(DEforecast)
DEforecast2 <- forecast(DE,h=10)
DEforecast2
plot(DEforecast2)

NWforecast <- HoltWinters(Bank.credit, beta=FALSE, gamma=FALSE)
NWforecast
plot(NWforecast)
NWforecast2 <- forecast(Bank.credit,h=10)
NWforecast2
plot(NWforecast2)
