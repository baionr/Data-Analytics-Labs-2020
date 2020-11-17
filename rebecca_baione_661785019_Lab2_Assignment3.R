#Rebecca Baione
#Assignment 3

#Read/Fix Data: NYT3, NYT4, NYT5, NYT6, NYT7
rm(list=ls())
setwd("C:/Users/baionr/Downloads/dds_ch2_nyt")
nyt3_data<-read.csv("nyt3.csv") 
nyt4_data<-read.csv("nyt4.csv")
nyt5_data<-read.csv("nyt5.csv")
nyt6_data<-read.csv("nyt6.csv")
nyt7_data<-read.csv("nyt7.csv")

#NYT3
#1a
attach(nyt3_data)
head(nyt3_data)
tail(nyt3_data)
summary(nyt3_data)
fix(nyt3_data)
tf<-is.na(nyt3_data)
nyt3 <- nyt3_data[!tf] 
boxplot(Age,Impressions)
#1b
hist(Age)
hist(Age,seq(0.,110.,1.0),prob=TRUE)
lines(density(Age,na.rm=TRUE,bw="SJ"))
rug(Age)
hist(Impressions)
hist(Impressions,seq(0.,20.,1.0),prob=TRUE)
lines(density(Impressions,na.rm=TRUE,bw="SJ"))
rug(Impressions)
#1c
plot(ecdf(Age),do.points=FALSE, verticals=TRUE)
qqnorm(Age)
qqplot(qt(ppoints(250),df=5),Age,xlab="Q-Q plot for Age, NYT3")
qqline(Age)
plot(ecdf(Impressions),do.points=FALSE, verticals=TRUE)
qqnorm(Impressions)
qqplot(qt(ppoints(250),df=5),Impressions,xlab="Q-Q plot for Impressions NYT3")
qqline(Impressions)

#NYT4
#1a
attach(nyt4_data)
head(nyt4_data)
tail(nyt4_data)
summary(nyt4_data)
fix(nyt4_data)
tf<-is.na(nyt4_data)
nyt4 <- nyt4_data[!tf] 
boxplot(Age,Impressions)
#1b
hist(Age)
hist(Age,seq(0.,110.,1.0),prob=TRUE)
lines(density(Age,na.rm=TRUE,bw="SJ"))
rug(Age)
hist(Impressions)
hist(Impressions,seq(0.,20.,1.0),prob=TRUE)
lines(density(Impressions,na.rm=TRUE,bw="SJ"))
rug(Impressions)
#1c
plot(ecdf(Age),do.points=FALSE, verticals=TRUE)
qqnorm(Age)
qqplot(qt(ppoints(250),df=5),Age,xlab="Q-Q plot for Age, NYT4")
qqline(Age)
plot(ecdf(Impressions),do.points=FALSE, verticals=TRUE)
qqnorm(Impressions)
qqplot(qt(ppoints(250),df=5),Impressions,xlab="Q-Q plot for Impressions NYT4")
qqline(Impressions)

#NYT5
#1a
attach(nyt5_data)
head(nyt5_data)
tail(nyt5_data)
summary(nyt5_data)
fix(nyt5_data)
tf<-is.na(nyt5_data)
nyt5 <- nyt5_data[!tf] 
boxplot(Age,Impressions)
#1b
hist(Age)
hist(Age,seq(0.,110.,1.0),prob=TRUE)
lines(density(Age,na.rm=TRUE,bw="SJ"))
rug(Age)
hist(Impressions)
hist(Impressions,seq(0.,20.,1.0),prob=TRUE)
lines(density(Impressions,na.rm=TRUE,bw="SJ"))
rug(Impressions)
#1c
plot(ecdf(Age),do.points=FALSE, verticals=TRUE)
qqnorm(Age)
qqplot(qt(ppoints(250),df=5),Age,xlab="Q-Q plot for Age, NYT5")
qqline(Age)
plot(ecdf(Impressions),do.points=FALSE, verticals=TRUE)
qqnorm(Impressions)
qqplot(qt(ppoints(250),df=5),Impressions,xlab="Q-Q plot for Impressions NYT5")
qqline(Impressions)

#NYT6
#1a
attach(nyt6_data)
head(nyt6_data)
tail(nyt6_data)
summary(nyt6_data)
fix(nyt6_data)
tf<-is.na(nyt6_data)
nyt6 <- nyt6_data[!tf] 
boxplot(Age,Impressions)
#1b
hist(Age)
hist(Age,seq(0.,110.,1.0),prob=TRUE)
lines(density(Age,na.rm=TRUE,bw="SJ"))
rug(Age)
hist(Impressions)
hist(Impressions,seq(0.,20.,1.0),prob=TRUE)
lines(density(Impressions,na.rm=TRUE,bw="SJ"))
rug(Impressions)
#1c
plot(ecdf(Age),do.points=FALSE, verticals=TRUE)
qqnorm(Age)
qqplot(qt(ppoints(250),df=5),Age,xlab="Q-Q plot for Age, NYT6")
qqline(Age)
plot(ecdf(Impressions),do.points=FALSE, verticals=TRUE)
qqnorm(Impressions)
qqplot(qt(ppoints(250),df=5),Impressions,xlab="Q-Q plot for Impressions NYT6")
qqline(Impressions)

#NYT7
attach(nyt7_data)
head(nyt7_data)
tail(nyt7_data)
summary(nyt7_data)
fix(nyt7_data)
tf<-is.na(nyt7_data)
nyt7 <- nyt7_data[!tf] 
boxplot(Age,Impressions)
hist(Age)
hist(Age,seq(0.,120.,1.0),prob=TRUE)
lines(density(Age,na.rm=TRUE,bw="SJ"))
rug(Age)
hist(Impressions)
hist(Impressions,seq(0.,20.,1.0),prob=TRUE)
lines(density(Impressions,na.rm=TRUE,bw="SJ"))
rug(Impressions)
#1c
plot(ecdf(Age),do.points=FALSE, verticals=TRUE)
qqnorm(Age)
qqplot(qt(ppoints(250),df=5),Age,xlab="Q-Q plot for Age, NYT7")
qqline(Age)
plot(ecdf(Impressions),do.points=FALSE, verticals=TRUE)
qqnorm(Impressions)
qqplot(qt(ppoints(250),df=5),Impressions,xlab="Q-Q plot for Impressions NYT7")
qqline(Impressions)