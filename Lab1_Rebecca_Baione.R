#Rebecca Baione
#Lab 1
rm(list=ls())
setwd("C:/Users/baionr/Downloads")
install.packages("MASS")
library("MASS")
attach(Boston) #attach a data frame

#Return info about data frame
?Boston #word description of Boston
help(Boston)
head(Boston) #First 6 rows, gives dimension
dim(Boston)
names(Boston)
str(Boston)
nrow(Boston)
ncol(Boston)
summary(Boston) #Stats of Boston
summary(Boston$crim)
hist(Boston$crim)
boxplot(Boston$crim)
fivenum(Boston$crim)
median(Boston$crim)

install.packages("ISLR")
library("ISLR")
attach(Auto)
data(Auto)
head(Auto)
help(AUto)
head(Auto,10) #See first 10 rows instead of 6
summary(Auto)
names(Auto)
fivenum(Auto$mpg)
boxplot(Auto$weight)
hist(Auto$mpg)
mean(Auto$weight) #Find mean of the weight

help(read.csv)
data1<-read.csv(file.choose(), Header=TRUE)
