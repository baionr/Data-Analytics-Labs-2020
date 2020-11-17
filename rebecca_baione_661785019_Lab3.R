#Rebecca Baione 
#Lab3
#RPART
#Packages
library(rpart)
library(rpart.plot)
data("msleep")
str(msleep)
str(data)
#create a new data fram with select columns 
mSleepDF1 <- msleep[,c(3,6,10,11)] 
str(mSleepDF1)
head(mSleepDF1)
sleepModel_1 <- rpart(sleep_total~., data=mSleepDF1, method="anova")
sleepModel_1

rpart.plot9sleepModel_1, type-3, fallen.leaves=TRUE)
rpart.plot(sleepModel_1, type = 3, digits=3, fallen.leaves=TRUE)
rpart.plot(sleepModel_1, type=3, digits=4, fallen.leaves=TRUE)
#CPART
#packages
install.packages("C50")
require(C50)
data("iris")
head(iris)
str(iris)
table(iris$Species)
set.seed(9850)
grn <-runif(nrow(iris))
irisrand<-riris[order(grn)]
str(irisrand)
classification1 <- C5.0(irisrand[1:100,-5],irisrand[1:100,5])
classification1
summary(classification1)
prediction1<- predict(classification1,irisrand[101:150,])
prediction1 
summary(prediction1)
table(irisrand[101:150,5], prediction1)
plot(classification1)
