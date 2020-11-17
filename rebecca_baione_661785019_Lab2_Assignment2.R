#Rebecca Baione
#Lab 2 Part 1: Measures of Central Tendency/Histogram/Data Manipulation

#Read in Data: 2010 EPI dataset|Test and fit the data
rm(list=ls())
setwd("C:/Users/baionr/Downloads")
EPI_data<-read.csv("EPI_data.csv") 
View("EPI_data")
attach(EPI_data)
head(EPI_data)
tail(EPI_data)
summary(EPI_data)

fix(EPI_data)
EPI
tf<-is.na(EPI)
E <- EPI[!tf] 
summary(EPI)

#Measures of Central Tendency: EPI & DALY
summary(EPI)
fivenum(EPI)
stem(EPI)
summary(DALY)
fivenum(DALY)
stem(DALY)

#Generate the Histogram: EPI & DALY
hist(EPI) 
hist(EPI,seq(30.,95.,1.0),prob=TRUE) 
lines(density(EPI,na.rm=TRUE,bw=1.))
lines(density(EPI,na.rm=TRUE,bw="SJ"))
rug(EPI)
hist(DALY)
hist(DALY,seq(0.,95.,1.0),prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY)

#Generate Boxplot: ENVHEALTH & ECOSYSTEM 
boxplot(ENVHEALTH,ECOSYSTEM)

#Generate Q-Q Plot: ENVHEALTH & ECOSYSTEM
plot(ecdf(ENVHEALTH),do.points=FALSE, verticals=TRUE)
qqnorm(ENVHEALTH)
qqplot(qt(ppoints(250),df=5),ENVHEALTH,xlab="Q-Q plot for ENVHEALTH")
qqline(ENVHEALTH)
plot(ecdf(ECOSYSTEM),do.points=FALSE,verticals=TRUE)
qqnorm(ECOSYSTEM)
qqplot(qt(ppoints(250),df=5),ECOSYSTEM,xlab="Q-Q plot for ECOSYSTEN")
qqline(ECOSYSTEM)
qqplot(ENVHEALTH,ECOSYSTEM)

#Least-Squares
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
dENVH<-coef(lmENVH)

#Predict
DALYNEW<-c(seq(5,95,5))
SIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<-predict(lmENVH,NEW,interval="prediction")
cENV<-predict(lmENVH,NEW,interval="confidence")

AIR_ENEW<-c(seq(5,95,5))
CLIMATENEW<-c(seq(5,95,5))
NEWER<-data.frame(AIR_ENEW,CLIMATENEW)
predict_ENV<-predict(lmENVH,NEWER,interval="prediction")
confidence_ENV<-predict(lmENVH,NEWER,interval="confidence")

#Lab 2 Part 2: EPI dataset
rm(list=ls())
setwd("C:/Users/baionr/Downloads")
Reg_data<-read.csv("dataset_multipleRegression.csv") 
View("Reg_data")
attach(Reg_data)
head(Reg_data)
tail(Reg_data)
summary(Reg_data)

fix(Reg_data)
tf<-is.na(Reg_data)
R <- Reg_data[!tf] 
summary(Reg_data)
head(Reg_data)

#Exercise 1: Regression
plot(UNEM~HGRAD)
mm<-lm(UNEM~HGRAD)
abline(mm)
abline(mm,col=3,lwd=3)
newdata<-data.frame(HGRAD=c(.07,90000))
nn<-lm(UNEM~HGRAD~INC)
newdata1<-data.frame(HGRAD=c(25000))
newdata<-data.frame(UNEM,HGRAD)
predict_ROLL<-predict(Reg_data,newdata1,interval="prediction")
newdata2<-data.frame(UNEM,HGRAD,INC)
predict_ROLL1<-predict(Reg_data,newdata2,interval="prediction")

#Exercise 2: Classification
rm(list=ls())
setwd("C:/Users/baionr/Downloads")
abalone_data<-read.csv("abalone.csv",header=FALSE,sep=",")
colnames(abalone_data)<-c("sex","length","diameter","height","whole_weight","shucked_weight","viscera_weight","shell_weight",'rings')
summary(abalone_data)
str(abalone_data)
summary(abalone_data$rings)
abalone_data$rings<-as.numeric(abalone_data$rings)
abalone_data$rings <-cut(abalone_data$rings, br=c(1,8,11,35), labels=c("young","adult","old"))
abalone_data$rings<-as.factor(abalone_data$rings)
summary(abalone_data$rings)
aba<-abalone_data
aba$sex<-NULL

normalize<-function(x){
  return<-((x-min(x))/max(x)-min(x)))
}
aba[1:7]<-as.data.frame(lapply(aba[1:7],normalize))
summary(aba$shucked_weight)
ind<-sample(2,nrow(aba),replace=TRUE,prob=c(0.7,0.3))
KNNtrain<-aba[ind==1]
KNNtest<-aba[ind==2]
sqrt(2918)
library(class)
KNNpred<-knn(train=KNNtrain[1:7],test=KNNtest[1:7],cl=KNNtrain$rings,k=55)
KNNpred
table(KNNpred)

#Exercise 3: Clustering
library(rpart)
library(rpart.plot)
iris
dim(iris)
sample_iris<-sample(150,1000) #creating the sample, k=1000 iterations
sample_iris

iris_train<-iris[sample_iris,]
iris_1<-iris[-sample_iris,]
dim(iris_1)
dtModel_iris<-rpart(Species~.,iris_train,method="class")
dtModel_iris
rpart.plot(dtMOdel_iris)

#Exercise 4: Dplyr
library(dplyr)
data<-data.frame(EPI,DALY)
sample_n(data,5)
sample_frac(data,.1)
new_decs_EPI<-arrange(data,desc(EPI))
new_decs_DALY<-arrange(data,desc(DALY))
double_EPI<-mutate(data,EPI=EPI*2)
double_DALY<-mutate(data,DALY=DALY*2)
summarise(data,EPI=mean(EPI,na.rm=TRUE))
summarise(data,DALY=mean(DALY,na.rm=TRUE))