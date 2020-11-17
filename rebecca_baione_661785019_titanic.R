#Rebecca Baione
#Titanic
#rpart
install.packages("titanic")
library(titanic)
data("Titanic")
data("titanic_test")
data("titanic_train")
head(Titanic)
library(rpart)
library(rpart.plot)
Titanic1 <- rpart(Survived~., data=titanic_train, method='class')
Titanic2 <- rpart(Survived~ Pclass + Age, data=titanic_train)
rpart.plot(Titanic1) 
rpart.plot(Titanic2)
#attempt to create partition map
install.packages("plotmo")
library(plotmo)
plotmo(Titanic2, nresponse="Survived", degree1=1, degree2=0, trace=-1, do.par=FALSE)
plotmo(Titanic2, nresponse="Survive", degree1=0, type2="image", col.image=gray(seq(.6,1,.05), col.response=ifelse(Titanic2$Survived=="died", "red" , "green"), pch=ifelse(Titanic2$Survived=="died",15,16))
#accuracy defined by (TP + TN)/(TP + TN + FP + FT)
titanic_predict <- predict(Titanic1, titanic_test, type = 'class')
titanic_predict_table <- table(titanic_test$Survived, titanic_predict)
accuracy = sum(diag(titanic_predict_table))/sum(titanic_predict_table)
#ctree
install.packages("party")
library(party)
titanic_c <- ctree(Survived~Pclass + Age, data=titanic_train)
titanic_c
plot(titanic_c)
#hclust
t <- dist(as.matrix(Titanic))
t_hc <- hclust(t)
plot(t_hc)
titanic_test2 <- function(dim, num, seed=17) {
  set.seed(2)
  matrix(rnorm(dim * num), nrow=num)
}
tm <- titanic_test2(2,10)
#random Forest
install.packages("randomForest")
library(randomForest)
set.seed(500)
Titanic3 <- randomForest(as.factor(Survived) ~ Pclass + Age + Sex + Name + Ticket + Fare + SibSp + Parch + Embarked + PassengerId + Cabin, data=titanic_train, importance=TRUE, ntree=2000)
varImpPlot(Titanic3)
