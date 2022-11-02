# Chaper 5 Lab: Cross-Validation 
install.packages("ISLR")
install.packages("Metrics")
library(ISLR)
library(Metrics)

str(Auto)
summary(Auto)
# The Validation Set Approach

set.seed(1)
train=sample(392,196)

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
summary(lm.fit)
attach(Auto)
predicted<-predict(lm.fit,Auto)[-train]
mse(predicted,mpg[-train])

##2os tropos orizo egw to MSE
## mean((mpg-predict(lm.fit,Auto))[-train]^2)




lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
summary(lm.fit)
predicted2<-predict(lm.fit2,Auto)[-train]
mse(predicted2,mpg[-train])


##2os tropos orizo egw to MSE
##mean((mpg-predict(lm.fit2,Auto))[-train]^2)


lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
summary(lm.fit3)
predicted3<-predict(lm.fit3,Auto)[-train]
mse(predicted3,mpg[-train])


##2os tropos orizo egw to MSE
##mean((mpg-predict(lm.fit3,Auto))[-train]^2)


##allagi training set

set.seed(2)
train=sample(392,196)

lm.fit=lm(mpg~horsepower,subset=train)
summary(lm.fit)
predicted<-predict(lm.fit,Auto)[-train]
mse(predicted,mpg[-train])

##2os tropos orizo egw to MSE
##mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
summary(lm.fit)
predicted2<-predict(lm.fit2,Auto)[-train]
mse(predicted2,mpg[-train])

##2os tropos orizo egw to MSE
##mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
summary(lm.fit3)
predicted3<-predict(lm.fit3,Auto)[-train]
mse(predicted3,mpg[-train])

##2os tropos orizo egw to MSE
##mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# Leave-One-Out Cross-Validation

library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
summary(glm.fit)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

# k-Fold Cross-Validation

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

#################### Neuronika diktia###############################################
##library(neuralnet)


##load the dataset and split into training and test sets
#data(iris)
ind=sample(2,nrow(iris), replace=T, prob=c(0.7,0.3))
trainset=iris[ind==1,]
testset=iris[ind==2,]
set.seed(1)

##load library neuralnet
library(neuralnet)

##add the columns versicolor,setosa and virginica based on the names in Species variable
trainset$setosa=trainset$Species=="setosa"
trainset$virginica=trainset$Species=="virginica"
trainset$versicolor=trainset$Species=="versicolor"

##train the NN with neuralnet function with 3 hidden neurons 
##in each layer.use set.seed to have the same results in every training process

network= neuralnet(versicolor+virginica+setosa~
                     Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                   data=trainset, hidden=3)

##visualize the trained nn with plot
plot(network)

##predicting labels based on a model trained by neuralnet

##generate prediction probability matrix based on a trained nnand the testing
##dataset, testset

net.predict=compute(network,testset[-5])$net.result

##obtain the possible labels by finding the column with the 
##gratest probability:

net.prediction=c("versicolor","virginica", "setosa")[apply(net.predict,1,which.max)]

##generate a classification table based on the predicted labels
##and the labels of the testing dataset:

predict.table=table(testset$Species,net.prediction)
predict.table

##finally use confusionmatrix
library(caret)
confusionMatrix(predict.table)

