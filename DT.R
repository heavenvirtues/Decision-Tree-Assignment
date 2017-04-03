setwd("D:/Deepti/R")
library("party")
library("caret")

data(iris)
iris = iris
head(iris)
??party
sum(is.na(iris))
sapply(iris,function(x)length(unique(x)))
class(iris)
nrow(iris)
dtindex = sample(1:nrow(iris),size=nrow(iris)*.7)
traindt = iris[dtindex,]
head(traindt)

testdt = iris[-dtindex,]

dt_tree = ctree(Species~.,traindt)
dt_tree
plot(dt_tree)

dt_predtest = predict(dt_tree,testdt[,-c(5)])
dt_predtest
plot(dt_predtest)
dtoutput = testdt
dtoutput$SpeciesResult = dt_predtest
dtoutput

dt_predtrain = predict(dt_tree,traindt)

#creating confusion matrix to check accuracy
testmatrix <- table(dt_predtest, testdt$Species)
confusionMatrix(testmatrix)
trainmatrix <- table(dt_predtrain, traindt$Species)
confusionMatrix(trainmatrix)
