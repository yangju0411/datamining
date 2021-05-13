#########################################################
#Ref: R 을 이용한 데이터마이닝(자유아카데미)
# ch3 의사결정나무
########################################################

### iris 데이터를 사용한 예제 ###
library(MASS)
library(tree)
data(iris)
str(iris)
win.graph()
plot(iris[,1:4], col=iris$Species)

ir.tr=tree(Species~., data=iris)
#ir.tr=tree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris)
summary(ir.tr)
x11()
ir.tr
plot(ir.tr)
text(ir.tr, all=T)

#분류표 만들기
predict(ir.tr)
pred=predict(ir.tr, iris, type="class")
table(pred, iris$Species)
library(caret)
confusionMatrix(pred,iris$Species )



ir.tr1=snip.tree(ir.tr, nodes=c(12,7))  #마디 7과 12제거
ir.tr1
x11()
plot(ir.tr1)
text(ir.tr1, all=T)

par(pty='s')
plot(iris[,3],iris[,4], type="n",
  xlab="petal length", ylab="petal width")
text(iris[,3], iris[,4], c("s", "c", "v")[iris[,5]])
partition.tree(ir.tr1, add=T, cex=1.5)

ir.tr2=prune.misclass(ir.tr)
plot(ir.tr2) #적정노드 수의 산정
fin.tr=prune.misclass(ir.tr, best=4)#끝마디의 수는 4
plot(fin.tr)
text(fin.tr, all=T)
ir.tr2

# R package party를 사용한 decision tree
library(party)
iris_ctree <- ctree(Species ~ ., data=iris)
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")

#####################################################################
### Data partition기: 데이터분할
# train:test data 7:3로 분할

set.seed(1234) 
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]


myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_train <- tree(myFormula, data=trainData)
pred_train=predict(iris_train, type="class")
table(pred_train, trainData$Species)


plot(iris_train)
text(iris_train)


# predict on test data
testPred <- predict(iris_train, newdata = testData, type="class")
table(testPred, testData$Species)



################################
## Data : HMEQ (Textbook p328)
library(tree)
library(rpart)
library(rpart.plot)

hmeq=read.csv(choose.files(),header = T)
str(hmeq)
dim(hmeq)
attach(hmeq)
hmeq$BAD=as.factor(hmeq$BAD)
by(hmeq, BAD, summary)

win.graph()
boxplot(hmeq$DEBTINC~hmeq$BAD, col="Blue")
barplot(table(hmeq$BAD, hmeq$REASON))

set.seed(1234) 
ind <- sample(2, nrow(hmeq), replace=TRUE, prob=c(0.7, 0.3))
trainData <- hmeq[ind==1,];dim(trainData)
testData <- hmeq[ind==2,] ;dim(testData)

##### package "tree"
hmeq_train <- tree(BAD~., data=trainData)

summary(hmeq_train)
hmeq_train
plot(hmeq_train)
text(hmeq_train)

pred_train=predict(hmeq_train, type="class")
table(pred_train, trainData$BAD)


hmeq.train1=prune.misclass(hmeq_train)
win.graph()
plot(hmeq.train1) #적정노드 수의 산정
fin.train=prune.misclass(hmeq_train, best=6)#끝마디의 수는 6
plot(fin.train)
text(fin.train, all=T)
summary(fin.train)
fin.train


# predict on test data
testPred <- predict(hmeq_train, newdata = testData, type="class")
table(testPred, testData$BAD)

## package "rpart"
hmeq_train2 <- rpart(BAD~., data=trainData)

hmeq_train2
summary(hmeq_train2)

plot(hmeq_train2)
text(hmeq_train2, use.n=T)
rpart.plot(hmeq_train2, type=0, extra=4)

pred_train2=predict(hmeq_train2, type="class")
table(pred_train2, trainData$BAD)


# predict on test data
testPred <- predict(hmeq_train2, newdata = testData, type="class")
table(testPred, testData$BAD)



###################################################
#   회귀나무 (Regression Tree)
###################################################
data("bodyfat", package = "TH.data")
####bodyfat TH.data
# age age in years.
# DEXfat body fat measured by DXA, response variable.
# waistcirc waist circumference.
# hipcirc hip circumference.
# elbowbreadth breadth of the elbow.
# kneebreadth breadth of the knee.
# anthro3a sum of logarithm of three anthropometric measurements.
# anthro3b sum of logarithm of three anthropometric measurements.
# anthro3c sum of logarithm of three anthropometric measurements.
# anthro4  sum of logarithm of three anthropometric measurements.

dim(bodyfat)
str(bodyfat)
bodyfat[1:5,]


set.seed(1234) 
ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]
# train a decision tree
library(rpart)
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart <- rpart(myFormula, data = bodyfat.train, 
                       control = rpart.control(minsplit = 10))#최소 노드수: 10
attributes(bodyfat_rpart)
print(bodyfat_rpart$cptable)
print(bodyfat_rpart)

x11()
plot(bodyfat_rpart)
text(bodyfat_rpart, use.n=T)


DEXfat_pred <- predict(bodyfat_rpart, newdata=bodyfat.test)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data=bodyfat.test, xlab="Observed", 
     ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)

###########################################################
#회귀나무 textbook103, 330 WAGES
wages=read.csv(choose.files(), header=T)
dim(wages)
str(wages)
wages[,2]=as.factor(wages[,2])
wages[,3]=as.factor(wages[,3])
wages[,4]=as.factor(wages[,4])
wages[,5]=as.factor(wages[,5])
wages[,6]=as.factor(wages[,6])
wages[,7]=as.factor(wages[,7])
wages[,10]=as.factor(wages[,10])
wages[,13]=as.factor(wages[,13])
wages[,14]=as.factor(wages[,14])
wages[,15]=as.factor(wages[,15])
wages[,16]=as.factor(wages[,16])
wages[,17]=as.factor(wages[,17])
wages[,18]=as.factor(wages[,18])
wages[,19]=as.factor(wages[,19])
wages[,20]=as.factor(wages[,20])

set.seed(1234) 
ind <- sample(2, nrow(wages), replace=TRUE, prob=c(0.7, 0.3))
wages.train <- wages[ind==1,]
wages.test <- wages[ind==2,]
# train a decision tree
library(rpart)

wages_rpart <- rpart(lnwage~., data = wages.train, 
                       control = rpart.control(minsplit = 10))#최소 노드수: 10
attributes(wages_rpart)
print(wages_rpart$cptable)
print(wages_rpart)

win.graph()
plot(wages_rpart)
text(wages_rpart, use.n=T)
rpart.plot(wages_rpart)

wages_pred <- predict(wages_rpart, newdata=wages.test)
xlim <- range(wages$lnwage)
plot(wages_pred ~ lnwage, data=wages.test, xlab="Observed", 
     ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)
