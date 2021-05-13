buytest=read.csv(choose.files(), header=T)
str(buytest)
dim(buytest)


buytest$RESPOND=as.factor(buytest$RESPOND)
buytest$MARRIED=as.factor(buytest$MARRIED)
buytest$COA6=as.factor(buytest$COA6)
buytest$OWNHOME=as.factor(buytest$OWNHOME)
buytest$CLIMATE=as.factor(buytest$CLIMATE)
buytest$DISCBUY=as.factor(buytest$DISCBUY)

buytest1=buytest[,1:18]

set.seed(123456) 
ind <- sample(2, nrow(buytest1), replace=T, prob=c(0.7, 0.3))
trainData <- buytest1[ind==1,]
testData <- buytest1[ind==2,]


##### R for Neural Network#############
library(nnet)
# nnet의  옵션
# size : hidden node 수 
# maxit : 반복횟수

nn_train <- nnet(RESPOND ~ AGE+INCOME , data=trainData, size=4)
summary(nn_train)

library(devtools)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')

win.graph()
plot.nnet(nn_train)
p_nn1<-predict(nn_train, trainData)
confusion.matrix=table(trainData$RESPOND, p_nn1>0.1)
round(prop.table(confusion.matrix) * 100, digit = 1)
p_nn2<-predict(nn_train, newdata=testData)
table(testData$RESPOND, p_nn2>0.1)

