########################################################
#과제 comment
#######################################################


# 1. #BUYTEST 데이터를 사용한 의사결정나무분석 
# Data : BUYTEST
library(tree)
library(rpart)
library(rpart.plot)

buytest=read.csv(choose.files(),header = T)
str(hmeq)
dim(hmeq)
buytest$RESPOND=as.factor(buytest$RESPOND)
buytest$MARRIED=as.factor(buytest$MARRIED)
buytest$SEX=as.factor(buytest$SEX)
buytest$COA6=as.factor(buytest$COA6)
buytest$OWNHOME=as.factor(buytest$OWNHOME)
buytest$LOC=as.factor(buytest$LOC)
buytest$CLIMATE=as.factor(buytest$CLIMATE)
buytest$ORGSRC=as.factor(buytest$ORGSRC)
buytest$DISCBUY=as.factor(buytest$DISCBUY)
buytest$RETURN24=as.factor(buytest$RETURN24)

buytest1=buytest[,2:18]

set.seed(123456) 
ind <- sample(2, nrow(buytest1), replace=T, prob=c(0.7, 0.3))
trainData <- buytest1[ind==1,]
testData <- buytest1[ind==2,]



buytest_train <- tree(RESPOND~., data=trainData)

summary(buytest_train)
buytest_train
win.graph()
plot(buytest_train)
text(buytest_train)


buytest_train <- rpart(RESPOND~., data=trainData)

buytest_train
summary(buytest_train)
rpart.plot(buytest_train, type=0, extra=4)



buytest_y=buytest1[buytest1$RESPOND==1,]
buytest_n=buytest1[buytest1$RESPOND==0,]
dim(buytest_y);dim(buytest_n)

my_index=sample(1:nrow(buytest_n),size=nrow(buytest_y))
buytest_nn=buytest_n[my_index,]
buytest_small=rbind(buytest_y,buytest_nn)
dim(buytest_small)

# Decision Tree with Full data set
win.graph()
buytest_train <- rpart(RESPOND~., data=trainData)
summary(buytest_train)
rpart.plot(buytest_train, type=0, extra=4)

# Decision Tree with 1:1 balanced data set
win.graph()
result2_s <- rpart(RESPOND~., data=buytest_small)

result2_s
summary(result2_s)
rpart.plot(result2_s, type=0, extra=4)



##################################
#평점표 만들기
##################################
hmeq=read.csv(choose.files(),header = T)
hmeq$BAD=as.factor(hmeq$BAD)
str(hmeq)

hmeq_out <- tree(BAD~., data=hmeq)

summary(hmeq_out)
hmeq_out
plot(hmeq_out)
text(hmeq_out)

hmeq_s=hmeq[,c(1,4,8,9,10,13)]

#교재 151쪽과 같이 그룹화

hmeq_s <- transform(hmeq_s, grp_DEBTINC = 
              ifelse(DEBTINC < 44, 2, 3))
hmeq_s$grp_DEBTINC[is.na(hmeq$DEBTINC)]=1
table(hmeq_s$grp_DEBTINC)


hmeq_s <- transform(hmeq_s, grp_DELINQ = 
                      ifelse(DELINQ == 0, 1,
                      ifelse(DELINQ < 2, 2,  3)))
hmeq_s$grp_DELINQ[is.na(hmeq$DELINQ)]=1
table(hmeq_s$grp_DELINQ)


hmeq_s <- transform(hmeq_s, grp_CLAGE = 
                      ifelse(CLAGE < 150,  1,
                      ifelse(CLAGE <= 240, 2,  3)))
hmeq_s$grp_CLAGE[is.na(hmeq$CLAGE)]=1
table(hmeq_s$grp_CLAGE)


hmeq_s <- transform(hmeq_s, grp_VALUE = 
                      ifelse(VALUE < 47500,  2,
                      ifelse(VALUE <= 200000, 3,  4)))
hmeq_s$grp_VALUE[is.na(hmeq$VALUE)]=1
table(hmeq_s$grp_VALUE)


hmeq_s <- transform(hmeq_s, grp_DEROG = 
                      ifelse(DEROG == 0,  2, 3))
hmeq_s$grp_DEROG[is.na(hmeq$DEROG)]=1
                            
table(hmeq_s$grp_DEROG)



str(hmeq_s)
hmeq_s$grp_DEBTINC=as.factor(hmeq_s$grp_DEBTINC)
hmeq_s$grp_DELINQ=as.factor(hmeq_s$grp_DELINQ)
hmeq_s$grp_CLAGE=as.factor(hmeq_s$grp_CLAGE)
hmeq_s$grp_VALUE=as.factor(hmeq_s$grp_VALUE)
hmeq_s$grp_DEROG=as.factor(hmeq_s$grp_DEROG)

out=glm(BAD~grp_DEBTINC+grp_DELINQ+grp_CLAGE+grp_VALUE+grp_DEROG, 
        family=binomial, data=hmeq_s)
summary(out)
