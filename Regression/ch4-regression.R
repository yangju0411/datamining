##############################################
#Ch4 회귀분석 
##############################################

##############################################
# Fitness Data in SAS Help Examples          #
##############################################
fitness=read.csv(choose.files(), header=T)
str(fitness)
win.graph() 
pairs(fitness)
plot(Oxygen~., data=fitness)
cor(fitness)
fit=lm(Oxygen~., data=fitness)
summary(fit)
namess(fit)
par(mfrow=c(2,2))
plot(fit)
fit.forward=step(fit, direction = "forward")
fit.backward=step(fit, direction = "backward")
fit.step=step(fit, direction = "both")
fit.step$anova


##############################################
# Housing Data Textbook Ex3.6                #
##############################################
housing=read.csv(choose.files(), header=T)
str(housing)
housing$CHAS=as.factor(housing$CHAS)

set.seed(1234567) 
ind <- sample(2, nrow(housing), replace=TRUE, prob=c(0.7, 0.3))
trainData <- housing[ind==1,]
testData <- housing[ind==2,]


win.graph()
pairs(trainData[,-4])  # CHAS 제외
plot(MEDV~., data=trainData)
cor(trainData$MEDV, trainData[, c(1:3,5:13)])
fit=lm(MEDV~., data=trainData)
fit
names(fit)
fit$coefficients
fit$residuals
summary(fit)

par(mfrow=c(1,2))
plot(fit)
#direction "forward", "backward", "both"
fit.step=step(fit, direction = "both")
names(fit.step)
fit.step$anova
summary(fit.step)

pred_train=predict(fit, trainData)
pred_test =predict(fit, newdata=testData)

plot(trainData$MEDV,pred_train, xlim=c(0, 50), ylim=c(0,50))
abline(a=0, b=1)
plot(testData$MEDV,pred_test, xlim=c(0, 50), ylim=c(0,50))
abline(a=0, b=1)


###################################
# Ch4 Logistic Regression
###################################
hmeq=read.csv(choose.files(),header = T)
str(hmeq)
attach(hmeq)

set.seed(1234) 
ind <- sample(2, nrow(hmeq), replace=TRUE, prob=c(0.7, 0.3))
trainData <- hmeq[ind==1,];dim(trainData)
testData <- hmeq[ind==2,] ;dim(testData)

out=glm(BAD~., family=binomial, data=trainData)
summary(out)
p=predict(out,trainData, type="response")

table(y,p>0.1)



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

set.seed(1234) 
ind <- sample(2, nrow(buytest1), replace=T, prob=c(0.7, 0.3))
trainData <- buytest1[ind==1,]
testData <- buytest1[ind==2,]

out_r=glm(RESPOND~AGE+CLIMATE, family=binomial, data=trainData)
summary(out_r)
names(out_r)
1/exp(out_r$coefficients)


mine=RESPOND~AGE+INCOME+SEX+MARRIED+FICO+OWNHOME+LOC+CLIMATE+BUY6+BUY12+BUY18+VALUE24+
             ORGSRC+DISCBUY+RETURN24+COA6
out_f=glm(mine, family=binomial, data=trainData)
summary(out_f)
out_step=step(out_f, direction="both")
summary(out_step)
exp(out_step$coefficients)

p_train=predict(out_step,trainData, type="response")
p_test=predict(out_step, newdata=testData, type="response")

table(trainData$RESPOND,p_train>0.5)
table(trainData$RESPOND,p_train>0.1)
table(testData$RESPOND,p_test>0.1)
