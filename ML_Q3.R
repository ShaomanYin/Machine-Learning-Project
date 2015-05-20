
################################################### Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rattle)
library(rpart.plot)

a<-segmentationOriginal
names(a)
table(a$Case)

training<-a[a$Case=="Train",]
testing<-a[a$Case=="Test",]


set.seed(125)

help(segmentationOriginal)
table(a$Class)



x<-training[,-3]
y<-training$Class
mod<-train(y=y,x=x,method="rpart")
pred<-predict(mod,testing)
confusionMatrix(pred,testing$Class)

##plot
plot(mod$finalModel)
text(mod$finalModel)
fancyRpartPlot(mod$finalModel)

boxplot(a$TotalIntenCh2~a$Class)
summary(a$VarIntenCh4)



################################################### Q2
library(pgmm)
library(caret)
library(rpart)
data(olive)
names(olive)
olive = data.frame(olive[,-1])


set.seed(125)

table(olive$Area)
names(olive)


?createDataPartition


in1<-createDataPartition(y=olive$Area,p=0.75,list=F)


training<-olive[in1,]
testing<-olive[-in1,]


dim(training)
dim(testing)
names(training)

str(training)

x<-training[,-1]
y<-as.factor(training$Area)
y<-training$Area

trc<-trainControl(number=100)
mod<-train(y=y,x=x,method="rpart",tuneLength=10)
pred<-predict(mod,testing[,-1])
confusionMatrix(pred,as.factor(testing$Area))


newdata = as.data.frame(t(colMeans(olive)))
pred2<-predict(mod,newdata)


set.seed(125)
mod2<-train(y=y,x=x,method="rpart")
newdata = as.data.frame(t(colMeans(olive)))
pred2<-predict(mod2,newdata)

##A , should treat Areas as factor not numeric, strange



################################################### Q4

library(ElemStatLearn)
library(caret)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
str(trainSA)
?trainControl
y<-as.factor(trainSA$chd)
x<-trainSA[,c(2,3,6,7,8,9)]
names(x)

?glm
mod3<-glm(chd~tobacco+ldl+typea+obesity+alcohol+age,data=trainSA,family=binomial())
summary(mod3)



mod3<-train(y=y,x=x,method="glm")

##train
pred1<-predict(mod3,trainSA[,names(x)])
val1<-trainSA$chd
mean(pred1!=val1)
#0.2727273

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(val1,pred1)


##test
pred1<-predict(mod3,testSA[,names(x)])
val1<-testSA$chd
#misclassfication rate
mean(pred1!=val1)
#0.3116883

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(val1,pred1)
##A


################################################### Q5


library(ElemStatLearn)
library(randomForest)
data(vowel.train)
data(vowel.test) 

names(vowel.test)

vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

set.seed(33833)

?train
mod5<-train(y=vowel.train$y,x=vowel.train[,-1])
pred5<-predict(mod5,vowel.test[,-1])
confusionMatrix(pred5,vowel.test$y)
varImp(mod5)
#x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10









