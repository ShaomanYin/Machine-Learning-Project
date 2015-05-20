

path<-"C:/Users/T400/Desktop/DataSci/Machine Learning Projects/"
setwd(path)



d<-read.csv("pml-training.csv")
str(d)
names(d)


summary(d$classe)
str(d$classe)
length(d$classe)


######################################class note

library(caret)
library(kernlab)
library(e1071)






setwd("C:/Users/T400/Desktop/DataSci/Machine Learning Projects/")
tr<-read.csv("pml-training.csv")

str(training)
?nearZeroVar


#get numeric var list
want<-numeric()
for (i in 1:ncol(tr)) {
  if (class(tr[,i])=="factor") {want[i]=0}
  else (want[i]=1)
}


numvar<-names(tr[want==1])
facvar<-names(tr[want==0])

a<-nearZeroVar(tr[,numvar])
numvar[39]
summary(tr[,numvar[114]])
head(tr[,numvar[114]],200)

numvar1<-numvar[-a]
length(a)
length(numvar1)
length(numvar)

##remove nearZerovar
need<-c(facvar,numvar1)

tr1<-tr[,need]
names(tr1)
mod1<-train(y=tr1$classe,tr1[,-37],method="gbm")

























str(spam)

intrain<-createDataPartition(y=spam$type,p=0.75,list=F)
training<-spam[intrain,]
testing<-spam[-intrain,]

dim(training)


seed(32343)
mod<-train(type~.,data=training,method="glm")
mod
summary(mod)

predication<-predict(mod,newdata=testing)
summary(predication)

confusionMatrix(predication,testing$type)


###data slicing
set.seed(32323)
folds<-createFolds(y=spam$type,k=10,list=T,returnTrain=T)
class(folds)
sapply(folds,length)
summary(folds[[1]])

folds2<-createResample(y=spam$type,time=10,list=T)
sapply(folds2,length)

time<-1:1000
folds3<-createTimeSlices(y=time,initialWindow=20,horizon=10)
class(folds3)


?trainControl
?train

####plot predicator
library(ISLR)
library(ggplot2)
data(Wage)
str(Wage)


intrain<-createDataPartition(y=Wage$wage,p=0.70,list=F)
training<-Wage[intrain,]
testing<-Wage[-intrain,]
dim(training)
featurePlot(x=training[,c(seq(1,11))],y=training$wage,plot="pairs")
qplot(age,wage,color=jobclass,data=training)

qq<-qplot(age,wage,color=education,data=training)
qq+geom_smooth(method="lm",formula=y~x)

library(Hmisc)
library(gridExtra)
cutwage<-cut2(training$wage,g=3)
table(cutwage)

p1<-qplot(cutwage,age,data=training,fill=cutwage,geom="boxplot")
p1
p2<-qplot(cutwage,age,data=training,fill=cutwage,geom=c("boxplot","jitter"))
p2
grid.arrange(p1,p2,ncol=2)

t1<-table(cutwage,training$education)
prop.table(t1,1)

qplot(wage,color=jobclass,data=training,geom="density")


(spam)

intrain<-createDataPartition(y=spam$type,p=0.75,list=F)
training<-spam[intrain,]
testing<-spam[-intrain,]
?preProcess
pre<-preProcess(training[,-58])
mod<-train(type~.,data=training,preProcess=c("center","scale"),method="glm")
mod



library(AppliedPredictiveModeling)
library(caret)
a<-data(AlzheimerDisease)
str(a)
str(predictors)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

str(training)
str(testing)





library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
str(mixtures)



library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
str(mixtures)
summary(mixtures$Superplasticizer)

cor(mixtures)











