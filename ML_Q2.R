
####Machine Learning Q2




############################################# Q1  C
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

str(predictors)

adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
##not A
str(training)
str(testing)


adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]



adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[trainIndex,]
str(training)
str(testing)
##not B

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[trainIndex,]
str(testing)


############################################# Q2   A?
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(Hmisc)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
#Make a plot of the outcome (CompressiveStrength) versus the index of the samples.
#Color by each of the variables in the data set (you may find the cut2() 
str(training) 
featurePlot(training[,c(seq(1,8))],training$CompressiveStrength)
                                                
?featurePlot                                                
training$id<-1:nrow(training)

training$F<-cut2(training$FlyAsh,g=2)
featurePlot(training$id,training$CompressiveStrength,col=as.factor(training$F))

                                                

training$A<-cut2(training$Age,g=3)

featurePlot(training$id,training$CompressiveStrength,col=as.factor(training$A))



library(AppliedPredictiveModeling)
library(ggplot2)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
str(mixtures)
id<-1:nrow(training)


library(Hmisc)
library(gridExtra)
fly<-cut2(training$FlyAsh,g=3)
qplot(id,training$CompressiveStrength,col=fly)

age<-cut2(training$Age,g=3)
qplot(id,training$CompressiveStrength,col=age)

age1<-cut2(training$CoarseAggregate,g=4)
qplot(id,training$CompressiveStrength,col=age1)








 ##########################     Q3            D                              
                                                
                                                
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

str(training)

par(mfrow=c(1,2))
hist(training$Superplasticizer)
hist(log(training$Superplasticizer+1))


##########################  Q4  c( 7 compoents)

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

a<-names(training)
sum(substr(a,1,2)=="IL")





IL<-training[,names(training)[substr(a,1,2)=="IL"]]
pc<-preProcess(IL,method="pca",thresh=0.9)
pc

##########################  Q5  
library(caret)
library(AppliedPredictiveModeling)
library(e1071)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

a<-names(training)
b<-a[substr(a,1,2)=="IL"]
train1<-training[,c(b,"diagnosis")]
str(train1)
test1<-testing[,c(b,"diagnosis")]

####one without pca

train1$diagnosis<-as.factor(ifelse(train1$diagnosis=="Impaired",1,0))
table(train1$diagnosis)
str(train1)

test1$diagnosis<-as.factor(ifelse(testing$diagnosis=="Impaired",1,0))
table(test1$diagnosis)
str(test1)


mod1<-train(y=train1$diagnosis, train1[,-13],method="glm")
mod1p<-predict(mod1,newdata=test1[,-13])
confusionMatrix(mod1p,test1$diagnosis)

                                                

####one with pca


train1$diagnosis<-as.factor(ifelse(train1$diagnosis=="Impaired",1,0))
table(train1$diagnosis)
str(train1)

test1$diagnosis<-as.factor(ifelse(testing$diagnosis=="Impaired",1,0))
table(test1$diagnosis)
str(test1)

trcontrol<-trainControl(preProcOptions=list(thresh=0.80))
mod2<-train(y=train1$diagnosis, train1[,-13],method="glm",preProcess="pca",trControl=trcontrol)


mod2p<-predict(mod2,newdata=test1[,-13])
confusionMatrix(mod2p,test1$diagnosis)                                                
                                                
                                                
                                                
 #############################################################################project                                               
                                                
                                                                                                                                  function in the Hmisc package useful for turning continuous covariates into factors). 
