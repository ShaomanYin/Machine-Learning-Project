




######################################

library(caret)
library(kernlab)
library(e1071)
library(rpart)
library(gbm)
library(plyr)
library(randomForest)
library(adabag)



##################################
setwd("C:/Users/T400/Desktop/DataSci/Machine Learning Projects/")
setwd("//cdc.gov/private/M327/wso3/Machine_Learning_R/")

start<-read.csv("pml-training.csv",na.strings=c("#DIV/0!"))
final<-read.csv("pml-testing.csv",na.strings=c("#DIV/0!"))
str(start)




##################################
#get numeric/inter var list
want<-numeric()
for (i in 1:ncol(start)) {
  if (class(start[,i])=="factor") {want[i]=0}
  else (want[i]=1)
}
numvar<-names(start[want==1])


##################################
##all nuermic vars is in test set
#want1<-colSums(is.na(final))==0
#testnum<-names(final)[want1]
#testnum %in% numvar
##################################
##get  initial predicators
prevars<-numvar[-c(1:4)]
modelset<-start[,c(prevars,"classe")]
table(modelset$classe)
##################################



#################################
##remove column with NA
a<-colSums(is.na(modelset))==0
modelset2<-modelset[,a]
#names(modelset2)




#################################
##remove nearZerovar
?nearZeroVar
b<-nearZeroVar(modelset2)
##no nearzerovars
#round(prop.table(table(modelset[,7]))*100,2)
##transform dummy
#fa<-dummyVars(~.,data=tr1[,facvar])
#tr1f<-predict(fa,tr1[,facvar])


#################################
##remove high correlated
names(modelset2)
cor<-cor(modelset2[,-53])
higcor<-findCorrelation(cor,0.9)
modelset3<-modelset2[,-higcor]

names(modelset3)
cor2<-cor(modelset3[,-46])
higcor2<-findCorrelation(cor2,0.9)

##################################




##################################
##get final initial predicators

names(modelset3)

for (i in 1:ncol(modelset3[,-46])) {
  modelset3[,i]<-as.numeric(modelset3[,i])
}

str(modelset3)

intrain<-createDataPartition(y=modelset3$classe,p=0.75,list=F)
training<-modelset[intrain,]
testing<-modelset[-intrain,]

dim(training)
dim(testing)
prop.table(table(training$classe))
prop.table(table(testing$classe))
##################################

#############################################################################  1
##################################
##################################                         rpart
##train model use rpart tree
set.seed(1234)

?trainControl
tr<-trainControl(method="cv",)
mod1<-train(y=as.factor(training$classe),training[,prevars],method="rpart",trControl=tr,tuneLength=10)
pred1<-predict(mod1,testing[,prevars])
confusionMatrix(pred1,testing$classe)
plot(mod1)
##0.67 accuracy
##################################
names(modelset3)


#############################################################################  2
##################################
##################################                         adabag
##train model use rpart tree
set.seed(1234)

str(modelset3)
attach(training)

##boosting 
mod2<-boosting(classe ~.,data=training,mfinal=100,control=rpart.control(maxdepth=15))
confusionMatrix(mod2$class,training$classe)

pred2<-predict.boosting(mod2,testing)
confusionMatrix(pred2$confusion)
##0.998 accuracy (but hard to interpret)



##boosting (notwork)
mod2<-boosting(classe ~.,data=training,mfinal=1000,control=rpart.control(maxdepth=1,cp=-1))
confusionMatrix(mod2$class,training$classe)

pred2<-predict.boosting(mod2,testing)
confusionMatrix(pred2$confusion)
##0.998 accuracy (but hard to interpret)


summary(mod2$imp)
barplot(mod2$imp[order(mod2$imp,decreasing=T)],ylim=c(0,30),main="Variable Relative Importance",col="lightblue")
im<-mod2$imp[mod2$imp>0.5]
par(mar=c(5,9,4,2))
barplot(im[order(im,decreasing=T)],xlim=c(0,25),main="Variable Relative Importance",col="lightblue",
        horiz=T,las=1,cex.names=1)



##margins plot
mar0<-margins(mod2,training)[[1]]
mar<-margins(pred2,testing)[[1]]
plot(sort(mar0),(1:length(mar0)/length(mar0)),type="l",xlim=c(-1,1),main="Margin cummulative distribution graph",
     xlab="m",ylab="% observations",lty=1,lwd=2,col="blue")
abline(v=0,col="red",lty=2,lwd=2)
lines(sort(mar),(1:length(mar)/length(mar)),type="l",col="green",lty=2,lwd=2)
legend("topleft",c("train","test"),col=c("blue","green"),lty=1,lwd=2)

##erro rate
err0<-errorevol(mod2,training)$error
err1<-errorevol(mod2,testing)$error

plot((1:length(err0)),err0,type="l",ylim=c(0,0.4),main="Boosting error vs. number of trees",xlab="Iterations",ylab="Error",
     col="red",lwd=2)
lines(err1,cex=0.5,col="blue",lty=2,lwd=2)
lines(v=min(err0))
abline(h=min(err0),col="red",lty=2,lwd=2)
abline(h=min(err1),col="blue",lty=2,lwd=2)




##bagging (not good,long time)
mod22<-bagging(classe ~.,data=training,mfinal=100,control=rpart.control(maxdepth=10) )
confusionMatrix(mod22$class,training$classe)
pred22<-predict.bagging(mod22,testing)
confusionMatrix(pred22$confusion)
##0.71 accuracy




##################################













##################################
##train model use gbm
tr<-trainControl(method="cv",)
mod2<-train(y=as.factor(training$classe),training[,prevars],method="gbm",trControl=tr)
pred1<-predict(mod1,testing[,prevars])
confusionMatrix(pred1,testing$classe)
plot(mod1)




?boosting





















