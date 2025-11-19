####preprocessing
library(caret);library(kernlab);data(spam)
inTrain<-createDataPartition(y=spam$type,
                             p=0.75,list=F)
training<-spam[inTrain,]
testing<-spam[-inTrain,]
dim(training)

hist(training$capitalAve,main="",xlab="ave. capital run length")

####huge amount of variation in features
mean(training$capitalAve)
sd(training$capitalAve)

###standardization
trainCapAve<-training$capitalAve
trainCapAveS<-(trainCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)


testCapAve<-testing$capitalAve
testCapAveS<-(testCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

###standardiztion preprcess function
preObj<-preProcess(training[,-58],method=c("center","scale"))
trainCapAveS<-predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS<-predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

###preprocess argument
set.seed(32343)
modelFit<-train(type~.,data=training,
                preProcess=c("center","scale"),method="glm")
modelFit

preObj<-preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS<-predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2));hist(trainCapAveS);qqnorm(trainCapAveS)



###imputing data with k nearest neighbrhood
set.seed(13343)

training$capAve<-training$capitalAve
selectNA<-rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA]<-NA

##impute
preObj<-preProcess(training[,-58],method="knnImpute")
capAve<-predict(preObj,training[,-58]$capAve)

##standardize true value
capAveTruth<-training$capitalAve
capAveTruth<-(capAveTruth-mean(capAveTruth))/sd(capAveTruth)

##
quantile(capAve-capAveTruth)
quantile((capAve-capAveTruth)[selectNA])
quantile((capAve-capAveTruth)[!selectNA])


###covariate creation 
library(kernlab);data(spam)
spam$capitalAveSq<-spam$capitalAve^2


library(ISLR);library(caret);data(Wage);
inTrain<-createDataPartition(y=Wage$wage,
                             p=.7,list=F)
training<-Wage[inTrain,];testing<-Wage[-inTrain,]


####common covariate
table(training$jobclass)

dummies<-dummyVars(wage~jobclass,data=training)
head(predict(dummies,newdata=training))

##remove zero covariates
nsv<-nearZeroVar(training,saveMetrics=T)
nsv

##spline basis
library(splines)
bsBasis<-bs(training$age,df=3)
bsBasis

##fitting with spline
lm1<-lm(wage~bsBasis,data=training)
plot(training$age,training$wage,pch=19,cex=.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)

##spline on test sets
predict(bsBasis,age=testing$age)


###corelatedpredicted

library(kernlab);data(spam);library(caret)
inTrain<-createDataPartition(y=spam$type,
                             p=0.75,list=F)

trainning<-spam[inTrain,]
testing<-spam[-inTrain,]

M<-abs(cor(training[,-58]))
diag(M)<-0
which(M>0.8,arr.ind=T)

###
names(spam[c(34,32)])

plot(spam[,34],spam[,32])

##rotation of plot
X<-.71*training$sum415+.71*training$num857
Y<-.71*training$sum415-.71*training$num857
plot(X,Y)

##prinnciple component analysis
smallSpam<-spam[,c(34,32)]
prComp<-prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

prComp$rotation

##PCA oN SPAM
typeColor<-((spam$type=="spam")*1+1)
prComp<-prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

preProc<-preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC<-predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)


##preprocessing with PCA
preProc<-preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC<-predict(preProc,log10(training[,-58]+1))
modelFit<-train(training$type~.,method="glm",data=trainPC)


testPC<-predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))
##alternative
modelFit<-train(training$type~.,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))




###############regression models#############
library(caret);data(faithful);set.seed(333)
inTrain<-createDataPartition(y=faithful$waiting,
                             p=0.5,list=F)
trainFaith<-faithful[inTrain,];testFaith<-faithful[-inTrain]
head(trainFaith)

plot(trainFaith.waiting,trainFaith$eruption,pch=19,col="blue,"xlab="waiting",ylab="Duration")





