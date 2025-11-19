###############regression models#############
library(caret);data(faithful);set.seed(333)
inTrain<-createDataPartition(y=faithful$waiting,
                             p=0.5,list=F)
trainFaith<-faithful[inTrain,];testFaith<-faithful[-inTrain,]
head(trainFaith)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="waiting",ylab="Duration")

lm1<-lm(eruptions~waiting,data=trainFaith)
summary(lm1)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="waiting",ylab="Duration")

lines(trainFaith$waiting,lm1$fitted,lwd=3)

coef(lm1)[1]+coef(lm1)[2]*80
###another form of prediction
newdata<-data.frame(waiting=80)
predict(lm1,newdata)



##plot prediction
par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)


####get trainning set/test set errors
# Calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))

# Calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))


pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)

modFit<-train(eruptions~waiting,data=trainFaith,method="lm")
summary(modFit$finalModel)

########

library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage))
summary(Wage)


inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

###Feature Plot
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

##plot age versus wage
qplot(age,wage,data=training)

##plot age versus wage colored by jobclass
qplot(age,wage,colour=jobclass,data=training)
qplot(age,wage,colour=education,data=training)


modFit<- train(wage ~ age + jobclass + education,
               method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)

##diagnostic
plot(finMod,1,pch=19,cex=0.5,col="#00000010")

##color by variable not used in model
qplot(finMod$fitted,finMod$residuals,colour=race,data=training)

##plot by index
plot(finMod$residuals,pch=19)

##predicted versus truth in test set
pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)

###if you want to use all the covariants
modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)

