###quiz 3 question 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
names(segmentationOriginal)
set.seed(125)
# inTrain <- createDataPartition(y=segmentationOriginal$Class,segmentationOriginal$Case == "Train", list=FALSE)
training<-segmentationOriginal[which(segmentationOriginal$Case == "Train"),]
testing<-segmentationOriginal[which(segmentationOriginal$Case == "Test"),]
dim(training); dim(testing)
library(caret)
modFit <- train(Class ~., method="rpart", data=training)
print(modFit$finalModel)
TotalIntenCh2 = 57000; FiberWidthCh1 = 8; PerimStatusCh1=100 

testing$TotalIntenCh2<-TotalIntenCh2
testing$FiberWidthCh1<-FiberWidthCh1
testing$PerimStatusCh1<-PerimStatusCh1

# segmentationOriginal$TotalIntenCh2
predict(modFit,testing)

testing<-segmentationOriginal[which(segmentationOriginal$Case == "Test"),]
FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
testing<-segmentationOriginal[which(segmentationOriginal$Case == "Test"),]
testing$VarIntenCh4<-VarIntenCh4
testing$FiberWidthCh1<-FiberWidthCh1
testing$PerimStatusCh1<-PerimStatusCh1
predict(modFit,testing)



##question 2
library(ggplot2);library(caret);
library(pgmm)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))

inTrain <- createDataPartition(y=olive$Area,p=1, list=FALSE)
training <- olive[inTrain,]
testing <- olive[-inTrain,]
dim(training); dim(testing)

modFit <- train(Area ~ .,method="rpart",data=training)
print(modFit$finalModel)

predict(modFit,newdata)

plot(modFit$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

###question 4

library(ggplot2);library(caret);
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
modFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method="glm",data=trainSA,family="binomial")
values<-testSA$chd

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA$chd,predict(modFit,testSA))
missClass(trainSA$chd,predict(modFit,trainSA))

##question 5
library(ggplot2);library(caret);
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
M <- train(y ~ ., data=vowel.train, method="rf")
varImp(M)
fit2 <- train(y~ .,data=vowel.train,method="rf")
varImp(fit2)
pred <- predict(modFit,vowel.test); vowel.test$predRight <- pred==vowel.test$y
table(pred,vowel.test$y)
?varImp
