setwd(file.path("D:", "sbu", "Rlearning",
                "practical machine learning", "finalproject"))
##ISLR: library of datasets
library(ISLR); library(ggplot2); 
library(caret);library(ElemStatLearn)
library(rpart)
set.seed(123)
##Reading Dataset
data <- read.csv("pml-training.csv", na.strings=c("#DIV/0!")) 

## Data set cleaning procedure starts from here
CleanedData <- data
for(i in c(8:ncol(CleanedData)-1)) {CleanedData[,i] = as.numeric(as.character(CleanedData[,i]))}

## removing all features with not assigned values
featuresnames <- colnames(CleanedData[colSums(is.na(CleanedData)) == 0])[-(1:7)]
features <- CleanedData[featuresnames]

## Data Features Partitioning
TrainData <- createDataPartition(y=features$classe, p=3/4, list=FALSE )
training <- features[TrainData,]
testing <- features[-TrainData,]
rm(training)
rm(testing)

library(dplyr)

By_classe<-group_by(features,classe)
classesum<-summarize(By_classe,Totalclass=sum(classe))
write.table(classesum, file = "classesum.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")


hist(features$classe, col = "red")

############## =========== neuralnet model
temp<-features
temp$classe<-as.character(temp$classe)
temp$classe[temp$classe=="A"]=1
temp$classe[temp$classe=="B"]=2
temp$classe[temp$classe=="C"]=3
temp$classe[temp$classe=="D"]=4
temp$classe[temp$classe=="E"]=5
temp$classe<-as.numeric(temp$classe)
Traintemp <- createDataPartition(y=temp$classe, p=3/4, list=FALSE )
trainingtemp <- temp[Traintemp,]
testingtemp <- temp[-Traintemp,]

library(neuralnet)
model1 <- neuralnet(temp[,53]~temp[,1]+temp[,2]+temp[,3]+temp[,4]+
temp[,5]+temp[,6]+temp[,7]+temp[,8]+temp[,9]+temp[,10]+temp[,11]+
temp[,12]+temp[,13]+temp[,14]+temp[,15]+temp[,16]+temp[,17]+
temp[,18]+temp[,19]+temp[,20]+temp[,21]+temp[,22]+temp[,23]+
temp[,24]+temp[,25]+temp[,26]+temp[,27]+temp[,28]+temp[,29]+
temp[,30]+temp[,31]+temp[,32]+temp[,33]+temp[,34]+temp[,35]+
temp[,36]+temp[,37]+temp[,38]+temp[,39]+temp[,40]+temp[,41]+
temp[,42]+temp[,43]+temp[,44]+temp[,45]+temp[,46]+temp[,47]+
temp[,48]+temp[,49]+temp[,50]+temp[,51]+temp[,52], data=temp, 
hidden = 25,err.fct = "ce", rep = 1, lifesign = "full",threshold = 20,stepmax = 1e+05)
pr.nn <- compute(model1,trainingtemp,rep=1)


model1 <- neuralnet(temp[,53]~temp[,1]+temp[,2]+temp[,3]+temp[,4]+
                     temp[,5]+temp[,6]+temp[,7]+temp[,8]+temp[,9]+
                     temp[,10]+temp[,11]+temp[,12]+temp[,13]+temp[,14]+
                     temp[,15]+temp[,16]+temp[,17]+temp[,18]+temp[,19]+
                     temp[,20]+temp[,21]+temp[,22]+temp[,23]+temp[,24]+
                     temp[,25]+temp[,26]+temp[,27]+temp[,28]+temp[,29]+
                     temp[,30]+temp[,31]+temp[,32]+temp[,33]+temp[,34]+
                     temp[,35]+temp[,36]+temp[,37]+temp[,38]+temp[,39]
                    , data=temp, hidden = 25, threshold = .09,
                    stepmax = 1e+05, rep = 2, startweights = NULL,
                    learningrate.limit = NULL,
                    learningrate.factor = list(minus = .5, plus = 1.2),
                    learningrate=NULL, lifesign = "full",
                    lifesign.step = 1000, algorithm = "rprop+",
                    err.fct = "ce", act.fct = "tanh",
                    linear.output = TRUE, exclude = NULL,
                    constant.weights = NULL, likelihood = FALSE)

library(nnet)
nnet(classe~., data=training, weights, size=10, Wts, mask,
     linout = FALSE, entropy = FALSE, softmax = T,
     censored = FALSE, skip = FALSE, rang = 0.7, decay = 0,
     maxit = 100, Hess = FALSE, trace = TRUE, MaxNWts = 1000,
     abstol = 1.0e-4, reltol = 1.0e-8)

seedsANN = nnet(classe~., data=training, size=10, softmax=TRUE)
nnettrainpred <- predict(seedsANN, training, type = "class")
nnettestpred <- predict(seedsANN, testing, type = "class")
acctrain=with(training,mean((classe==nnettrainpred)))
acctest=with(testing,mean((classe==nnettestpred))) ##misclassification 

confusetrain <- confusionMatrix(pretrain, training$classe)
confusetest <- confusionMatrix(pretest, testing$classe)





## Number of trees Tunning and Parallel processing in CPUs
library(doParallel); require(foreach)
registerDoParallel(cores=4)
mtry<-11 # number of splits in Random Forest
ntree1<-50 # number of Trees in Bagging
ntree2<-50 # number of trees in Random Forest
ntree3<-50 # number of Trees in Boosting


################## neural network without PCA
# Training Set Accuracy: 98.593444
# Testing Set Accuracy: 96.850153

################## neural network with PCA
# Training Set Accuracy: 96.538650
# Testing Set Accuracy: 93.975535
NNacctrain=98.593444
NNacctest=96.850153
NNPCAacctrain=96.538650
NNPCAacctest=93.975535


library(xtable)
new.table1<-xtable(x[c("MeanDecreaseAccuracy", "MeanDecreaseGini")],digits=4)
new.table2<-xtable(x[c("A", "A.pval", "B", "B.pval", "C")],digits=4)
new.table3<-xtable(x[c("C.pval", "D", "D.pval", "E", "E.pval")],digits = 4)
print(new.table1)
print(new.table2)
print(new.table3)

## training Random forest and gaining accuracy in Training and Testing Datasets 
treenumber<-50
fit2 <- foreach(ntree=treenumber, .combine=randomForest::combine, .packages='randomForest') %dopar%
        randomForest(training[-ncol(training)], mtry=11, training$classe, ntree=ntree)
TestPred <- predict(fit2, newdata=testing)
TrainPred <- predict(fit2, newdata=training)
confusetrain <- confusionMatrix(TrainPred, training$classe)
confusetest <- confusionMatrix(TestPred, testing$classe)
trainerror=fit2$err.rate[treenumber,1]
RFtestacc=with(testing,mean((classe==TestPred)))
RFtrainacc=with(training,mean((classe==TrainPred)))


confusetraintable1<-xtable(confusetrain$byClass[,1:4],digits=4)
confusetraintable2<-xtable(confusetrain$byClass[,5:8],digits=4)
confusetesttable1<-xtable(confusetest$byClass[,1:4],digits=4)
confusetesttable2<-xtable(confusetest$byClass[,5:8],digits=4)

data(iris)
rp1 <- rfPermute(Species ~ ., iris, ntree=50, norm.votes=FALSE, nrep = 100,mtry=5)
rfP$null.dist$pval

### Training bagging model
library(adabag);
start.time <- Sys.time()
fit1 <- foreach(mfinal = ntree1, .combine=bagging, .packages = "adabag") %dopar%
        bagging(classe ~ ., data = training, mfinal = mfinal)
end.time <- Sys.time()
time.taken <- end.time - start.time

PredTest=predict(fit1,newdata=testing,newmfinal=ntree1)$class
PredTrain=predict(fit1,newdata=training,newmfinal=ntree1)$class
bagacctest=with(testing,mean((classe==PredTest))) ##misclassification 
bagacctrain=with(training,mean((classe==PredTrain)))


## gaining Test and Train Error in different number of Trees of Bagging Model
n.trees1=seq(from=1,to=ntree1,by=1)
testerror1 <- rep(0,length(n.trees1))
trainerror1 <- rep(0,length(n.trees1))
for (i in 1:length(n.trees1)){
PredTest=predict(fit1,newdata=testing,newmfinal=n.trees1[i])$class
PredTrain=predict(fit1,newdata=training,newmfinal=n.trees1[i])$class
testerror1[i]=with(testing,mean((classe!=PredTest))) ##misclassification 
trainerror1[i]=with(training,mean((classe!=PredTrain)))
cat(i," ")
}

### random forrest model
library(randomForest);

fit2 <- foreach(ntree=ntree2, .combine=randomForest::combine, .packages='randomForest') %dopar%
        randomForest(training[-ncol(training)], mtry=mtry, training$classe, ntree=ntree)
TestPred <- predict(fit2, newdata=testing)
TrainPred <- predict(fit2, newdata=training)
RFacctest <- with(testing,mean((classe==TestPred))) ##misclassification 
RFacctrain <- with(training,mean((classe==TrainPred)))

confusetrain <- confusionMatrix(TrainPred, training$classe)
confusetest <- confusionMatrix(TestPred, testing$classe)

rf.perm <- rf.significance(fit2, training, nperm=99, ntree=ntree2)
n.trees2=seq(from=1,to=ntree2,by=1)
testerror2 <- rep(0,length(n.trees2))
trainerror2 <- rep(0,length(n.trees2))
RFpredict<-predict(fit2,testing,predict.all=TRUE)$individual

#### training Random Forest and Gaining Accuracy in Train and Test dataset
n.trees2=seq(from=1,to=ntree2,by=1)
testerror2 <- rep(0,length(n.trees2))
trainerror2 <- rep(0,length(n.trees2))
for (i in 1:length(n.trees2)){
fit2 <- foreach(ntree=n.trees2[i], .combine=randomForest::combine, .packages='randomForest') %dopar%
        randomForest(training[-ncol(training)], mtry=mtry, training$classe, ntree=ntree)
        RFpredict<-predict(fit2,testing)
        trainerror2[i]=fit2$err.rate[n.trees2[i],1]
        testerror2[i]=with(testing,mean((classe!=RFpredict)))
        cat(i," ")
}

### Boosting Model
library(gbm);
fit3 <-gbm(classe~., data = training, var.monotone = NULL,
           n.trees = ntree3, interaction.depth = 16, n.minobsinnode = 10,
           shrinkage = 0.3, bag.fraction = 0.5, train.fraction = 1.0,
           cv.folds=0, keep.data = TRUE, verbose = "CV",
           class.stratify.cv=NULL, n.cores = NULL)

################# accuracy is defined in with assigned number of trees #######
TestPred <- predict(fit3, newdata=testing, n.trees=ntree3,type="response")
TrainPred <- predict(fit3, newdata=training, n.trees=ntree3,type="response")
class <- c("A","B","C","D","E")
gbmtestpre<-rep(0, nrow(TestPred))
gbmtrainpre<-rep(0, nrow(TrainPred))
testmaxpre<-apply(TestPred, 1, max)
trainmaxpre<-apply(TrainPred, 1, max)
for (k in 1:nrow(TestPred)){
        gbmtestpre[k] <- class[(TestPred[k,,]==testmaxpre[k])]
}
for (k in 1:nrow(TrainPred)){
        gbmtrainpre[k] <- class[(TrainPred[k,,]==trainmaxpre[k])]
}
boostacctest=with(testing,mean(classe==gbmtestpre))
boostacctrain=with(training,mean(classe==gbmtrainpre))

######################################
n.trees3=seq(from=1,to=ntree3,by=1)
testerror3 <- rep(0,length(n.trees3))
trainerror3 <- rep(0,length(n.trees3))
for (i in 1:length(n.trees3)){
TestPred <- predict(fit3, newdata=testing, n.trees=n.trees3[i],type="response")
TrainPred <- predict(fit3, newdata=training, n.trees=n.trees3[i],type="response")
class <- c("A","B","C","D","E")
gbmtestpre<-rep(0, nrow(TestPred))
gbmtrainpre<-rep(0, nrow(TrainPred))
testmaxpre<-apply(TestPred, 1, max)
trainmaxpre<-apply(TrainPred, 1, max)
for (k in 1:nrow(TestPred)){
        gbmtestpre[k] <- class[(TestPred[k,,]==testmaxpre[k])]
}
for (k in 1:nrow(TrainPred)){
        gbmtrainpre[k] <- class[(TrainPred[k,,]==trainmaxpre[k])]
}
testerror3[i]=with(testing,mean(classe!=gbmtestpre))
trainerror3[i]=with(training,mean(classe!=gbmtrainpre))
cat(i," ")
}

#### Tunning of Bagginng Model
library(ipred);library("rpart");library("MASS")
fit1 <- bagging(classe ~. , data=training, coob=TRUE,ntree=ntree1,OOB=TRUE)


#### Number of Variable Split per Tree Tunning

#### using random forest for tunning
oob.err=double(ncol(training))
test.err=double(ncol(training))
ntree<-50
for(mtry in 1:ncol(training)){
        fit22<- randomForest(classe ~., ntree=ntree, data=training,mtry=mtry)
        oob.err[mtry]=fit22$err.rate[ntree,1]
        pred=predict(fit22,testing)
        test.err[mtry]=with(testing,mean((classe!=pred)))
        cat(mtry," ")
}

#### tunning of boosting model with different depth Values
library(gbm)
mod1<-c(.3) ##shrinkage value
mod2<-c(8,10,12,14,16,18,20,25,30,35) ##depth value
ntree3<-200
oob.error<-rep(0,(length(mod1)*length(mod2)))
valid.error<-rep(0,(length(mod1)*length(mod2)))
test.error<-rep(0,(length(mod1)*length(mod2)))
train.error<-rep(0,(length(mod1)*length(mod2)))
for (j in 1:length(mod1)){
      for (i in 1:length(mod2)){
      depth<-mod2[i]
      shrink<-mod1[j]
      fit33 <-gbm(classe~., data = training, var.monotone = NULL,
      n.trees = ntree3, interaction.depth = depth, n.minobsinnode = 10,
      shrinkage = shrink, bag.fraction = 0.5, train.fraction = 1.0,
      cv.folds=0, keep.data = TRUE, verbose = "CV", class.stratify.cv=NULL,
      n.cores = NULL)
      
      oob.error[((j-1)*length(mod2))+i]=fit33$train.error[ntree3]
      valid.error[((j-1)*length(mod2))+i]=fit33$valid.error[ntree3]
      
      TestPred <- predict(fit33, newdata=testing, n.trees=ntree3,type="response")
      TrainPred <- predict(fit33, newdata=training, n.trees=ntree3,type="response")
      
      class <- c("A","B","C","D","E")
      gbmtestpre<-rep(0, nrow(TestPred))
      gbmtrainpre<-rep(0, nrow(TrainPred))
      testmaxpre<-apply(TestPred, 1, max)
      trainmaxpre<-apply(TrainPred, 1, max)
      for (k in 1:nrow(TestPred)){
              gbmtestpre[k] <- class[(TestPred[k,,]==testmaxpre[k])]
      }
      for (k in 1:nrow(TrainPred)){
              gbmtrainpre[k] <- class[(TrainPred[k,,]==trainmaxpre[k])]
      }
      test.error[((j-1)*length(mod2))+i]=with(testing,mean((classe!=gbmtestpre)))
      train.error[((j-1)*length(mod2))+i]=with(training,mean((classe!=gbmtrainpre)))
      cat((j-1)*length(mod2)+i," ")
      }
}
matplot(mod2, cbind(test.error,oob.error,valid.error,train.error), pch=19, col=c("red","blue","green","black"), type="b",
ylab="MissClassification Error",xlab="Shrinkage Value")
legend("topright",legend=c("Test Error","OOB","Valid Error","Train Error"),pch=19,col=c("red","blue","green","black"))

plot(mod2, fit33$train.error, xlab="Number of Trees",
     ylab="OOB Error Rate", main="Out Of Band Error Rate VS Number of Trees")

fit33 <-gbm(classe~., data = training, var.monotone = NULL,
           n.trees = ntree3, interaction.depth = 6, n.minobsinnode = 10,
           shrinkage = 0.001, bag.fraction = 0.5, train.fraction = 1.0,
           cv.folds=0, keep.data = TRUE, verbose = "CV", class.stratify.cv=NULL, n.cores = NULL)
#boosting model
# library(adabag)
# fit2 <- foreach(mfinal = ntree, .combine = c, .packages = "adabag") %dopar%
#         boosting(classe ~ ., data = training, boos = TRUE, mfinal = mfinal,
#         control = c(minsplit = 0, cp = 0.000001))

##bagging model
fit3 <- foreach(ntree=ntree3, .combine=randomForest::combine, .packages='randomForest') %dopar%
        randomForest(training[-ncol(training)], mtry=53, training$classe, ntree=ntree)

evol.train<-errorevol(fit2,newdata=training)
errorevol(fit2,newdata=Vehicle[-sub, ])->evol.test

matplot(seq(1,ntree3,1),cbind((fit33$train.error)/100,(fit33$cv.error)/100),pch=19,col=c("red","blue"), 
        type="b",ylab="Mean Squared Error",xlab="Number of Variables Per Split",
        main="Number of Trees")
legend("topright",legend=c("bagging","random forrest"),pch=19,col=c("red","blue"))

plot(seq(1,ntree3,1), fit3$err.rate[,1], xlab="Number of Trees",
     ylab="OOB Error Rate", main="Out Of Band Error Rate VS Number of Trees")
dev.copy2pdf(out.type="pdf", file = "ErrorVShrinkage.pdf" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!

TrainPred <- predict(fit33, newdata=training, n.trees=ntree3,type="response")
class <- c("A","B","C","D","E")
TrainPred[1,,]==maxpre[1]
gbmpre<-rep(0, nrow(TrainPred))
maxpre<-apply(TrainPred, 1, max)
for (i in 1:nrow(TrainPred)){
        gbmpre[i] <- class[(TrainPred[i,,]==maxpre[i])]
}

confuse1 <- confusionMatrix(gbmpre, training$classe)

predictionsTe <- predict(gbmModel, newdata=testing)
confuse2 <-confusionMatrix(predictionsTe,testing$classe)


predict.gbm(fit33, newdata=testing, n.trees=ntree3, type = "prob", single.tree=FALSE)

##gbm using caret package
fitControl <- trainControl(method = "repeatedcv", repeats=2,
        number = 10, classProbs = TRUE)
gbmGrid <-  expand.grid(.interaction.depth = 6,
        .n.trees = 500,.shrinkage = c(0.01))
gbmModel <- train(classe~., data = training, var.monotone = NULL,
method = "gbm", trControl = fitControl, tuneGrid = gbmGrid, verbose = FALSE)
plot(gbmModel)

#predict for the test data
prediction <- predict.gbm(object = GBM_model
                          ,newdata = alldata[scorerows,-targindex]
                          ,GBM_NTREES)
############ Cleanning Datasets
temp<-features
temp$classe<-as.character(temp$classe)
temp$classe[temp$classe=="A"]=1
temp$classe[temp$classe=="B"]=2
temp$classe[temp$classe=="C"]=3
temp$classe[temp$classe=="D"]=4
temp$classe[temp$classe=="E"]=5
temp$classe<-as.numeric(temp$classe)
temp$classe
# Write CSV in R
write.table(temp, file = "feature.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
write.table(testing2C, file = "test.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

######### final Test 
fit2 <- foreach(ntree=50, .combine=randomForest::combine, .packages='randomForest') %dopar%
        randomForest(training[-ncol(training)], mtry=11, training$classe, ntree=ntree)
testing2<-read.csv("pml-testing.csv")
testing2C<-testing2[featuresnames[1:(length(featuresnames)-1)]]
TestPred <- predict(fit2, newdata=testing)
predictionsTe2 <- predict(fit2, newdata=testing2C)
predictionsTe2

Train.Accuracy<-c(bagacctrain*100,RFtrainacc*100,boostacctrain*100,NNacctrain,NNPCAacctrain)
Test.Accuracy<-c(bagacctest*100,RFtestacc*100,boostacctest*100,NNacctest,NNPCAacctest)

charac<-c("Bagging Train Accuracy","Bagging Test Accuracy","Random Forest Train Accuracy","Random Forest Test Accuracy",
          "Boosting Train Accuracy","Boosting Test Accuracy","Neural Network Train Accuracy","Neural Network Test Accuracy",
          "Neural Network(using PCA) Train Accuracy", "Neural Network(using PCA) Test Accuracy")
Model<-c("Bagging", "Random Forest",
          "Boosting","Neural Network",
          "Neural Network(using PCA)")
accuracy<-data.frame(cbind(Model,Train.Accuracy,Test.Accuracy))
library(xtable)
new.table.acc<-xtable(accuracy,digits=4)
print(new.table.acc,floating=T,,digits = 4)

###number of important features
# features[c(rownames(perform)[1:(ncol(features)-1)],"classe")]
write.table(ImpFeature,
file = "impFeatures.csv",row.names=F, na="",col.names=F, sep=",")

library(randomForest);library(rfPermute);
rfP <- rfPermute(classe ~ ., data = training, ntree = 50, nrep = 100)
perform<-data.frame(cbind(seq(1,ncol(training)-1,1),rp.importance(rfP,
sort.by = "MeanDecreaseAccuracy",decreasing = T)))
# x<-rbind(head(perform,3),tail(perform,3))
# plot.rfPermute(rfP)
# rp.importance(rfP)
# calc.imp.pval(rfP)
# proximity.plot(rfP)
rfP$importance
imp<-rp.importance(rfP, sort.by = "MeanDecreaseAccuracy",decreasing = F)

ImpFeature <- read.table("impFeatures2.csv",sep=",")
ImpFeature<-features[c(rownames(imp),"classe")]   #alternative important features

treenumber<-50
RFtestacc <- rep(0,(ncol(ImpFeature)-1))
RFtrainacc <- rep(0,(ncol(ImpFeature)-1))
for (i in 1:(ncol(ImpFeature)-1)){
        ImpFea<-ImpFeature[c(names(ImpFeature)[1:i],"classe")]
        ImpTrainData <- createDataPartition(y=ImpFea$classe, p=3/4, list=FALSE )
        ImpTraining <- ImpFea[ImpTrainData,]
        ImpTesting <- ImpFea[-ImpTrainData,]
        fit2 <- foreach(ntree=treenumber, .combine=randomForest::combine, .packages='randomForest') %dopar%
                randomForest(ImpTraining[-ncol(ImpTraining)], mtry=13, ImpTraining$classe, ntree=ntree)
        TestPred <- predict(fit2, newdata=testing)
        TrainPred <- predict(fit2, newdata=training)
        trainerror=fit2$err.rate[treenumber,1]
        RFtestacc[i]=with(ImpTesting,mean((classe==TestPred)))
        RFtrainacc[i]=with(ImpTraining,mean((classe==TrainPred)))
        cat(i," ")
}

#### compressed statistical features
setwd(file.path("C:", "Users", "lenovo", "Desktop", "matlab"))
ImpC<-25
ImpFea<-ImpFeature[c(names(ImpFeature)[1:ImpC],"classe")]
temp<-ImpFea
temp$classe<-as.character(temp$classe)
temp$classe[temp$classe=="A"]=1
temp$classe[temp$classe=="B"]=2
temp$classe[temp$classe=="C"]=3
temp$classe[temp$classe=="D"]=4
temp$classe[temp$classe=="E"]=5
temp$classe<-as.numeric(temp$classe)
write.table(temp, file = "Statisticalfeature2.csv",row.names=FALSE, 
            na="",col.names=FALSE, sep=",")

imptest<-testing2C[c(names(ImpFeature)[1:ImpC])]
write.table(imptest, file = "Statisticaltest.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")


# final test
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
# pml_write_files(predictionsTe2)




