##spam example on data splitting

library(caret);library(kernlab);data(spam)
inTrain<-createDataPartition(y=spam$type,
                            p=0.75,list=F)
training<-spam[inTrain,]
testing<-spam[-inTrain,]
dim(training)

##fit a model
set.seed(32343)
modelFit<-train(type~.,data=training,method="glm")
modelFit

modelFit$finalModel

##prediction
prediction<-predict(modelFit,newdata=testing)
prediction

##confusion matrix
confusionMatrix(prediction,testing$type)

##k fold splitting
set.seed(32323)
folds<-createFolds(y=spam$type,k=10,
                   list=T,returnTrain=T)
sapply(folds,length)

folds[[1]][1:10]

folds<-createFolds(y=spam$type,k=10,
                   list=T,returnTrain=F)
sapply(folds,length)

folds[[1]][1:10]

##resampling
set.seed(32323)
folds<-createResample(y=spam$type,time=10,list=T)
sapply(folds,length)
folds[[1]][1:10]



##time slices
set.seed(32323)
tme<-1:1000
folds<-createTimeSlices(y=tme,initialWindow=20,
                        horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]

##trainning option
args(train.default)

# function(x,y,method='rf',preProcess=NULL,...,weight=NULL,
#          metric=ifelse(is.factor(y),"Accuracy",,"RMSE"),
#          maximize=ifelse(metric=="RMSE",F,T),trControl=trainControl(),
#          tuneGrid=NULL,tuneLength=3)
#         NULL

args(trainControl)
# 
# function (method = "boot", number = ifelse(grepl("cv", method), 
#                                            10, 25), repeats = ifelse(grepl("cv", method), 1, number), 
#           p = 0.75, initialWindow = NULL, horizon = 1, fixedWindow = TRUE, 
#           verboseIter = FALSE, returnData = TRUE, returnResamp = "final", 
#           savePredictions = FALSE, classProbs = FALSE, summaryFunction = defaultSummary, 
#           selectionFunction = "best", preProcOptions = list(thresh = 0.95, 
#                                                             ICAcomp = 3, k = 5), index = NULL, indexOut = NULL, timingSamps = 0, 
#           predictionBounds = rep(FALSE, 2), seeds = NA, adaptive = list(min = 5, 
#                                                                         alpha = 0.05, method = "gls", complete = TRUE), allowParallel = TRUE) 
#         NULL


#### different seeds work like different ips
set.seed(1234)
modelFit2<-train(type~.,data=training,method="glm")
modelFit2


set.seed(1235)
modelFit3<-train(type~.,data=training,method="glm")
modelFit3

##wage data
library(ISLR);library(ggplot2);library(caret);
data(Wage)
summary(Wage)


##get training and test sets
inTrain<- createDataPartition(y=Wage$wage,p=0.7,list=F)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
dim(training);dim(testing)

####Feature plt in caret package
featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,
            plot="pairs")

###qplot with and without color
qplot(age,wage,data=training)

qplot(age,wage,colour=jobclass,data=training)

##qplot with color and regression smoother
qq<-qplot(age,wage,colour=education,data=training)
qq+geom_smooth(method='lm',formula=y~x)


####cat2 plotting for make factors in dataset
library(Hmisc)
cutWage<-cut2(training$wage,g=3)
table(cutWage)


##boxplot
p1<-qplot(cutWage,age,data=training,fill=cutWage,
          geom=c("boxplot"))
p1


##boxplot with points overlayed
p2<-qplot(cutWage,age,data=training,fill=cutWage,
          geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)


###Tables

t1<-table(cutWage,training$jobclass)
t1

## correlation between features of groups
## (t1,1) proportion in each row and (t1,2)
##proportion in each column (package not found!!!!)
prob.table(t1,1)
Density

qplot(wage,colour=education,data=training,geom="density")





