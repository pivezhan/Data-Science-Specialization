setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\practical machine learning\\week4")

###regularized regression
library(ElemStatLearn); data(prostate)
str(prostate)

small = prostate[1:5,]
lm(lpsa ~ .,data =small)

####combining predictor
library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))

# Create a building data set and validation set
inBuild <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]

inTrain <- createDataPartition(y=buildData$wage,
                               p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]


dim(training)
dim(testing)
dim(validation)

mod1 <- train(wage ~.,method="glm",data=training)
mod2 <- train(wage ~.,method="rf",
              data=training, 
              trControl = trainControl(method="cv"),number=3)

pred1 <- predict(mod1,testing); pred2 <- predict(mod2,testing)
# qplot(pred1,pred2,colour=wage,data=testing)

predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage ~.,method="gam",data=predDF)
combPred <- predict(combModFit,predDF)

confusionMatrix (testing$wage,pred1)


sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))

pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)

sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2))

####unsupervised learning
data(iris); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

#cluster with kmeans
kMeans1 <- kmeans(subset(training,select=-c(Species)),centers=3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width,Petal.Length,colour=clusters,data=training)

##compare with real values
table(kMeans1$cluster,training$Species)

# build predictor
modFit <- train(clusters ~.,data=subset(training,select=-c(Species)),method="rpart")
table(predict(modFit,training),training$Species)


##apply to test
testClusterPred <- predict(modFit,testing) 
table(testClusterPred ,testing$Species)

#########forecasting
library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from = from.dat, to = to.dat)
head(GOOG)

mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen,frequency=12)
plot(ts1,xlab="Years+1", ylab="GOOG")
plot(decompose(ts1),xlab="Years+1")s
ts1Train <- window(ts1,start=1,end=5)
ts1Test <- window(ts1,start=5,end=(7-0.01))
ts1Train

plot(ts1Train)
lines(ma(ts1Train,order=3),col="red")

ets1 <- ets(ts1Train,model="MMM")
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test,col="red")
accuracy(fcast,ts1Test)

# QUIZ 4
# question1
library(ISLR); library(ggplot2); 
library(caret);library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
set.seed(33833)
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

mod1 <- train(y ~.,method="gbm",data=vowel.train)
mod2 <- train(y ~.,method="rf", data=vowel.train)

pred1 <- predict(mod1,vowel.test); pred2 <- predict(mod2,vowel.test)
predDF <- data.frame(pred1,pred2,y=vowel.test$y,agree = hat1 == hat2)
combModFit <- train(y ~.,method="gam",data=predDF)
combPred <- predict(combModFit,predDF)
accuracy <- sum(pred1[predDF$agree] == predDF$y[predDF$agree]) / sum(predDF$agree)
accuracy

identical(levels(pred1),levels(vowel.test$y))
# head(vowel.test$y)

confusionMatrix(as.numeric(vowel.test$y),pred1)$overall
confusionMatrix(as.numeric(vowel.test$y),pred2)$overall
confusionMatrix(as.numeric(vowel.test$y),combPred)$overall

# table(pred1,vowel.test$y)
# table(pred2,vowel.test$y)
# table(combPred,vowel.test$y)

# Question 2
library(caret)
library(gbm);library(MASS)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
mod1 <- train(diagnosis ~.,method="gbm",data=training)
mod2 <- train(diagnosis ~.,method="lda" , data=training )
mod3 <- train(diagnosis ~.,method="rf",data=training)

pred1 <- predict(mod1,testing); 
pred2 <- predict(mod2,testing);
pred3 <- predict(mod3,testing);

# table(pred3,testing$diagnosis)

predDF <- data.frame(pred1,pred2,pred3,diagnosis=testing$diagnosis)
combModFit <- train(diagnosis ~.,method="rf",data=predDF)
combPred <- predict(combModFit,predDF)
confusionMatrix(testing$diagnosis,pred1)$overall
confusionMatrix(testing$diagnosis,pred2)$overall
confusionMatrix(testing$diagnosis,pred3)$overall
confusionMatrix(testing$diagnosis,combPred)$overall

##question3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)# ?plot.enet
M1 <- train(CompressiveStrength ~ ., data=training, method="lasso")
M1
plot(M1$finalModel, xvar="penalty")


## question 4
library(lubridate)  # For year() function below
library(forecast)
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest = ts(testing$visitsTumblr)

fit <- bats(tstrain)
fit
plot(forecast(fit));
fcast <- forecast(fit,length(testing$visitsTumblr))
hat <- cbind(testing, data.frame(fcast))
hat$isIn95 <- hat$Lo.95 < hat$visitsTumblr & hat$visitsTumblr < hat$Hi.95
prop.table(table(hat$isIn95))
# lines(tstest,col="red")
# accuracy(fcast,tstest)
# fcast

##question 5
set.seed(3523)
library(AppliedPredictiveModeling);library(e1071)
data(concrete)
inTrain1 = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training1 = concrete[ inTrain1,]
testing1 = concrete[-inTrain1,]
set.seed(325)

model1 <- svm(CompressiveStrength~.,data=training1)
pred1 <- predict(model1,testing1)
sqrt(sum((pred-testing1$CompressiveStrength)^2)/length(testing1$CompressiveStrength))


names(training)
