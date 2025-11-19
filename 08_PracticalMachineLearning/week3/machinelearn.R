setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\practical machine learning\\week3")

##predicting with trees 
# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache=TRUE, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
        if(is.numeric(x)) {
                round(x, getOption('digits'))
        } else {
                paste(as.character(x), collapse = ', ')
        }
})
knit_hooks$set(plot = knitr:::hook_plot_html)


##### decision Treees

par(mar=c(0,0,0,0)); set.seed(1234); x = rep(1:4,each=4); y = rep(1:4,4)
plot(x,y,xaxt="n",yaxt="n",cex=3,col=c(rep("blue",15),rep("red",1)),pch=19)


par(mar=c(0,0,0,0)); 
plot(x,y,xaxt="n",yaxt="n",cex=3,col=c(rep("blue",8),rep("red",8)),pch=19)

data(iris); library(ggplot2);library(caret);
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

qplot(Petal.Width,Sepal.Width,colour=Species,data=training)

library(caret)
modFit1 <- train(Species ~ .,method="rpart",data=training)
print(modFit$finalModel)
modFit2 <- train(Species ~ .,method="bagFDA",data=training)
modFit3<-cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))
plot(modFit3$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

library(rattle)
fancyRpartPlot(modFit2$finalModel)

#### predicting new values
predict(modFit,newdata=testing)

# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
        if(is.numeric(x)) {
                round(x, getOption('digits'))
        } else {
                paste(as.character(x), collapse = ', ')
        }
})
knit_hooks$set(plot = knitr:::hook_plot_html)

########   bagging   ##########

library(ElemStatLearn); data(ozone,package="ElemStatLearn")
library(caret)
ozone <- ozone[order(ozone$ozone),]
head(ozone)

ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
        ss <- sample(1:dim(ozone)[1],replace=T)
        ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
        loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
        ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)

predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))
####un authorized error
plot(ozone$ozone,temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")

ctreeBag$fit

ctreeBag$pred

ctreeBag$aggregate



#######random forests
# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, fig.align = 'center', cache=TRUE, dpi = 100, tidy = F, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
        if(is.numeric(x)) {
                round(x, getOption('digits'))
        } else {
                paste(as.character(x), collapse = ', ')
        }
})
knit_hooks$set(plot = knitr:::hook_plot_html)

data(iris); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]


library(caret)
modFit <- train(Species~ .,data=training,method="rf",prox=TRUE)
modFit

getTree(modFit$finalModel,k=2)

irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species,data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),
               size=5,shape=4,data=irisP)

pred <- predict(modFit,testing); testing$predRight <- pred==testing$Species
table(pred,testing$Species)

qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,
      main="newdata Predictions")

###################     boosting  ##################
## make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA,
               fig.align = 'center', dpi = 100, cache=TRUE,tidy = F,
               cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
        if(is.numeric(x)) {
                round(x, getOption('digits'))
        } else {
                paste(as.character(x), collapse = ', ')
        }
})
knit_hooks$set(plot = knitr:::hook_plot_html)

library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)
print(modFit)

qplot(predict(modFit,testing),wage,data=testing)

# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, 
               fig.align = 'center', dpi = 100, cache=TRUE,tidy = F, 
               cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
        if(is.numeric(x)) {
                round(x, getOption('digits'))
        } else {
                paste(as.character(x), collapse = ', ')
        }
})
knit_hooks$set(plot = knitr:::hook_plot_html)

######################### model based prediction #######################
data(iris); library(ggplot2)
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training2 <- iris[inTrain,]
testing2 <- iris[-inTrain,]
dim(training2); 
dim(testing2)

modlda = train(Species ~ ., data=training2, method="lda")
modnb = train(Species ~ ., data=training2, method="nb")
plda = predict(modlda,testing2); pnb = predict(modnb,testing2)
table(plda,pnb)

modfit2 <- train(Species ~.,method="rf", data=training2)
pred2 <- predict(modfit2,testing2);

confusionMatrix(testing2$Species,pred2)$overal

equalPredictions = (plda==pnb)
qplot(Petal.Width,Sepal.Width,colour=equalPredictions,data=testing)



