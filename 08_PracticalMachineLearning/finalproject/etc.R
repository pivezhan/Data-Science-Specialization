require(randomForest)
require(MASS)
set.seed(101)
dim(Boston)
train=sample(1:nrow(Boston),300)
rf.boston=randomForest(medv~.,data=Boston,subset=train)
oob.err=double(13)
test.err=double(13)

for(mtry in 1:13){
        fit=randomForest(medv~.,data=Boston,subset=train,mtry=mtry,ntree=400)
        oob.err[mtry]=fit$mse[400]
        pred=predict(fit,Boston[-train,])
        test.err[mtry]=with(Boston[-train,],mean((medv-pred)^2))
        cat(mtry," ")
}

matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))

library(xtable)
data(tli)
 ## Demonstrate data.frame
tli.table <- xtable(tli[1:10,])
digits(tli.table)[c(2,6)] <- 0
print(tli.table,floating=FALSE)

require(foreach)
registerDoParallel(cores=4)
 system.time(m <- foreach(i=1:100) %dopar% 
{ matrix(rnorm(1000*1000), ncol=5000) } )

user  system elapsed 
11.522   4.082  24.444 

a<-c(242,120,163,131,119,120,260,153,130,81,489,378)
sum(a)
b<-c(48,43,41,41,41,37)
sum(b)
hist(a)
