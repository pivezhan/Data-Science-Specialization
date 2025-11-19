setwd(file.path("C:", "Users", "lenovo", "Desktop", "Rlearning", "regression models", "final project"))

###########question1 specify miles per gallon consumption in both Manual and Automate cars 
library(datasets)
data(mtcars)
factor(mtcars$am)
library(dplyr);library(UsingR);
mtcars$am<-factor(mtcars$am)
dfmt<-tbl_df(mtcars)
by_am<-group_by(dfmt,am)
summarize(by_am, mean(disp))
plot(factor(mtcars$am),mtcars$mpg,pch=20,col="green",
   xlab="Automatic=0,Manual=1",ylab="Miles Per Gallon")

library(dplyr);
hotcold<-group_by(mtcars,am)
summarize(hotcold,mean(disp))

amean<-mean(mtcars$disp[mtcars$am==1])
mmean<-mean(mtcars$disp[mtcars$am==0])
asd<-sd(mtcars$disp[mtcars$am==0])
msd<-sd(mtcars$disp[mtcars$am==1])
diff<-rbind(c(amean,asd),c(mmean,msd))
diff<-data.frame(diff)
colnames(diff)<-c("Mean Value","Standard Deviation")
rownames(diff)<-c("Automatic Transmission","Manual Transmission")
diff
plot(mtcars$mpg, mtcars$am,
     xlab="Miles/(US) gallon",
     ylab="Transmission (0 = automatic, 1 = manual)",
     bg="lightblue",
     col="black", cex=1.1, pch=21, frame=F)
abline(lm(am~mpg, data=mtcars), lwd=2)
fit<-lm(mpg~., data=mtcars)

fit <- glm(mpg~., data=mtcars)
coef(fit)
summary(fit)$coefficients
#########question 2 

