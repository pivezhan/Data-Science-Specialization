setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\regression models\\week2")

##diamond dataset frm UsingR

library(UsingR)
data(diamond)
 plot(diamond$carat, diamond$price, 
     xlab="Mass (carats)",
     ylab="Price (SIN $)",
     bg="lightblue",
     col="black", cex=1.1, pch=21, frame=F)
abline(lm(price~carat, data=diamond), lwd=2)

##fitting linear regression model
library(UsingR)
fit<-lm(price~carat, data=diamond)
coef(fit)

##Getting interpretable intercept
fit2<-lm(price~I(carat-mean(carat)),data=diamond)
coef(fit2)

##scale changing
fit3<-lm(price~I(carat*10),data=diamond)
coef(fit3)

##prediction price  of diamond
newx<-c(0.16,0.27,0.34)
coef(fit)[1]+coef(fit)[2]*newx

predict(fit,newdata=data.frame(carat=newx))

##residual code
data(diamond)
y<-diamond$price; x<-diamond$carat; n<-length(y)
fit<-lm(y~x)
e<-resid(fit)
yhat<-predict(fit)
max(abs(e-(y-yhat)))  ## residual are same as difference y and yhat
max(abs(e-(y-coef(fit)[1]-coef(fit)[2]*x))) ## equivalent to upper codes


## nonlinear data and residual plot
x<-runif(100,-3,3);y<-x+sin(x)+rnorm(100,sd=.2);
plot(x,y);abline(lm(y~x))
plot(x,resid(lm(y~x)));
abline(h=0)


##hetroskedasticity
x<-runif(100,0,6);y<-x+rnorm(100,mean=0,sd=0.001*x);
plot(x,y);abline(lm(y~x))

plot(x,resid(lm(y~x)));
abline(h=0);


## diamond example
y<-diamond$price;x<-diamond$carat;n<-length(y)
fit<-lm(y~x)
summary(fit)$sigma

sqrt(sum(resid(fit)^2)/(n-2))

data(anscombe);
example(anscombe)

###example diamond data set
library(UsingR);data(diamond)
y<-diamond$price;x<-diamond$carat;n<-length(y)
beta1<-cor(y,x)*sd(y)/sd(x)
beta0<-mean(y)-beta1*mean(x)
e<-y-beta0-beta1*x
sigma<-sqrt(sum(e^2)/(n-2))
ssx<-sum((x-mean(x))^2)
seBeta0<-(1/n+mean(x)^2/ssx)^.5*sigma
seBeta1<-sigma/sqrt(ssx)
tBeta0<-beta0/seBeta0;tBeta1<-beta1/seBeta1
pBeta0<-2*pt(abs(tBeta0),df=n-2,lower.tail=F)
pBeta1<-2*pt(abs(tBeta1),df=n-2,lower.tail=F)
coefTable<-rbind(c(beta0,seBeta0,tBeta0,pBeta0),c(beta1,seBeta1,tBeta1,pBeta1))
colnames(coefTable)<-c("Estimate","std. Error","t value","p(>|t|")
rownames(coefTable)<-c("(intercept","x")
coefTable

fit<-lm(y~x)
summary(fit)$coefficients   ## this code is equivalent to upper code

###Getting a confidence interval
sumCoef<-summary(fit)$coefficients
sumCoef[1,1]+c(-1,1)*qt(.975,df=fit$df)*sumCoef[1,2]

sumCoef[1,1]+c(-1,1)*qt(.975,df=fit$df)*sumCoef[2,2]


### plotting and prediction intervals
plot(x,y,frame=F,xlab="carat",ylab="Dollars", pch=21,col="black",bg="lightblue", cex=2)
abline(fit,lwd=2)
xVal<-seq(min(x),max(x),by=.01)
yVal<-beta0+beta1*xVal
se1<-sigma*sqrt(1/n+(xVal-mean(x))^2/ssx)
se2<-sigma*sqrt(1+1/n+(xVal-mean(x))^2/ssx)
lines(xVal,yVal+2*se1)
lines(xVal,yVal-2*se1)
lines(xVal,yVal+2*se2)
lines(xVal,yVal-2*se2)

newdata<-data.frame(x=xVal)
p1<-predict(fit, newdata, interval=("confidence"))
p2<-predict(fit, newdata, interval=("prediction"))
plot(x,y,frame=F,xlab="Carat",ylab="Dollars",pch=21,col="black",bg="lightblue",cex=2)
abline(fit,lwd=2)
lines(xVal,p1[,2]);lines(xVal,p1[,3])
lines(xVal,p2[,2]);lines(xVal,p2[,3])


## using an example in multivariate regression model
n<-100;x<rnorm(n);x2<-rnorm(n);x3<-rnorm(n)
y<-x+x2+x3+rnorm(n,sd=.1)
e<-function(a,b)a-sum(a*b)/sum(b^2)*b  ## simple form of creation of function
ey<-e(e(y,x2),e(x3,x2))
ex<-e(e(x,x2),e(x3,x2))
sum(ey*ex)/sum(ex^2)

coef(lm(y~x+x2+x3-1))

## showing that the order is not matter at all
ey<-e(e(y,x3),e(x2,x3))
ey<-e(e(y,x3),e(x2,x3))
sum(ey*ex)/sum(ex^2)
coef(lm(y~x+x2+x3-1))  


###another method but not working

library(UsingR)
ey<-resid(lm(y~x2+x3-1))
ex<-resid(lm(x~x2+x3-1))
sum(ey*ex)/sum(ex^2)
coef(lm(y~x+x2+x3-1))  



### quiz2 question 1,2
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
library(UsingR)
beta1<-cor(y,x)*sd(y)/sd(x)
beta0<-mean(y)-beta1*mean(x)
e<-y-beta0-beta1*x
sigma<-sqrt(sum(e^2)/(n-2))
ssx<-sum((x-mean(x))^2)
seBeta0<-(1/n+mean(x)^2/ssx)^.5*sigma
seBeta1<-sigma/sqrt(ssx)
tBeta0<-beta0/seBeta0;tBeta1<-beta1/seBeta1
pBeta0<-2*pt(abs(tBeta0),df=n-2,lower.tail=F)
pBeta1<-2*pt(abs(tBeta1),df=n-2,lower.tail=F)
coefTable<-rbind(c(beta0,seBeta0,tBeta0,pBeta0),c(beta1,seBeta1,tBeta1,pBeta1))
colnames(coefTable)<-c("Estimate","std. Error","t value","p(>|t|")
rownames(coefTable)<-c("(intercept","x")
coefTable


###quiz2 question3
data(mtcars)
y<-mtcars$mpg;x<-mtcars$wt;n<-length(y)
fit<-lm(y~x)
summary(fit)$coefficients   ## this code is equivalent to upper code
sumCoef[1,1]+c(-1,1)*qt(.95,df=fit$df)*sumCoef[1,2]

sumCoef[1,1]+c(-1,1)*qt(.95,df=fit$df)*sumCoef[2,2]


 