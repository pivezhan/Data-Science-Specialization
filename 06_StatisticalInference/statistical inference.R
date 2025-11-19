# x<-c(-0.5,0,1,1,1,1.5)
# y<-c(0,0,2,0,0)
# y=2x
# plot(x,y,lwd=3,frame=FALSE,type="1")
library(manipulate)
y<-function(x){
  if (0<x & x<1){
   2*x
  }else{
    0
  }
}

myHist<-function(mu){
  g<-ggplot(galton,aes(x=child))
  g<-g+geom_histogram(fill="salmon",binwidth=1,aes(y=..density..),colour="black")
  g<-g+geom_density(size=2)
  g<-g+geom_vline(xintercept=mu,size=2)
  mse<-round(mean((galton$child-mu)^2),3)
  g<-g+labs(title=paste('mu=',mu,'MSE=',mse))
  g
}
manipulate(myHist(mu),mu=slider(62,74,step=0.5))

library(manipulate)
myHist<-function(mu){
hist(galton$child,col="blue",break=100)
lines(c(mu,mu),c(0,150),col="red",lwd=5)
mse<-mean((galton$child-mu)^2)
text(63,150,paste("mu=",mu))
text(63,140,paste("MSE=",round(mse,2)))
}
manipulate(myHist(mu),mu=slider(62,74,step=0.5))
