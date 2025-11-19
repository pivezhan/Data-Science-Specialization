# #install.packages("UsingR")
# 
# library(UsingR);data(galton)
# par(mfrow=c(1,2))
# hist(galton$child,col="blue",breaks=100)
# hist(galton$parent,col="blue",breaks=100)
# 
# 
# library(UsingR); data(galton); library(reshape); long <- melt(galton)
# g <- ggplot(long, aes(x = value, fill = variable))
# g <- g + geom_histogram(colour = "black", binwidth=1)
# g <- g + facet_grid(. ~ variable)
# g



# library(manipulate)
# myHist<-function(mu){
#   hist(galton$child,col="blue",breaks=100)
#   lines(c(mu,mu),c(0,150),col="red",lwd=5)
#   mse<-mean((galton$child-mu)^2)
#   text(63,150,paste("mu=",mu))
#   text(63,140,paste("MSE=",round(mse,2)))
# }
# manipulate(myHist(mu),mu=slider(62,74,step=0.5))
  

# mean(galton$child)
# ggplot(galton, aes(x = parent, y = child)) + geom_point()


myPlot <- function(beta){
  y <- galton$child - mean(galton$child)
  x <- galton$parent - mean(galton$parent)
  freqData <- as.data.frame(table(x, y))
  names(freqData) <- c( "child", "parent", "freq")
  
  plot(
    as.numeric(as.vector(freqData$parent)),
    as.numeric(as.vector(freqData$child)),
    pch=21,col="black",bg="lightblue",
    cex=.15*freqData$freq,
    xlab="parent",ylab="child"
    )
  abline(0,beta,lwd=3)
  points(0,0,cex=2,pch=19)
  mse<-mean((y-beta*x)^2)
  title(paste("beta=",beta,"mse=",round(mse,3)))
  }
manipulate(myPlot(beta), beta = slider( 0.8, 1.2, step = 0.02))

#quiz1 question1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
weighted.mean(x, w)

least<-function(x,w,u){
  for (i in 1:length(x)){
    s<-w*((x-u)^2)
  }
  sum(s)
}
#quiz1 question2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
sum(x*y)/sum(x^2)


#quiz1 question3
data(mtcars)
x=mtcars$mpg
y=mtcars$wt
beta1<-cor(y,x)*sd(x)/sd(y)

#quiz1 question6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x-mean(x))/sd(x)

#quiz1 question7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
beta1<-cor(x,y)/(sd(x)*sd(y))
beta0<-mean(y)-beta1*mean(x)
rbind(c(beta0,beta1),coef(lm(y~x)))






