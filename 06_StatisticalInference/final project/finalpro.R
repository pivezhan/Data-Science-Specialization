setwd(file.path("D:", "sbu", "RLearning", "statistical inference", "final project"))

mean(rexp(n, .2))
sd(rexp(n, .2))

n<-10000
means<-cumsum(rexp(n, .2)/(1:n))
library(ggplot2)
g<-ggplot(data.frame(x=1:n,y=means),aes(x=x,y=y))
g<-g+geom_hline(yintercept=0)+geom_line(size=2)
g<-g+labs(x="Number of obs",y="cumulative mean")
g
dev.copy(png, file = "samplemean.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!

n<-10000
variance<-cumsum(((rexp(n, .2)^2)/(1:n))-(rexp(n, .2)/(1:n))^2)
# variance<-cumsum(((rexp(n, .2)/(1:n))-(mean(rexp(n, .2))))^2/(n-1))

library(ggplot2)
g<-ggplot(data.frame(x=1:n,y=variance),aes(x=x,y=y))
g<-g+geom_hline(yintercept=0)+geom_line(size=2)
g<-g+labs(x="Number of obs",y="cumulative variance")
g
dev.copy(png, file = "samplevariance.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!

mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(rexp(40, .2)))
hist(mns,col="blue",100)

std = NULL
for (i in 1 : 1000) std = c(mns, sd(rexp(40, .2)))
hist(std,col="blue",100)


length(rexp(40, .2))


hist(runif(1000))



# plot the histogram of averages
set.seed(3)
lambda <- 0.2
num_sim <- 1000
sample_size <- 40
sim <- matrix(rexp(num_sim*sample_size, rate=lambda), num_sim, sample_size)
row_means <- rowMeans(sim)

hist(row_means, breaks=50, prob=TRUE,
     main="Distribution of averages of samples,
     drawn from exponential distribution with lambda=0.2",
     xlab="")
# density of the averages of samples
lines(density(row_means))
# theoretical center of distribution
abline(v=1/lambda, col="red")
# theoretical density of the averages of samples
xfit <- seq(min(row_means), max(row_means), length=100)
yfit <- dnorm(xfit, mean=1/lambda, sd=(1/lambda/sqrt(sample_size)))
lines(xfit, yfit, pch=22, col="red", lty=2)
# add legend
legend('topright', c("simulation", "theoretical"), lty=c(1,2), col=c("black", "red"))


library(scales)
v<-rescale(yfit, length(dty$y))


library(datasets)
data(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)
