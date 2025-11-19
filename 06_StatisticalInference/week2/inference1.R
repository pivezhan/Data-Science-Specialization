##standard normal
nosim<-1000
n<-10
sd(apply(matrix(rnorm(nosim*n),nosim),1,mean))
1/sqrt(n)

##standard uniform
nosim<-1000
n<-10
sd(apply(matrix(runif(nosim*n),nosim),1,mean))
1/sqrt(12*n)

##standard poisson
nosim<-1000
n<-10
sd(apply(matrix(rpois(nosim*n,4),nosim),1,mean))
2/sqrt(n)

##coin flip
nosim<-1000
n<-10
sd(apply(matrix(sample(0:1,nosim*n,replace=T),nosim),1,mean))
1/(2*sqrt(n))

##Data example
library(UsingR);data(father.son);
x<-father.son$sheight
n<-length(x)
round(c(var(x),var(x)/n,sd(x),sd(x)/sqrt(n)),2)

##binomial trials example
choose(8,7)*.5^8+choose(8,8)*.5^8
pbinom(6,size=8,prob=.5,lower.tail=F)

##95 percentile of a gaussian distribution
qnorm(.95, mean=0, sd=1)

##examples
pnorm(1160,mean=1020,sd=50,lower.tail=F)
pnorm(2.8,sd=50,lower.tail=F)

qnorm(.75,mean=1020,sd=50)


## poison example
ppois(3,lambda=2.5*4)


##poison comparison
pbinom(2,size=500,prob=0.01)
ppois(2,lambda=500*.01)

n<-1000
means<-cumsum(rnorm(n)/(1:n))
library(ggplot2)
g<-ggplot(data.frame(x=1:n,y=means),aes(x=x,y=y))
g<-g+geom_hline(yintercept=0)+geom_line(size=2)
g<-g+labs(x="Number of obs",y="cumulative mean")
g


library(UsingR)
data(father.son);
x<-father.son$sheight
(mean(x)+c(-1,1)*qnorm(0.975)*sd(x)/sqrt(length(x)))/12

##campain win  probability
round(1/sqrt(10^(1:6)),3)

0.56+c(-1,1)*qnorm(0.975)*sqrt(.56*.44/100)
## up function is equivalent to this
binom.test(56,100)$conf.int #as 56 percent win probability


n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
        phats <- rbinom(nosim, prob = p, size = n)/n
        ll <- phats - qnorm(0.975) * sqrt(phats * (1- phats)/n)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1- phats)/n)
        mean(ll < p & ul > p)
})


n <- 100
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage2 <- sapply(pvals, function(p) {
        phats <- rbinom(nosim, prob = p, size = n)/n
        ll <- phats - qnorm(0.975) * sqrt(phats * (1- phats)/n)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1- phats)/n)
        mean(ll < p & ul > p)
})

n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
        phats <- (rbinom(nosim, prob = p, size = n) + 2)/(n + 4)
        ll <- phats - qnorm(0.975) * sqrt(phats * (1- phats)/n)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1- phats)/n)
        mean(ll < p & ul > p)
})

x <- 5
t <- 94.32
lambda <- x/t
round(lambda + c(-1, 1) * qnorm(0.975) * sqrt(lambda/t), 3)
poisson.test(x, T= 94.32)$conf


lambdavals <- seq(0.005, 0.1, by = 0.01)
nosim <- 1000
t <- 100
coverage <- sapply(lambdavals, function(lambda) {
        lhats <- rpois(nosim, lambda = lambda * t)/t
        ll <- lhats - qnorm(0.975) * sqrt(lhats/t)
        ul <- lhats + qnorm(0.975) * sqrt(lhats/t)
        mean(ll < lambda & ul > lambda)
})




