setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\statistical inference\\week4")




#############Manipulate
library(manipulate);library(ggplot2)
mu0 = 30
myplot <- function(sigma, mua, n, alpha) {
        g = ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
        g = g + stat_function(fun = dnorm, geom = "line", args = list(mean = mu0, 
                                                                      sd = sigma/sqrt(n)), size = 2, col = "red")
        g = g + stat_function(fun = dnorm, geom = "line", args = list(mean = mua, 
                                                                      sd = sigma/sqrt(n)), size = 2, col = "blue")
        xitc = mu0 + qnorm(1- alpha) * sigma/sqrt(n)
        g = g + geom_vline(xintercept = xitc, size = 3)
        g
}
manipulate(myplot(sigma, mua, n,alpha), sigma = slider(1, 10, step = 1, initial = 4), 
           mua = slider(30, 35, step = 1, initial = 32), n = slider(1, 50, step = 1, 
                                                                    initial = 16), alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05))

###### POWER
sigma <- 10; mu_0 = 0; mu_a = 2; n <- 100; alpha = .05
plot(c(-3, 6),c(0, dnorm(0)), type = "n", frame = false, xlab = "Z value", ylab = "")
xvals <- seq(-3, 6, length = 1000)
lines(xvals, dnorm(xvals), type = "l", lwd = 3)
lines(xvals, dnorm(xvals, mean = sqrt(n) * (mu_a - mu_0) / sigma), lwd =3)
abline(v = qnorm(1 - alpha))

power.t.test(n = 16, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$power
power.t.test(n = 16, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$power
power.t.test(n = 16, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$power

power.t.test(power = .8, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$n
power.t.test(power = .8, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$n
power.t.test(power = .8, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$n

set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000){
        y <- rnorm(20)
        x <- rnorm(20)
        pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}

# Controls false positive rate
sum(pValues < 0.05)

# Controls FWER 
sum(p.adjust(pValues,method="bonferroni") < 0.05)
# Controls FDR 
sum(p.adjust(pValues,method="BH") < 0.05)



##### multiple testing
set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000){
        x <- rnorm(20)
        # First 500 beta=0, last 500 beta=2
        if(i <= 500){y <- rnorm(20)}else{ y <- rnorm(20,mean=2*x)}
        pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}
trueStatus <- rep(c("zero","not zero"),each=500)
table(pValues < 0.05, trueStatus)

# Controls FWER 
table(p.adjust(pValues,method="bonferroni") < 0.05,trueStatus)
# Controls FDR 
table(p.adjust(pValues,method="BH") < 0.05,trueStatus)

par(mfrow=c(1,2))
plot(pValues,p.adjust(pValues,method="bonferroni"),pch=19)
plot(pValues,p.adjust(pValues,method="BH"),pch=19)


##resampled Data

library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
theta <- median(x)
jk <- sapply(1 : n,
             function(i) median(x[-i])
)
thetaBar <- mean(jk)
biasEst <- (n - 1) * (thetaBar - theta) 
seEst <- sqrt((n - 1) * mean((jk - thetaBar)^2))

c(biasEst, seEst)
library(bootstrap)
temp <- jackknife(x, median)
c(temp$jack.bias, temp$jack.se)

B <- 1000
resamples <- matrix(sample(x,
                           n * B,
                           replace = TRUE),
                    B, n)
medians <- apply(resamples, 1, median)
sd(medians)
quantile(medians, c(.025, .975))

hist(medians)

data(InsectSprays)
boxplot(count ~ spray, data = InsectSprays)

subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1 : 10000, function(i) testStat(y, sample(group)))
observedStat
mean(permutations > observedStat)

hist(permutations)



####### Resampled Inference
library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
theta <- median(x)
jk <- sapply(1 : n,
             function(i) median(x[-i])
)
thetaBar <- mean(jk)
biasEst <- (n - 1) * (thetaBar - theta) 
seEst <- sqrt((n - 1) * mean((jk - thetaBar)^2))

c(biasEst, seEst)
library(bootstrap)
temp <- jackknife(x, median)
c(temp$jack.bias, temp$jack.se)

B <- 1000
resamples <- matrix(sample(x,
                           n * B,
                           replace = TRUE),
                    B, n)
medians <- apply(resamples, 1, median)
sd(medians)
quantile(medians, c(.025, .975))

hist(medians)

data(InsectSprays)
boxplot(count ~ spray, data = InsectSprays)

subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1 : 10000, function(i) testStat(y, sample(group)))
observedStat
mean(permutations > observedStat)

hist(permutations)



