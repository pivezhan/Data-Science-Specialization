# homework 4
# q1
library(datasets); data(mtcars);
# head(mtcars)
xbar=mean(mtcars$mpg)
xbar+c(-1,1)*qnorm(.05)*(sd(mtcars$mpg)/sqrt(nrow(mtcars)))

# q2
m4 <- mtcars$mpg[mtcars$cyl == 4]
m6 <- mtcars$mpg[mtcars$cyl == 6]
t.test(m4,m6, paired=FALSE, altnernative="two.sided")

# q3
n=100
mPSA=3.0
sdPSA=1.1
mPSA+c(-1,1)*qnorm(.975) *(sdPSA/sqrt(n))

# q4
H0=>p=.5,Ha=>p=.55
ans <- round(pbinom(55-1, prob = .5, size = 100, lower.tail = FALSE),4)
ans
####exact calculation
pnorm(.55,mean=.5,sd=sqrt(.5*.5/100),lower.tail = FALSE)

#q5
HitDay=520
pv <- ppois(15800 - 1, lambda = 520 * 30, lower.tail = FALSE)
pv
pnorm(15800 / 30, mean = 520, sd = sqrt(520 / 30), lower.tail = FALSE)

#q6 
m1 <- 10; m2 <- 11
n1 <- n2 <- 100
s <- 4
se <- s * sqrt(1 / n1 + 1 / n2)
ts <- (m2 - m1) / se
pv <- 2 * pnorm(-abs(ts))

#q7
pnorm(10 + qnorm(.95) * .4, mean = 11, sd = .4, lower.tail = FALSE)
Xmean<-10+qnorm(.95, lower.tail = FALSE)*.4
Xmean

#q8
pnorm(10 + qnorm(.95) * .4, mean = 11, sd = .4, lower.tail = FALSE)

# q9
n <- (qnorm(.95) + qnorm(.8)) ^ 2 * .04 ^ 2 / .01^2

# q10
mpg8 <- mtcars$mpg[mtcars$cyl == 8]
mpg6 <- mtcars$mpg[mtcars$cyl == 6]
m8 <- mean(mpg8); m6 <- mean(mpg6)
s8 <- sd(mpg8); s6 <- sd(mpg6)
n8 <- length(mpg8); n6 <- length(mpg6)


# q11
p <- t.test(mpg8, mpg6, paired = FALSE,
            alternative="two.sided", var.equal=TRUE)$p.value
mixprob <- (n8 - 1) / (n8 + n6 - 2)
s <- sqrt(mixprob * s8 ^ 2  +  (1 - mixprob) * s6 ^ 2)
ss <- sqrt(((n8-1) * s8 ^ 2  +  (n6-1) * s6 ^ 2)/(n8+n6-2))
z <- (m8 - m6) / (s * sqrt(1 / n8 + 1 / n6))
pz <- 2 * pnorm(-abs(z))
## Hand calculating the T just to check
#2 * pt(-abs(z), df = n8 + n6 - 2)






### question1
Baseline=c(140,138,150,148,135)
Week2=c(132,135,151,146,130)
summary(lm(Baseline ~ Week2))$coeff[2,4]
pValue

# TRUE
# Problem 1.
pharm <- data.frame(baseline = c(140, 138, 150, 148, 135), 
                    week2 = c(132, 135, 151, 146, 130))
t.test(pharm$baseline, pharm$week2,
       alternative = "two.sided", paired = T) # p-value = 0.08652


### question2
Xbar=1100;sdX=30;n=9
Xbar+c(-1,1)*qt(.975,8)*sdX/3


### question3
# coke vs Pepsi
# n=4
# Coke=3,4
choose(4,3)*.5^4+choose(4,4)*.5^4

### question4
# benchmark rate = 1 per 100
# benchmarked hospital => 10 per 1787

library(stats)
binom.test(x = 3, n = 4, p = .5, alt = "greater") # p-value = 0.3125

# question5
nx=9;ny=9;xbar=-3;ybar=1;stx=1.5;sty=1.8
sp <- sqrt( ((nx - 1) * stx^2 + (ny - 1) * sty^2) / (nx + ny - 2))
z <- (xbar - ybar) / (sp * sqrt(1 / nx + 1 / ny))
pz <- 2 * pnorm(-abs(z))
pz
# TRUE
p <- 1 / 100
pr <- 10 / 1787
n <- 1787
serror <- sqrt(p * (1-p) / n)
z <- (p-pr) / serror
pnorm(z, lower.tail = F) # 0.03066625

# question6
mu0=1078; mua=1078; 


# question7
pnorm( qnorm(.975) * .04, 
               mean = .01, sd = .04, lower.tail = FALSE)

# question8

power <- .9
power.t.test(power = power, delta = mu, sd = sd, type = "one.sample", alt = "one.sided")$n # 138.3856
