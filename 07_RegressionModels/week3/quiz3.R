##quiz3

# question1
library(datasets); data(mtcars);
head(mtcars)
summary(lm(mpg ~ as.factor(cyl)+wt, data=mtcars))$coef

##question2
summary(lm(mpg ~ as.factor(cyl)+wt, data=mtcars))$coef
summary(lm(mpg ~ as.factor(cyl), data=mtcars))$coef

##question3
library(datasets); data(mtcars);
lmboth1<-lm(mtcars$mpg ~ as.factor(mtcars$cyl)*mtcars$wt)
summary(lmboth1)
summary(lm(mpg ~ as.factor(cyl)*wt, data=mtcars))$coef

# question4
lmboth<-lm(mtcars$mpg ~ I(mtcars$wt * 0.5) + factor(mtcars$cyl))
summary(lmboth)
# I(mtcars$wt * 0.5)
# mtcars$wt

# question5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit1<-lm(y~x)
infl<-lm.influence(fit1)$hat
max(infl)


# Question 6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y~x)
lm.influence(fit)$hat
dfbetas(fit)

##question7

