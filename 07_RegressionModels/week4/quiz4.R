# question1

library(MASS)
data(shuttle)
head(shuttle)
logRegRavens <- glm(shuttle$use ~ shuttle$sign+factor(shuttle$wind)
                    ,family= "binomial")
# logRegRavens <- glm(use ~ sign+wind,data=shuttle,family= "binomial")
summary(logRegRavens)

# TRUE answer
# method 1
library(MASS)
data(shuttle)
## Make our own variables just for illustration
shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind, data = shuttle, family = binomial)
exp(coef(fit))

# method 2
## Another way without redifing variables
fit <- glm(relevel(use, "noauto") ~ relevel(wind, "tail"), data = shuttle, family = binomial)
exp(coef(fit))

##question2
logRegRavens <- glm(shuttle$use ~ shuttle$wind
                    + shuttle$magn,family= "binomial")
summary(logRegRavens)

# TRUE answer
# method 1
shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind + magn, data = shuttle, family = binomial)
exp(coef(fit))
# method 2
## Another way without redifing variables
fit <- glm(relevel(use, "noauto") ~ relevel(wind, "tail") + magn, data = shuttle, 
           family = binomial)
exp(coef(fit))

##question 4
library(datasets)
data(InsectSprays)
head(InsectSprays)
glm1 <- glm( InsectSprays$count ~factor(InsectSprays$spray),family= "poisson")
summary(glm1)

# TRUE answer
fit <- glm(count ~ relevel(spray, "B"), data = InsectSprays, family = poisson)
exp(coef(fit))[2]


### question5

glm(count ~ x + offset(t), family = poisson)
t2 <- log(10) + t
glm(count ~ x + offset(t2), family = poisson)

#####question6
ramp<-function(x,y){
        r<-matrix(rep(0,length(x)),1:length(x))
        for (i in (length(x)-1):1){
                r[i]<-(y[i+1]-y[i])/(x[i+1]-x[i])
        }
        r
}
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knots <- seq( -5,5 , length = 3);
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind( 1, x,splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
# plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
# lines(x, yhat, col="red", lwd = 2)
ramp(x,yhat)


# simple answer
z <- (x > 0) * x
fit <- lm(y ~ x + z)
sum(coef(fit)[2:3])