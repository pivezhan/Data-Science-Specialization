###Homework3
#question1
library(datasets)
data(mtcars)
head(mtcars)
summary(mtcars)
mean(mtcars$mpg)+c(-1,1)*qnorm(0.975)*sd(mtcars$mpg)
# head(mtcars$mpg)
# tail(mtcars$mpg)
# length(mtcars$mpg)
##TRUE ANSWER
round(t.test(mtcars$mpg)$conf.int)

#question2
qt(0.975, 8)*1/3
##TRUE ANSWER
round(qt(.975, df = 8) * 1 / 3, 2)

#question3
# The observations between the groups are naturally assumed to be 
# statistically independent

#question4
g1 <- mtcars$mpg[(mtcars$cyl==4),]; g2 <- mtcars$mpg[(mtcars$cyl==6),]
difference <- g2 - g1

m4 <- mtcars$mpg[mtcars$cyl == 4]
m6 <- mtcars$mpg[mtcars$cyl == 6]
#this does 4 - 6
confint <- as.vector(t.test(m4, m6, var.equal = TRUE)$conf.int)
confint



# quiz3 
# question 1
1100+(c(-1,1)*qt(0.975,df=9-1)*30/sqrt(9))

#question2
# mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
(2*sqrt(9))/qt(.975, 9-1)


#question4
sp <- sqrt(((10-1) * .6^2 + (10-1)*.68^2) / (10+10-2))
3 - 5 + c(-1, 1) * qt(.975, (10+10-2)) * sp * (1/10 + 1/10)^.5

#question5
md=6-4
sdd=2-.5
n=10

md+c(-1,1)*qt(0.975,99)*sdd
sdd
md

# question7
sp <- sqrt(((9-1) * 1.8^2 + (9-1) * 1.5^2) / (9+9-2))
-3 - 1 + c(-1, 1) * qt(.95, (9+9-2)) * sp * (1/9 + 1/9)^.5





