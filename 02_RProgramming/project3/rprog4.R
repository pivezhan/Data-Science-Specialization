setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\r programming\\week4")

str(str)
str(lm)
str(ls)
x<-rnorm(100,2,4)
str(x)
summary(x)
f<-gl(40,10)
str(f)
summary(f)
library(datasets)
str(airquality)
m<-matrix(rnorm(100),10,10)

str(m)
m[,1]


s<-split(airquality,airquality$Month)
str(s)

#################simulation
# lm(y~x)
 sample.interval=10000
Rprof(ls)


# quiz4
set.seed(1)
rpois(5, 2)

library(datasets)
data(mtcars)
head(mtcars)
y<-mtcars$mpg
x1<-mtcars$cyl
x2<-mtcars$disp

Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)
summaryRprof()$by.self
summaryRprof()$by.total


