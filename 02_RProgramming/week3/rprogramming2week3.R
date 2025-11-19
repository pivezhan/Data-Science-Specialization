log(-1)

printmessage <- function(x) {
        if(x > 0)
                print("x is greater than zero")
        else
                print("x is less than or equal to zero")
        invisible(x)
}


printmessage <- function(x) {
        if(x > 0) 
                print("x is greater than zero") elseprint("x is less than or equal to zero")
        invisible(x)
}
printmessage(1)

printmessage(NA)

printmessage2 <- function(x) {
        if(is.na(x)) 
                print("x is a missing value!") 
        else if(x > 0) 
                print("x is greater than zero") 
        elseprint("x is less than or equal to zero")
        invisible(x)
}
x <- log(-1)

printmessage2(x)

###traceback
mean(x)
# Error inmean(x) : object 'x'not found
traceback()

lm(y ~ x)
# Error ineval(expr, envir, enclos) : object ’y’ not found
traceback()


##debug
debug(lm)
lm(y ~ x)

n
##recover
options(error = recover)
read.csv("nosuchfile")

###quiz3
library(datasets)
# data(iris)
s <- split(iris,iris$Species)
g<-sapply(s, function(x) colMeans(x[, c("Sepal.Length","Petal.Width",
                                     "Petal.Length")],na.rm = TRUE))

###quiz3 
library(datasets)
data(mtcars)
# ?mtcars
h<-tapply(mtcars$hp, mtcars$cyl, mean) 
