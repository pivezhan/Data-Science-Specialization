setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\r programming\\week2")

mydata <- rnorm( 100)
sd(mydata)
sd(x = mydata)
sd(x = mydata, na.rm = FALSE)
sd(na.rm = FALSE, x = mydata)
sd(na.rm = FALSE, mydata)

##argument maching
args(lm)

lm(data = mydata, y ~ x, model = FALSE, 1:100)
lm(y ~ x, mydata, 1:100, model = FALSE)

###definning a function

f <- function(a, b = 1, c = 2, d = NULL) {
}


##lazy evaluation

f <- function(a, b) {
        a^2
}
f(2)

f <- function(a, b) {
        print(a)
        print(b)
}
f(45)


###the ... argument
myplot <- function(x, y, type = "l", ...) {
        plot(x, y, type = type, ...)
}

args(cat)



##dates in R
x <- as.Date( "1970-01-01")
unclass(x)
unclass(as.Date( "1970-01-02"))

x <- Sys.time()

p <- as.POSIXlt(x)
names(unclass(p))
p$sec

datestring <- c( "January 10, 2012 10:40" , "December 9", 2011)
x <- strptime(datestring, "%B %d, %Y %H:%M")
x
class(x)


x <- as.Date( "2012-01-01")
y <- strptime( "9 Jan 2011 11:34:21" , "%d %b %Y %H:%M:%S" )
x-y

x <- as.POSIXct( "2012-10-25 01:00:00" )
y <- as.POSIXct( "2012-10-25 06:00:00" , tz = "GMT")
y-x

###diversion on binding values to symbol
lm <- function(x) { x * x }

function(x) { x * x }
search()

f <- function(x, y) {
        x^2+ y / z
}

##lexical scopping
make.power <- function(n) {
        pow <- function(x) {
                x^n
        }
        pow
}

cube <- make.power( 3)
square <- make.power( 2)
cube(3)

ls(environment(cube))
get("n", environment(cube))
ls(environment(square))
get("n", environment(square))

##lexical vs dynamic scoping
y <- 10
f <- function(x) {
        y <- 2
        y^2+ g(x)
}
g <- function(x) {
        x*y
}

##optimization()maximizing a normal liklihood
make.NegLogLik <- function(data, fixed=c( FALSE,FALSE)) {
        params <- fixed
        function(p) {
                params[!fixed] <- p
                mu <- params[ 1]
                sigma <- params[ 2]
                a <- - 0.5*length(data)*log( 2*pi*sigma^2)
                b <- - 0.5*sum((data-mu)^ 2) / (sigma^2)
                -(a + b)
        }
}

set.seed(1); normals <- rnorm( 100, 1, 2)
nLL <- make.NegLogLik(normals)
nLL
ls(environment(nLL))

mysummary <- function(x,npar=TRUE,print=TRUE) {
        if (!npar) {
                center <- mean(x); spread <- sd(x)
        } else {
                center <- median(x); spread <- mad(x)
        }
        if (print & !npar) {
                cat("Mean=", center, "\n", "SD=", spread, "\n")
        } else if (print & npar) {
                cat("Median=", center, "\n", "MAD=", spread, "\n")
        }
        result <- list(center=center,spread=spread)
        return(result)
}
