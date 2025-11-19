setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\developing data product\\week3")

############# R packages ##########
export("mvtsplot")
importFrom(graphics,  "Axis")
import(splines)

export("read.polyfile" , "write.polyfile" )
importFrom(graphics, plot)
exportClasses( "gpc.poly", "gpc.poly.nohole" )
exportMethods( "show", "get.bbox", "plot", "intersect?, " union?, "setdiff", 
               "[", "append.poly" , "scale.poly", "area.poly", "get.pts", 
               "coerce", "tristrip", "triangulate" )

\name{line}
\alias{line}
\alias{residuals.tukeyline }
\title{Robust Line Fitting }
\description{
        Fit a line robustly as recommended in  \emph{Exploratory Data Analysis }.
}

\usage{
        line(x, y)
}
\arguments{
        \item{x, y}{the arguments can be any way of specifying x-y pairs. See
                    \code{\link{xy.coords}}.}
}

\details{
        Cases with missing values are omitted.
        Long vectors are not supported.
}
\value{
        An object of class\code{"tukeyline"}.
        Methods are available for the generic functions\code{coef},
        \code{residuals}, \code{fitted}, and\code{print}.
}

\references{
        Tukey, J. W. (1977).
        \emph{Exploratory Data Analysis },
        Reading Massachusetts: Addison-Wesley.
}

system("R CMD build newpackage" )
system("R CMD check newpackage" )


########### S3 class
class(1)

class( TRUE )

class(rnorm(100 ))

class(NA)

class( "foo" )


x <- rnorm( 100 )
y <- x + rnorm( 100 )
fit <- lm(y ~ x)  ## linear regression model
class(fit)

mean

print

methods("mean" )

show

showMethods("show" )

######## S3 class/method  #####
set.seed(2)
x <- rnorm( 100 )
mean(x)

head(getS3method("mean", "default"), 10)
tail(getS3method("mean", "default"), 10)

set.seed(3)
df <- data.frame(x = rnorm( 100 ), y =  1:100 )
sapply(df, mean)

set.seed(10)
x <- rnorm(100)
plot(x)

#######class S3 example 2
set.seed(10)
x <- rnorm(100)
x <- as.ts(x)  ## Convert to a time series object 
plot(x)


###########class S4#########
library (methods)
setClass("polygon" ,
         representation(x="numeric" ,
                        y="numeric"))

setMethod ("plot", "polygon",
           function (x, y,  ...) {
                   plot(x@x, x@y, type =  "n" ,  ...)
                   xp <- c(x@x, x@x[ 1])
                   yp <- c(x@y, x@y[ 1])
                   lines(xp, yp)
           })

library (methods)
showMethods("plot")

p <- new( "polygon" , x = c( 1,  2, 3,  4), y = c( 1, 2,  3,  1))
plot(p)


################ yhat  #################
dir()
d<-read.csv("annual_all_2013.csv")
str(d)





