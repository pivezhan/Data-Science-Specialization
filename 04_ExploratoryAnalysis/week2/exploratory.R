library(lattice)
library(datasets)
## Simple scatterplot
xyplot(Ozone ~ Wind, data = airquality)

library(datasets)
library(lattice)
## Convert 'Month' to a factor variable
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c( 5, 1))
 

p <- xyplot(Ozone ~ Wind, data = airquality) ## Nothing happens!
print(p) ## Plot appears


set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c( "Group 1", "Group 2"))
xyplot(y ~ x | f, layout = c( 2, 1)) ## Plot with 2 panels


## Custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...) {
        panel.xyplot(x, y, ...) ## First call the default panel function for 'xyplot'
        panel.abline(h = median(y), lty = 2) ## Add a horizontal line at the median
})


## Custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...) {
        panel.xyplot(x, y, ...) ## First call default panel function
        panel.lmline(x, y, col = 2) ## Overlay a simple linear regression line
})


rm(list=ls())
library(ggplot2)
qplot(displ, hwy,data=mpg,color	=drv)	
qplot(displ,hwy,data=mpg,geom=c("point","smooth"))
qplot(hwy,data=mpg,fill=drv)
qplot(displ,hwy,data=mpg,facets=.~drv)
qplot(hwy,data=mpg,facets=drv~.,binwidth=2)

library(datasets)
qplot(log(eno),data=mpg)
qplot(log(eno),data=mpg,fill=mopos)
qplot(log(eno),data=mpg,geom="density")
qplot(log(eno),data=mpg,geom="density",color=mopos)
qplot(log(pm25),log(eno),data=mpg)
qplot(log(pm25),log(eno),data=mpg,shape=mopos)
qplot(log(pm25),log(eno),data=mpg,color=mopos)
qplot(log(pm25),log(eno),data=mpg,color=mopos,geom=c("point","smooth"),method="lm")
qplot(log(pm25),log(eno),data=mpg,geom=c("point","smooth"),method="lm",facets=.~mopos)
qplot(logpm25, NocturnalSympt, data = mpg, facets = . ~ bmicat, geom =c("point", "smooth"), method = "lm")
g <- ggplot(mpg, aes(logpm25, NocturnalSympt))!
        g + geom_point()
g + geom_point() + geom_smooth(method = "lm")!
g + geom_point() + geom_smooth()!

g + geom_point() + facet_grid(. ~ bmicat) + geom_smooth(method = "lm")

g + geom_point(color = "steelblue", size = 4, alpha = 1/2)
g + geom_point(aes(color = bmicat), size = 4, alpha = 1/2)

g + geom_point(aes(color = bmicat)) + labs(title = "mpg Cohort") + labs(x = expression("log"* PM[2.5]), y = "Nocturnal Symptoms")

g + geom_point(aes(color = bmicat), size = 2, alpha = 1/2) + 
        geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE)!

g + geom_point(aes(color = bmicat)) + theme_bw(base_family = "Times")
testdat <- data.frame(x = 1:100, y = rnorm(100))!
testdat[50,2] <- 100 ## Outlier!!
plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3))

g <- ggplot(testdat, aes(x = x, y = y))!
        g + geom_line()

g + geom_line() + ylim(-3, 3)

g + geom_line() + coord_cartesian(ylim = c(-3, 3))!

        head(mpg)
        mpg$cyl<-factor(mpg$cyl)

p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, group=Chick)) +
        geom_line() +
        ggtitle("Growth curve for individual chicks")
print(p1)

g <- ggplot(mpg, aes(displ, hwy))
g + geom_point(alpha = 1/2) 
+ facet_wrap(.~drv, nrow = 3, ncol = 1) 
+ geom_smooth(method="lm", se=FALSE, col="steelblue")      
+ theme_bw(base_family = "Avenir", base_size = 10) 
+ labs(x = expression("log " * PM[2.5]) 
+ labs(y = "Nocturnal Symptoms") 
+ labs(title = "MAACS Cohort")
       