setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\exploratory data analysis\\week1")

###ab=nual average
pollution <- read.csv( "avgpm25.csv" , colClasses = c("numeric", "character","factor", "numeric", "numeric"))
head(pollution)

## five number summary
summary(pollution$pm25)

##boxplot
boxplot(pollution$pm25, col = "blue")

##histogram
hist(pollution$pm25, col = "green")
rug(pollution$pm25)

hist(pollution$pm25, col = "green",100)

###overlaying features
boxplot(pollution$pm25, col = "blue")
abline(h = 12)

hist(pollution$pm25, col = "green")
abline(v = 12, lwd = 2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)

##barplot
barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region" )

##multiple histogram
par(mfrow = c( 2, 1), mar = c(4, 4, 2, 1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

##scatter plot
with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)

##scatterplot
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)

##multiple scatter plot
par(mfrow = c( 1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))


###############plotting systems in R

##base plot
library(datasets)
data(cars)
with(cars, plot(speed, dist))

## lattice plot
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c( 4, 1))

##ggplot2 plot
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)


library(datasets)
with(airquality, plot(Wind, Ozone))

###transform into the  factor variable
library(datasets)
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)" )


par("lty")
## [1] "solid"
par("col")
## [1] "black"
par("pch")
## [1] 1
par("bg")
## [1] "transparent"
par("mar")
## [1] 5.1 4.1 4.1 2.1
par("mfrow")
## [1] 1 1

#####base plot with anotation
library(datasets)
with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City" ) ## Add a title

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City" ))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

with(airquality, plot(Wind, Ozone, 
                      main = "Ozone and Wind in New York City" ,type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1,
       col = c("blue", "red"), legend = c( "May", "Other Months" ))

with(airquality, plot(Wind, Ozone,main = "Ozone and Wind in New York City" ,pch = 19))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

par(mfrow = c( 1, 2))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind" )
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation" )
})

par(mfrow = c( 1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind" )
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation" )
  plot(Temp, Ozone, main = "Ozone and Temperature" )
  mtext("Ozone and Weather in New York City" , outer = TRUE)
})

##################graphics Devices

##how does plot get created
library(datasets)
with(faithful, plot(eruptions, waiting)) ## Make plot appear on screen device
title(main = "Old Faithful Geyser data" ) ## Annotate with a title


pdf(file = "myplot.pdf") ## Open PDF device; create 'myplot.pdf' in my working directory
## Create plot and send to a file (no plot appears on screen)
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data" ) ## Annotate plot; still nothing on screen
dev.off() ## Close the PDF file device
## Now you can view the file 'myplot.pdf' on your computer

####coppying plots in png format
library(datasets)
with(faithful, plot(eruptions, waiting)) ## Create plot on screen device
title(main = "Old Faithful Geyser data" ) ## Add a main title
dev.copy(png, file = "geyserplot.png" ) ## Copy my plot to a PNG file
dev.copy2pdf(out.type = "pdf", file="geyserplot.pdf")
dev.off() ## Don't forget to close the PNG device!

# trainning
x=rnorm(100)
y=rnorm(100)
fit<-lm(x~y)
abline(fit)
abline(fit,lwd=3,col="red")
z<-rpois(100)
plot(x,y,xlab="Weight",ylab="length",main="Scatterplot",pch=20)
legend("topright",legend(Data),pch=20)
abline(fit,lwd=3,col="red")
par(mfrow=c(2,1))
par("mar")
 plot(x,y,pch=20)
 plot(x,y,pch=19)
example(points)
par(mar=c(4,4,4,4))
g<-gl(2,50,label=c("Male","Female"))  ##grouping plot
str(g)
plot(x,y,type="n")
points(x[g=="Male"],y[g=="Male"],col="green")
points(x[g=="Female"],y[g=="Female"],col="blue")
points(x[g=="Female"],y[g=="Female"],col="blue",pch=19)
#end trainning




