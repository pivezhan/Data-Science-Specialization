setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\r programming\\swirl")
swirl()
12
5+7
x<-5+7
x
y<-x-3
y
c(1.1,9,3.14)
z<-c(1.1,9,3.14)
?c
z
c(z,555,z)
z*2+100
my_sqrt<-sqrt(z-1)
my_sqrt
my_div<-z/my_sqrt
my_div
c(1,2,3,4)+c(0,10)
c(1,2,3,4)+c(0,10,100)
z*2+1000
my_div
getwd()
ls()
x<-9
ls()
list.files()
dir()
list.files()
info()
?list.files
args(list.files)
old.dir<-getwd()
dir.create("testdir")
file.create("mytest.R")
ls
list.files()
file.exists("mytest.R")
file.info("mytest.R")
file.info("mytest.R")$mode
file.rename("mytest.R","mytest2.R")
file.copy("mytest2.R","mytest3.R")
file.path()
file.path("mytest3.R")
?file.path
file.path
file.path("folder1", "folder2")
?dir.create
dir.create(file.path('testdir2', 'testdir3'), recursive = TRUE)
dir()
getwd()
unlink('testdir2', recursive = TRUE)
setwd(old.dir)
dir.delete("testdir")
dir()
unlink("testdir", recursive = TRUE)
1:20
pi:10
15:1
?`:`
seq(`:`)
seq(1,20)
seq(0,10,by=0.5)
seq(0,10,length=30)
my_seq<-seq(5,10,length=30)
length(my_seq)
1:length(my_seq)
seq(along.with = my_seq)
seq_along()
seq_along(my_seq)
rep(0,times=40)
rep(c(0, 1, 2), times = 10)
rep(c(0, 1, 2), each = 10)

getwd()
setwd("~/")
C:\Users\lenovo\Desktop\Rlearning
x<-list(foo=1:4,bar=0.6)
x$bar
save.image("C:/Users/lenovo/Desktop/a.RData")
x$foo
x[[1]][[3]]
x[[1]][[4]]
x[[2]][[1]]
x[[2]][[1]]
airquality(1:6,)
airquality[1:6,]
x<-airquality[1:6,]
View(x)
View(x)
View(x)
good=complete.cases(x)
x[good,][1:6,]
good
x
airquality[good,][1:6,]
x[good,][1:6,]
x
View(x)
View(x)
x[good,][1:6,]
View(airquality)
View(airquality)
dir()
y<-data.frames(a=1,b="a")
y<-data.frame(a=1,b="a")
dput(y)
dput(y,file="y.R")
new.y=dget("y.R")
new.y
y<-"foo"
x<-data.frame(a=1,b="a")
dump(c("x","y"),file="data.R")
rm(x,y)
source("data.R")
x
y
x<-factor(c("yes","no","yes","no","no"))
x
table(x)
unclass(x)
y
}
y
y
y[1]
clear
y
for i=1:10{
        if (a<3){
                y[i]<-1
        }else{
                y[i]<-0
        }
}
for i=1:10{
        if (a<3){
                y[i]<-1
        }else{
                y[i]<-0
        }
}
a++
        clear
a=0
for i=1:10{
        if (a<3){
                y[i]<-1
        }else{
                y[i]<-0
        }
        a++
}
a=0
for i=1:10{
        if (a<3){
                y[i]<-1
        }else{
                y[i]<-0
        }
        a<-a+1
}
x<-c("a","b","c","d","e")
fori=1:5{
        print(x[i])
}
a=0
for i=1 in 10{
        if (a<3){
                y[i]<-1
        }else{
                y[i]<-0
        }
        a<-a+1
}
x<-c("a","b","c","d","e")
fori=1 in 5{
        print(x[i])
}
a=0
for i in 1:10{
        if (a<3){
                y[i]<-1
        }else{
                y[i]<-0
        }
        a<-a+1
}
x<-c("a","b","c","d","e")
fori in 1:5{
        print(x[i])
}
x<-c("a","b","c","d","e")
for i in 1:5{
        print(x[i])
}
x<-c("a","b","c","d","e")
for (i in 1:5){
        print(x[i])
}
a=0
for (i in 1:10){
        if (a<3){
                y[i]<-1
        }else{
                y[i]<-0
        }
        print(y[i])
        a<-a+1
}
x<-matrix(1:6,2,3)
for (i in seq_len(nrow(x)))
        for (j in seq_len(ncol(x)))
                print(x[i,j])
end
end
x<-matrix(1:6,2,3)
for (i in seq_len(nrow(x))){
        for (j in seq_len(ncol(x))){
                print(x[i,j])
        }
}
x
x<-rbinom(1,1.4,.5)
x<-rbinom(1,1.4,0.5)
rbinom
z<-3
while(z<=5 && z>=2){
        print(z)
        if coin = rbinom(1,1,.5){
                z<-z+1
        }else{
                z<-z-1
        }
}
z<-3
while(z<=5 && z>=2){
        print(z)
        coin = rbinom(1,1,.5)
        if coin==1{
                z<-z+1
        }else{
                z<-z-1
        }
}
z<-3
while(z<=5 && z>=2){
        print(z)
        coin = rbinom(1,1,.5)
        if (coin==1){
                z<-z+1
        }else{
                z<-z-1
        }
}
add2(1,2)
add2(1,2)
add2(1,2)
add2 <- function(x,y){
        x+y
}
add2(2)
add2(2,1)
source('C:/Users/lenovo/Desktop/add2.R')
source('C:/Users/lenovo/Desktop/add2.R')
add2(1,2)
source('~/.active-rstudio-document')
x=c(10,20,3,9)
above(x,4)
above(x,10)
source('~/.active-rstudio-document')
columnmean(airquality)
columnmean(airquality)
columnmean(airquality)
source('C:/Users/lenovo/Desktop/add2.R')
columnmean(airquality)
source('C:/Users/lenovo/Desktop/add2.R')
columnmean(airquality)
mydata
mydata=rnorm(100)
source('~/.active-rstudio-document')
f(2,3)
args(paste)
args(cat)
?rnorm
a<-available.packages()
install.packages(KernSmooth)
install.packages("KernSmooth")
library(KernSmooth)
git commit
library(knitr)
source('~/.active-rstudio-document', echo=TRUE)
install.packages("C:/Users/lenovo/Desktop/pack/knitr_1.9.zip", repos = NULL)
install.packages("C:/Users/lenovo/Desktop/pack/knitrBootstrap_0.9.0.zip", repos = NULL)
rm(lis=ls)
rm(list=ls)
rm(list=ls())
install.packages("C:/Users/lenovo/Desktop/pack/markdown_0.7.4.zip", repos = NULL)
install.packages("C:/Users/lenovo/Desktop/pack/knitr_1.9.zip", repos = NULL)
install.packages("C:/Users/lenovo/Desktop/pack/knitrBootstrap_0.9.0.zip", repos = NULL)
install.packages("C:/Users/lenovo/Desktop/pack/rmarkdown_0.5.1.zip", repos = NULL)
install.packages("C:/Users/lenovo/Desktop/pack/roxygen2_4.1.1.zip", repos = NULL)
install.packages("C:/Users/lenovo/Desktop/pack/devtools_1.7.0.zip", repos = NULL)
library(UsingR)
library(devtools)
install_github('slidify','ramnathv')
install_github('slidifyLibraries','ramnathv')
library(slidify)
install.packages("swirl")
library(swirl)
swirl()
q()
setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\r programming\\swirl")
library(swirl)
swirl()
1:20
my_vector<-1:20
my_vector
dim(my_vector)
length(my_vector)
dim(my_vector)<-c(4,5)
dim(my_vector)
attributes(my_vector)
my_vector
class(my_vector)
my_matrix<-my_vector
?matrix
my_matrix2<-matrix(1:20,4,5)
identical(my_matrix,my_matrix2)
patients<-c( "Bill", "Gina", "Kelly", "Sean")
cbind(patients,my_matrix)
my_data <- data.frame(patients, my_matrix)
my_data
class(my_data)
cnames<-c("patient", "age", "weight", "bp", "rating", "test")
colnames(my_data)<-cnames
my_data
TRUE==TRUE
(F==TRUE)==F
(FALSE == TRUE) == FALSE
6==7
6<7
10<=10
5!=7
! 5==7
FALSE & FALSE
TRUE & c(TRUE, FALSE, FALSE)
c(TRUE, TRUE, TRUE) & c(TRUE, FALSE, FALSE)
TRUE && c(TRUE, FALSE, FALSE)
TRUE | c(TRUE, FALSE, FALSE)
TRUE || c(TRUE, FALSE, FALSE)
5 > 8 || 6 != 8 && 4 > 3.9
isTRUE(6>4)
identical('twins', 'twins')
xor(5 == 6,!FALSE)
ints<-sample(10)
ints
ints>5
which(ints>7)
any(ints<0)
all(ints>0)
Sys.Date()
mean(c(2,4,5))
submit()
boring_function('My first function!')
boring_function
submit()
my_mean(c(4, 5, 10))
submit()
submit()
reminder(5)
remainder(5)
remainder(11,5)
remainder(divisor = 11, num = 5)
reminder(4,div=2)
remainder(4,div=2)
remainder(div=2,4)
args(remainder)
submit
submit()
floor(1.2)
evaluate(sum, c(2, 4, 6))
source('C:/Users/lenovo/AppData/Local/Temp/RtmpKQB6h1/evaluate.R')
source('C:/Users/lenovo/AppData/Local/Temp/RtmpKQB6h1/evaluate.R')
evaluate(sum, c(2, 4, 6))
evaluate(sum, c(2, 4, 6))
func
source('C:/Users/lenovo/AppData/Local/Temp/RtmpKQB6h1/evaluate.R')
evaluate(sum, c(2, 4, 6))
evaluate(sum, c(2, 4, 6))
source('C:/Users/lenovo/AppData/Local/Temp/RtmpKQB6h1/evaluate.R')
evaluate(sum, c(2, 4, 6))
args(evaluate)
source('C:/Users/lenovo/AppData/Local/Temp/RtmpKQB6h1/evaluate.R')
source('C:/Users/lenovo/AppData/Local/Temp/RtmpKQB6h1/evaluate.R')
args(evaluate)
evaluate(sum, c(2, 4, 6))
as.character(sum)
submit()
source('C:/Users/lenovo/AppData/Local/Temp/RtmpKQB6h1/evaluate.R')
submit()
evaluate(sd,c(1.4, 3.6, 7.9, 8.8))
evaluate(function(x){x+1}, 6)
evaluate(function(x){x[1]},c(8,4,0))
evaluate(function(x){x[end]},c(8,4,0))
evaluate(function(x){tail(x, n = 1)},c(8,4,0))
?paste
paste("Programming", "is", "fun!")
source('C:/Users/lenovo/AppData/Local/Temp/RtmpKQB6h1/telegram.R')
submit()
submit()
telegram("at","the")
submit()
place
submit()
source('C:/Users/lenovo/AppData/Local/Temp/RtmpKQB6h1/mad_libs.R')
mad_libs("aa")
submit()
source('C:/Users/lenovo/AppData/Local/Temp/RtmpKQB6h1/mad_libs.R')
submit()
mad_libs("aa")
submit()
mad_libs(I,doing,mhammad)
mad_libs("I","doing","mohammad")
submit()
source('C:/Users/lenovo/AppData/Local/Temp/RtmpKQB6h1/bin_op.R')
"Good" %p% "job!"
submit()
'I' %p% 'love' %p% 'R!'
head(flags)
dim(flags)
class(flags)
cls_list <- lapply(flags, class)
cls_list
class(cls_list)
as.character(cls_list)
cls_vect<-sapply(flags,class)
class(cls_vect)
sum(flags$orange)
flags$orange
flag_colors <- flags[, 11:17]
head(flag_colors)
lapply(flag_colors,sum)
sapply(flag_colors,sum)
sapply(flag_colors,mean)
flag_shapes <- flags[, 19:23]
lapply(flag_shapes,range)
shape_mat<-sapply(flag_shapes,range)
shape_mat
class(shape_mat)
unique(c(3, 4, 5, 5, 5, 6, 6))
unique_vals<-lapply(flags,unique)
unique_vals
length(unique_vals)
lapply(unique_vals,length)
sapply(unique_vals,length)
sapply(flags,unique)
lapply(unique_vals, function(elem) elem[2])
sapply(flags, unique)
vapply(flags, unique, numeric(1))
ok()
vapply(flags, class)
sapply(flags, class)
vapply(flags, class, character(1))
?tapply
table(flags$landmass)
table(flags$animate)
tapply(flags$animate, flags$landmass, mean)
tapply(flags$population, flags$red, summary)
tapply(flags$population, flags$landmass, summary)
ls()
class(plants)
dim(plants)
nrow(plants)
ncol(plants)
object.size(plants)
names(plants)
head(plants)
head(plants)
head(plants, 10)
tail(plants)
tail(plants,15)
summary(plants)
table(plants$Active_Growth_Period)
str(plants)
?sample
sample(1:6, 4, replace = TRUE)
sample(1:6, 4, replace = TRUE)
sample(1:20, 10, replace = F)
sample(1:20, 10, replace = FALSE)
sample(1:20, 10)
LETTERS
sample(LETTERS)
flips<-prob = c(0.3, 0.7)
flips<-sample(1:2,prob = c(0.3, 0.7))
flips<-sample(c(0,1),100,prob = c(0.3, 0.7))
flips <- sample(c(0,1), 100, replace = TRUE, prob = c(0.3, 0.7))
flips
sum(flips)
?rbinom
rbinom(1, size = 100, prob = 0.7)
flip2<-rbinom(1, size = 100, prob = 0.7)
flips2<-rbinom(1, size = 100, prob = 0.7)
flips2<-rbinom(100, size = 1, prob = 0.7)
flips
flip2
flips2
sum(flips)
sum(flips2)
?rnorm
rnorm(10)
rnorm(10,mean=100,sd=25)
rpois(5,lambda=10)
replicate(100, rpois(5, 10))
my_pois <- replicate(100, rpois(5, 10))
my_pois
colMean(my_pois)
cm<-colMeans(my_pois)
hist(cm)
d1<-Sys.Date()
class()
class(d1)
unclass(d1)
d1
as.Date("1969-01-01")
d2<-as.Date("1969-01-01")
unclass(d2)
t1<-Sys.times()
t1<-Sys.time()
t1
class(t1)
unclass(t1)
t2<-as.POSIXlt(Sys.time())
class(t2)
t2
unclass(t2)
str(unclass(t2))
t2$min
weekday(t1)
weekday(d1)
weekdays(d1)
months(t1)
quarters(t2)
t3<- "October 17, 1986 08:24"
strptime(t3, "%B %d, %Y %H:%M")
<-t4strptime(t3, "%B %d, %Y %H:%M")
t4<-strptime(t3, "%B %d, %Y %H:%M")
t4
class(t4)
Sys.time() > t1
Sys.time() - t1
difftime(Sys.time(), t1, units = 'days')
data(cars)
?cars
head(cars)
plot(cars)
?plot9
?plot()
?plot
plot(x = cars$speed, y = cars$dist)
plot(y = cars$speed, x = cars$dist)
plot(y = cars$speed, x = cars$dist,xlabel="speed")
?plot
plot(y = cars$speed, x = cars$dist,xlab="speed")
plot(y = cars$speed, x = cars$dist,xlab="speed")
plot(x = cars$speed, y = cars$dist,xlab="speed")
plot(x = cars$speed, y = cars$dist, xlab = "Speed")
plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance")
plot(x = cars$speed, y = cars$dist, ylab = "Stopping Distance")
plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance")
plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance",main="My Plt")
plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance",main="My Plot")
plot(x = cars$speed, y = cars$dist, main="My Plot")
plot(cars, main="My Plot")
plot(cars, sub="My Plot Subtitle")
plot(cars, col = 2)
plot(cars, xlim=c(10,15))
plot(cars, pch=2)
data(mtcars)
?boxplot
boxplot(formula = mpg ~ cyl)
boxplot(formula = mpg ~ cyl,data=mtcars)
hist(formula = mpg ~ cyl,data=mtcars)
hist(mpg,data=mtcars)
hist(mtcars$mpg)
savehistory("C:/Users/lenovo/Desktop/Rlearning/r programming/swirl/2.Rhistory")
