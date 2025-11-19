setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\exploratory data analysis\\week4")

load("samsungData.rda")
names(samsungData)[1:12]

table(samsungData$activity)

# plotting average acceleration
par(mfrow = c( 1, 2), mar = c(5, 4, 1, 1))
samsungData <- transform(samsungData, activity = factor(activity))
sub1 <- subset(samsungData, subject == 1)
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[ 1])
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[ 2])
legend("bottomright" , legend = unique(sub1$activity), col = unique(sub1$activity),
       pch = 1)

###clustering based on maximum acceleration
source("myplclust.R" )
distanceMatrix <- dist(sub1[, 1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

##maximum acceleration
par(mfrow = c( 1, 2))
plot(sub1[, 10], pch = 19, col = sub1$activity, ylab = names(sub1)[ 10])
plot(sub1[, 11], pch = 19, col = sub1$activity, ylab = names(sub1)[ 11])


source("myplclust.R" )
distanceMatrix <- dist(sub1[, 10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

svd1 = svd(scale(sub1[, -c(562, 563)]))
par(mfrow = c( 1, 2))
plot(svd1$u[, 1], col = sub1$activity, pch = 19)
plot(svd1$u[, 2], col = sub1$activity, pch = 19)

plot(svd1$v[, 2], pch = 19)

maxContrib <- which.max(svd1$v[, 2])
distanceMatrix <- dist(sub1[, c( 10:12, maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

names(samsungData)[maxContrib]

####k-mean clustering
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6)
table(kClust$cluster, sub1$activity)

kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 1)
table(kClust$cluster, sub1$activity)

kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity)

kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity)

plot(kClust$center[1, 1:10], pch = 19, ylab = "Cluster Center" , xlab = "")

plot(kClust$center[4, 1:10], pch = 19, ylab = "Cluster Center" , xlab = "")




#################### case study ############################
# rm(list=ls())
load("samsungData.rda")
load("exploratoyDataAnalysis.R")
load("face.rda")
source("myplclust.R" )

RD_501_88101_1999 = read.table("RD_501_88101_1999-0.txt")
RD_501_88101_2012 = read.table("RD_501_88101_2012-0.txt")

RD_501_88101_1999
grep("|",RD_501_88101_1999)
RD_501_88101_1999

pm0 = read.table("RD_501_88101_1999-0.txt",comment.char="#"
                               ,header=FALSE, sep="|", na.string="")
pm1 = read.table("RD_501_88101_2012-0.txt",comment.char="#"
                 ,header=FALSE, sep="|", na.string="")
dim(pm0)
head(pm0)

##column names comes with this code
cnames<-readLines("RD_501_88101_1999-0.txt",1)
cnames<-strsplit(cnames,"|",fixed=T)
names(pm0)=cnames[[1]]
head(pm0)

##repair all column names with replace spaces with dots
names(pm0)<-make.names(cnames[[1]])
head(pm0)

x0<-pm0$Sample.Value
class(x0)
str(x0)
summary(x0)
mean(is.na(x0))

#########################pm1 analysis
dim(pm1)
names(pm1)<-make.names(cnames[[1]])
head(pm1)

x1<-pm1$Sample.Value
str(x1)
summary(x0)
summary(x1)
mean(is.na(x1))

boxplot(x0,x1)
boxplot(log10(x0),log10(x1))

summary(x1)
negative<-x1<0
str(negative)
sum(negative,na.rm=TRUE)
mean(negative,na.rm=TRUE)###take the proportion of negative values
dates<-pm1$Date
str(dates)
dates<-as.Date(as.character(dates), "%Y%m%d")
str(dates)
hist(dates,"month")
hist(dates[negative],"month")

site0<-unique(subset(pm0,State.Code==36,c(County.Code,Site.ID)))
site1<-unique(subset(pm1,State.Code==36,c(County.Code,Site.ID)))
site0<-paste(site0[,1],site0[,2],sep=".")
site1<-paste(site1[,1],site1[,2],sep=".")
str(site0)
str(site1)
both<-intersect(site0,site1)
both
###how many of observation
pm0$County.Site<-with(pm0,paste(County.Code,Site.ID,sep="."))
pm1$County.Site<-with(pm1,paste(County.Code,Site.ID,sep="."))
cnt0<-subset(pm0,State.Code==36 & County.Site %in% both)
cnt1<-subset(pm1,State.Code==36 & County.Site %in% both)
head(cnt0)
##split data frame
split(cnt0,cnt0$County.Site)
sapply(split(cnt0,cnt0$County.Site),nrow)
sapply(split(cnt1,cnt1$County.Site),nrow)

pm1sub<-subset(pm1,State.Code==36&County.Code==63&Site.ID==2008)
pm0sub<-subset(pm0,State.Code==36&County.Code==63&Site.ID==2008)
dim(pm1sub)
dim(pm0sub)

dates1<-pm1sub$Date
x1sub<-pm1sub$Sample.Value
plot(dates1,x1sub)
dates1<-as.Date(as.character(dates1),"%Y%m%d")
str(dates1)
plot(dates1,x1sub)

dates0<-pm0sub$Date
dates0<-as.Date(as.character(dates0),"%Y%m%d")
x0sub<-pm0sub$Sample.Value
str(dates0)
plot(dates0,x0sub)

par(mfrow=c(1,2),mar=c(4,4,2,1))
plot(dates0,x0sub,pch=20)
abline(h=median(x0sub,na.rm=T))
plot(dates1,x1sub,pch=20)
abline(h=median(x1sub,na.rm=T))

rng<-range(x0sub,x1sub,na.rm=T)
par(mfrow=c(1,2),mar=c(4,4,2,1))
plot(dates0,x0sub,pch=20,ylim=rng)
abline(h=median(x0sub,na.rm=T))
plot(dates1,x1sub,pch=20,ylim=rng)
abline(h=median(x1sub,na.rm=T))

###taking average of sample values by state code
head(pm0)
mn0<-with(pm0,tapply(Sample.Value,State.Code,mean,na.rm=T))
mn1<-with(pm1,tapply(Sample.Value,State.Code,mean,na.rm=T))
str(mn0)
summary(mn0)
summary(mn1)

d0<-data.frame(state=names(mn0),mean=mn0)
d1<-data.frame(state=names(mn1),mean=mn1)

head(d0)
head(d1)

mrg<-merge(d0,d1,by="state")
dim(mrg)
str(mrg)
head(mrg)

par(mfrow=c(1,1))
with(mrg,plot(rep(1999,52),mrg[,2],xlim=c(1998,2013)))
with(mrg,points(rep(2012,52),mrg[,3]))

segments(rep(1999,52),mrg[,2],rep(2012,52),mrg[,3])

