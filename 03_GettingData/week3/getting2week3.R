setwd("C:/Users/lenovo/Desktop/Rlearning/Getting and Cleaning Data/week3")

##creating new variables
if(!file.exists( "./data")){dir.create( "./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile= "./data/restaurants.csv")
restData <- read.csv( "./data/restaurants.csv" )

##creating sequences
s1 <- seq(1,10,by=3) ; s1
s2 <- seq(1,10,length=3); s2
x <- c(1,3,8,25,100); seq(along = x)

##subsetting variables
restData$nearMe = restData$neighborhood %in% c("Roland Park", "Homeland" )
table(restData$nearMe)
restData$zipWrong = ifelse(restData$zipCode < 0, TRUE, FALSE)
table(restData$zipWrong,restData$zipCode < 0)

###creating categorical variables
restData$zipGroups = cut(restData$zipCode,breaks=quantile(restData$zipCode))
table(restData$zipGroups)
table(restData$zipGroups,restData$zipCode)

##creating factor variable by cut2
library(Hmisc)
restData$zipGroups = cut2(restData$zipCode,g= 4)
table(restData$zipGroups)
restData$zcf <- factor(restData$zipCode)
restData$zcf[ 1:10]
class(restData$zcf)

##levels of factor variables
yesno <- sample(c( "yes","no"),size=10,replace=TRUE) 

##repeatedly creation of yes or no values
yesnofac = factor(yesno,levels=c( "yes","no"))
relevel(yesnofac,ref= "yes")
as.numeric(yesnofac)

##cutting produces factor variables 
library(Hmisc)
restData$zipGroups = cut2(restData$zipCode,g= 4)
table(restData$zipGroups)

##using the mutation function
library(Hmisc); library(plyr)
restData2 = mutate(restData,zipGroups=cut2(zipCode,g= 4))
table(restData2$zipGroups)

#####reshaping data
library(reshape2)
head(mtcars)

##melting data frames
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars,id=c( "carname","gear","cyl"),
                measure.vars=c( "mpg","hp"))
head(carMelt,n= 3)
tail(carMelt,n= 3)

##casting data frames
cylData <- dcast(carMelt, cyl ~ variable)
cylData
cylData <- dcast(carMelt, cyl ~ variable,mean)
cylData
head(InsectSprays)

##averaging values
tapply(InsectSprays$count,InsectSprays$spray,sum)

##another way by splitting
spIns = split(InsectSprays$count,InsectSprays$spray)
spIns

## take apply for summation
sprCount = lapply(spIns,sum)
sprCount

##combining
unlist(sprCount)
sapply(spIns,sum)

##plyr package
ddply(InsectSprays,.(spray),summarize,sum=sum(count))##
spraySums <- ddply(InsectSprays,.(spray),summarize,
                   sum=ave(count,FUN=sum))
dim(spraySums)
head(spraySums)

### working with dplyr 
options(width=105)
library(dplyr)
chicago<-readRDS("chicago.rds")
dim(chicago)
names(chicago)

##select function
head(select(chicago,city:dptp))
head(select(chicago,-(city:dptp)))
i<-match("city",names(chicago))
j<-match("city",names(chicago))
head(chicago[,-(i:j)])

##filter function
chic.f<-filter(chicag,pm25tmean2>30)
head(chic.f,10)
chic.f<-filter(chicago,pm25tmean2>30&tmpd>80)
head(chicago)
tail(chicago)

##arrange function
chicago<-arrange(chicago,date)
head(chicago)
tail(chicago)

chicago<-arrange(chicago,desc(date)) ###decreasing data
head(chicago)
tail(chicago)

##mutate
chicago<-mutate(chicago,pm25detrend=pm25tmean2-mean(pm25tmean2,na.rm=T))
head(select(chicago,pm25detrend=pm25tmean2-mean(pm25tmean2,na.rm=T)))

chicago<-mutate(chicago,tempcat=factor(1*(tmpd>80),
                                       labels=c("cold","hot")),na.rm=T)

hotcold<-group_by(chicago,tempcat)
hotcold


summarize(hotcold,pm25tmean2=mean(pm25tmean2),
          o3=max(o3tmean2), no2=median(no2tmean2))

summarize(hotcold,pm25tmean2=mean(pm25tmean2,na.rm=T),
          o3=max(o3tmean2), no2=median(no2tmean2))

chicago<-mutate(chicago,year=as.POSIXlt(date)$year+1900) 
##problem in reading POSIXlt
years<-group_by(chicago,year)

chicago %>% 
mutate(month=as.POSIXlt(data)$mon +1) %>% 
group_by(month) %>%
summarize(pm25=mean(pm25,na.rm=T), o3=max(o3tmean), no2=median(no2tmean2))



##rename
chicago<-rename(chicago, pm25 = pm25tmean2, dewpoint=dptp)
head(chicago)


if(!file.exists( "./data")){dir.create( "./data")}
fileUrl1 = "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 = "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1,destfile= "./data/reviews.csv")
download.file(fileUrl2,destfile= "./data/solutions.csv")
reviews = read.csv("./data/reviews.csv" ); solutions <- read.csv( "./data/solutions.csv" )
head(reviews, 2)

head(solutions, 2)

names(reviews)

names(solutions)

mergedData = merge(reviews,solutions,by.x="solution_id" ,by.y="id",all=TRUE)
head(mergedData)

intersect(names(solutions),names(reviews))

mergedData2 = merge(reviews,solutions,all=TRUE)
head(mergedData2)

df1 = data.frame(id=sample(1:10),x=rnorm(10))
df2 = data.frame(id=sample(1:10),y=rnorm(10))
arrange(join(df1,df2),id)

df1 = data.frame(id=sample(1:10),x=rnorm(10))
df2 = data.frame(id=sample(1:10),y=rnorm(10))
df3 = data.frame(id=sample(1:10),z=rnorm(10))
dfList = list(df1,df2,df3)
join_all(dfList)





