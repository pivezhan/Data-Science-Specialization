setwd("C:/Users/lenovo/Desktop/Rlearning/Getting and Cleaning Data/week3")
## quiz3question1
library(UsingR)

X<-read.csv("getdata_data_ss06hid.csv")

# names(housing)
library(data.table)        
dt <- data.table(read.csv("getdata_data_ss06hid.csv"))
agricultureLogical <- dt$ACR == 3 & dt$AGS == 6
which(agricultureLogical)[1:3] ## shows number of rows with TRUE results

## quiz3 question 2
library(jpeg)
image<-readJPEG("getdata_jeff.jpg", native = T)
summary(image)
str(image)
##quantile of quantitative values
quantile(image,probs=c(0.3,0.8))

## quiz3 question 3 **incorrect**
GDPC<-read.csv("GDP.csv",skip=4,sep=",",nrows=215)
GDPC<-GDPC[GDPC$X!="",]
EDSTATS<-read.csv("getdata_data_EDSTATS_Country.csv")
GDPC<-rename(GDPC, replace=c("X"="CountryCode","X.1"="ranking","X.3"="gdp"))

library(plyr)
DT<-arrange(join(GDPC,EDSTATS),CountryCode)
DT<-DT[order(DT$ranking),]
DTR<-DT[DT$ranking[1:192],]
DTR$CountryCode[DTR$ranking[13]]
dt <- merge(GDPC, EDSTATS, all=TRUE, by=c("CountryCode"))
sum(!is.na(unique(dt$ranking)))
head(DTR,5)

##question4
HOECD<-DTR[DTR$Income.Group=="High income: OECD",]
HNOECD<-DTR[DTR$Income.Group=="High income: nonOECD",]
head(HOECD)
HOECD$ranking
mean(HOECD$ranking[!is.na(HOECD$ranking)])
mean(HNOECD$ranking[!is.na(HNOECD$ranking)])


summary(EDSTATS)
str(EDSTATS)

##quiz 3 question5
DTR$rankingGroup=cut(DTR$ranking,breaks=quantile(DTR$ranking,probs=c(0,.2,.4,.6,0.8)))
# table(DTR$rankingGroup~Income.Group)
cylData <- dcast(DTR, rankingGroup~Income.Group)
sum(DTR$Income.Group[1:39]=="Lower middle income")
