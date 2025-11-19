# question1
housing <- read.csv("getdata_data_ss06hid.csv")
names(housing)
splitNames = strsplit(names(housing),"wgtp")
splitNames[[123]]

##question2
library(plyr)
GDPC<-read.csv("getdata_data_GDP.csv",skip=4,sep=",",nrows=215)
GDPC<-GDPC[GDPC$X!="",]
EDSTATS<-read.csv("getdata_data_EDSTATS_Country.csv")
GDPC<-rename(GDPC, replace=c("X"="CountryCode","X.1"="ranking","X.3"="gdp"))
DT<-arrange(join(GDPC,EDSTATS),CountryCode)
MValues<-DT$X.4[grep("[0-9]",DT$X.4)]
mD<-as.numeric(gsub(",","",MValues))
(sum(mD)/length(mD))

# question3
library(UsingR);library(plyr)
GDP <- rename(GDP, replace=c("X.2" = "countryNames"))
grep("^United",GDP$countryNames)
length(grep("^United",GDP$countryNames))

###question4
library(plyr)
GDPC<-read.csv("getdata_data_GDP.csv",skip=4,sep=",",nrows=215)
GDPC<-GDPC[GDPC$X!="",]
EDSTATS<-read.csv("getdata_data_EDSTATS_Country.csv")
GDPC<-rename(GDPC, replace=c("X"="CountryCode","X.1"="ranking","X.3"="gdp"))
DT<-arrange(join(GDPC,EDSTATS),CountryCode)
temp<-DT$Special.Notes[(grep("June",DT$Special.Notes))]
length(grep("Fiscal",temp))

# question5
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 
splitNames = strsplit(as.character(sampleTimes),"\\-")
length(grep("2012",splitNames))
WEEK<-weekdays(sampleTimes)
temp<-sampleTimes[grep("ÏæÔäÈå",WEEK)]
split2 = strsplit(as.character(temp),"\\-")
length(grep("2012",split2))
