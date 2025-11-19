setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\reproducible research\\programming2")
StormData<-read.csv("repdata_data_StormData.csv",sep=",")

test <- runif(30) * 360
ROSE(test)
ROSE(test, bins=10, rscale=2, labels=TRUE, rings=TRUE, col="cyan", lwd=2)

library(lubridate)
newd<-gsub(" ","",as.character(StormData$BGN_DATE))
newd<-gsub("0:00:00","",newd)
newd<-as.Date(newd,format='%m/%d/%Y')
StormData$BGN_DATE<-(factor(newd))

plot(hacide.train[, 2:3], main="Unbalanced data", xlim=c(-4,4),
     ylim=c(-4,4), col=as.numeric(hacide.train$cls), pch=20)
legend("topleft", c("Majority class","Minority class"), pch=20, col=1:2)

head(StormData)

grep("2051",as.character(newd))

as.Date(newd)

?StormData
StormData$EVTYPE[StormData$INJURIES==max(StormData$INJURIES)]

library(dplyr)
DataSample<-tbl_df(StormData)
rm(StormData)

By_EVTYPE<-group_by(DataSample,EVTYPE)
healthSum<-summarize(By_EVTYPE,TotalInjureis=sum(INJURIES))
healthsum<-arrange(healthsum,desc(TotalInjureis))
head(healthsum)

By_Economics<-group_by(DataSample,PROPDMG)
EconomicSum<-summarize(By_EVTYPE,TotalDistortion=sum(PROPDMG))
EconomicSum<-arrange(EconomicSum,desc(TotalDistortion))
head(EconomicSum)

events<-20
others<-sum(healthSum$TotalInjureis[(events+1):nrow(healthsum)])
HeSum<-matrix(rep(0,2*(events+1)),(events+1),2)
HeSum[,1]<-c(as.character(healthsum$EVTYPE[1:events]),"others")
HeSum[,2]<-c(healthsum$TotalInjureis[1:events], others)
HeSum<-data.frame(HeSum)
colnames(HeSum)<-c("EVTYPE","Total.Injureis")
HeSum

others<-sum(EconomicSum$TotalDistortion[(events+1):nrow(EconomicSum)])
EcSum<-matrix(rep(0,2*(events+1)),(events+1),2)
EcSum[,1]<-c(as.character(EconomicSum$EVTYPE[1:events]),"others")
EcSum[,2]<-c(EconomicSum$TotalDistortion[1:events], others)
EcSum<-data.frame(EcSum)
colnames(EcSum)<-c("EVTYPE","Total.Distortion")
EcSum

head(StormData,2)

events<-21
others<-sum(EconomicSum$TotalDamage[(events+1):nrow(EconomicSum)])
EcSum<-matrix(rep(0,2*(events+1)),(events+1),2)
EcSum[,1]<-c(as.character(EconomicSum$EVTYPE[1:events]),"others")
EcSum[,2]<-c(EconomicSum$TotalDamage[1:events], others)
EcSum<-data.frame(EcSum)
colnames(EcSum)<-c("EVTYPE","Total.Damage")

library(lattice)
xyplot(INJURIES ~ PROPDMG | EcSum$EVTYPE[1:20], data = DataSample, layout = c(5, 4))
dev.copy(png, file = "latticeplot.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!
