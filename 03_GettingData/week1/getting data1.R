setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\Getting and Cleaning Data")

if (!file.exists("Getting and Cleaning Data")) {
  dir.create("Getting and Cleaning Data")
}

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./cameras.csv")
list.files()
dateDownloaded <- date()

cameraData <- read.table("./cameras.csv",sep=",",header=T)
head(cameraData)

#reading exell files
if(!file.exists("data")){dir.create("data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./cameras.xlsx", mode='wb')
dateDownloaded <- date()

library(xlsx)
cameraData <- read.xlsx("./cameras.xlsx",sheetIndex=1,header=TRUE)
head(cameraData)

colIndex <- 2:3
rowIndex <- 1:4
cameraDataSubset <- read.xlsx("./cameras.xlsx",sheetIndex=1,
                              colIndex=colIndex,rowIndex=rowIndex)
cameraDataSubset


#reading XML

library(XML)
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl,useInternal= TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
rootNode[[1]]
rootNode[[1]][[1]]
xmlSApply(rootNode,xmlValue)

fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
# download.file(fileUrl, destfile = "./doc1.XML")
# doc1<-xmlTreeParse("doc1.XML",useInternal=TRUE)

doc1 <- htmlTreeParse(fileUrl,useInternal= TRUE)
scores <- xpathSApply(doc, "//li[@class='score']" ,xmlValue)
teams <- xpathSApply(doc, "//li[@class='team-name']" ,xmlValue)
scores

#reading JSON
library(jsonlite)
jsonData <- fromJSON( "https://api.github.com/users/jtleek/repos" )
names(jsonData)
jsonData$name

names(jsonData$owner)

jsonData$owner$login

myjson <- toJSON(iris, pretty= TRUE)
cat(myjson)

iris2 <- fromJSON(myjson)
head(iris2)


#Data frame

library(data.table)
DF = data.frame(x=rnorm( 9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DF,3)

DT = data.table(x=rnorm( 9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DT,3)

tables()

DT[2,]

DT[DT$y=="a",]

DT[c(2,3)]

DT[,c(2,3)]

{
  x = 1
  y = 2
}
k = {print(10); 5}

DT[,list(mean(x),sum(z))]

DT[,w:=z^2]

DT2 <- DT
DT[, y:= 2]

head(DT,n=3)

DT[,m:= {tmp <- (x+z); log2(tmp+ 5)}]

DT[,a:=x>0]

DT[,b:= mean(x+w),by=a]

set.seed(123);
DT <- data.table(x=sample(letters[1:3], 1E5, TRUE))
DT[, .N, by=x]

DT <- data.table(x=rep(c( "a","b","c"),each=100), y=rnorm(300))
setkey(DT, x)
DT['a']

DT1 <- data.table(x=c( 'a', 'a', 'b', 'dt1'), y=1:4)
DT2 <- data.table(x=c( 'a', 'b', 'dt2'), z=5:7)
setkey(DT1, x); setkey(DT2, x)
merge(DT1, DT2)

big_df <- data.frame(x=rnorm( 1E6), y=rnorm(1E6))
file <- tempfile()
write.table(big_df, file=file, row.names= FALSE, col.names=TRUE, sep="\t", quote=FALSE)
system.time(fread(file))

system.time(read.table(file, header= TRUE, sep="\t"))


##quiz1
##question1
setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\Getting and Cleaning Data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./data1quiz1.csv")
quiz1 <- read.table("./data1quiz1.csv",sep=",",header=T)
adj<-quiz1["ADJUST"]
c<-0
for (i in 1:length(quiz1)){
  if(adj[i,]>1e6 & !(is.na(adj[i,]))){
    c<-c+1
  } 
}

##question3
# remove.packages(c("rJava", "xlsxjars", "xlsx"))

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, destfile='./sample.xlsx', mode='wb')
colIndex <- 7:15
rowIndex <- 18:23
dat <- read.xlsx("./sample.xlsx",sheetIndex=1,colIndex=colIndex,rowIndex=rowIndex)
sum(dat$Zip*dat$Ext,na.rm=T) 


##question4
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(fileUrl, destfile = "./data3quiz1.XML")
doc<-xmlTreeParse("data3quiz1.XML",useInternal=TRUE)
temp<-xpathSApply(doc,"//zipcode",xmlValue)
c<-0
for (i in 1:length(temp)){
  if (temp[i]==21231){
    c<-c+1
  }
}


##question5
library(data.table)
# DT <- fread("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv")

DT<-fread("getdata_data_ss06pid.csv")
system.time({sapply(split(DT$pwgtp15,DT$SEX),mean)})
system.time({tapply(DT$pwgtp15,DT$SEX,mean)})
system.time({ mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)})
system.time({DT[,mean(pwgtp15),by=SEX]})
system.time({rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]})
system.time({mean(DT$pwgtp15,by=DT$SEX)})

start.time <- Sys.time()
tapply(DT$pwgtp15,DT$SEX,mean)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#A
st = proc.time()
for (i in 1:100){
  sapply(split(DT$pwgtp15,DT$SEX),mean)
}
print (proc.time() - st)

#B
st = proc.time()
for (i in 1:100){
  rowMeans(DT)[DT$SEX==1];rowMeans(DT)[DT$SEX==2]
}
print (proc.time() - st)

#C
st = proc.time()
for (i in 1:100){
  mean(DT$pwgtp15,by=DT$SEX)
}
print (proc.time() - st)

#D
st = proc.time()
for (i in 1:100){
  tapply(DT$pwgtp15,DT$SEX,mean)
}
print (proc.time() - st)

#E
st = proc.time()
for (i in 1:100){
  mean(DT[DT$SEX==1,]$pwgtp15);mean(DT[DT$SEX==2,]$pwgtp15)
}
print (proc.time() - st)

#F
st = proc.time()
for (i in 1:100){
  DT[,mean(pwgtp15),by=SEX]
}
print (proc.time() - st)

