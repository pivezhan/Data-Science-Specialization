setwd("C:/Users/lenovo/Desktop/Rlearning/Getting and Cleaning Data/week3")

####subbsetting
set.seed(13435)
X <- data.frame( "var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
X <- X[sample( 1:5),]; X$var2[c( 1,3)] = NA
X

# subsetting
X[,1]
X[,"var1"]
X[1:2,"var2"]

###logical ands ors
X[(X$var1 <= 3& X$var3 > 11),]
X[(X$var1 <= 3| X$var3 > 15),]

##dealing with missing values
X[which(X$var2 > 8),]  ##which do not return NAs

##sorting
sort(X$var1)

sort(X$var1,decreasing= TRUE)

sort(X$var2,na.last= TRUE)


###ordering
X[order(X$var1),]

X[order(X$var1,X$var3),] ##multi variable sorting order


##ordering with plyr
library(plyr)
arrange(X,var1)
arrange(X,desc(var1))

###adding rows and columns
X$var4 <- rnorm( 5)
X
##another way
Y <- cbind(X,rnorm(5))
Y

###summarizing data
##baltimore data reading and export
##https://data.baltimorecity.gov/Community/Restaurants/k5ry-ef3g

if(!file.exists( "./data")){dir.create( "./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile= "./data/restaurants.csv" )
restData <- read.csv( "./data/restaurants.csv" )

##look at data
head(restData,n= 3) ##n equals to number of rows

tail(restData,n= 3)

summary(restData)

## extra information
str(restData)

##quantile of quantitative values
quantile(restData$councilDistrict,na.rm= TRUE)
quantile(restData$councilDistrict,probs=c(0.5,0.75,0.9))

##making table
table(restData$zipCode,useNA= "ifany")
table(restData$councilDistrict,restData$zipCode)

##checking for missing values
sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode > 0)

##row and column sums
colSums(is.na(restData))
all(colSums(is.na(restData))== 0)
table(restData$zipCode %in% c("21212"))
table(restData$zipCode %in% c("21212","21213"))

###values with specific characteristics
restData[restData$zipCode %in% c("21212","21213"),]

##cross tabs(summaries)
data(UCBAdmissions)
DF = as.data.frame(UCBAdmissions)
summary(DF)
xt <- xtabs(Freq ~ Gender + Admit,data=DF)
xt


##flat tables
warpbreaks$replicate <- rep( 1:9, len = 54)
xt = xtabs(breaks ~.,data=warpbreaks)
xt
ftable(xt)

##size of dataset
fakeData=rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData),units="Mb")







