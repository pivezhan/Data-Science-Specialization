setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\Getting and Cleaning Data\\week4")


##fixing character vectors
# if(!file.exists("./data")){dir.create("./data")}
# fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
# download.file(fileUrl,destfile="./data/cameras.csv",method="curl")
cameraData <- read.csv("cameras.csv")
names(cameraData)

# lower case of all names
tolower(names(cameraData))
splitNames = strsplit(names(cameraData),"\\.")
splitNames[[5]]

splitNames[[6]]

# quick aside -lists
mylist <- list(letters = c("A", "b", "c"), numbers = 1:3, matrix(1:25, ncol = 5))
head(mylist)

mylist[1]

mylist$letters

mylist[[1]]

#### split first part of the sixth element 
splitNames[[6]][1]

firstElement <- function(x){x[1]}
sapply(splitNames,firstElement)

# fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
# fileUrl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
# download.file(fileUrl1,destfile= "./data/reviews.csv" ,method="curl")
# download.file(fileUrl2,destfile= "./data/solutions.csv" ,method="curl")
reviews <- read.csv( "reviews.csv" ); solutions <- read.csv("solutions.csv" )
head(reviews, 2)

head(solutions, 2)

names(reviews)

# replace variable names
sub("_","",names(reviews),)

testName <- "this_is_a_test"
sub("_","",testName)

gsub("_","",testName)

grep("Alameda",cameraData$intersection)

table(grepl("Alameda",cameraData$intersection))

cameraData2 <- cameraData[!grepl( "Alameda",cameraData$intersection),]

grep("Alameda",cameraData$intersection,value= TRUE)

grep("JeffStreet",cameraData$intersection)

length(grep("JeffStreet",cameraData$intersection))

library(stringr)
nchar("Jeffrey Leek" )

substr("Jeffrey Leek" ,1,7)

paste("Jeffrey","Leek")

paste0("Jeffrey","Leek")

str_trim("Jeff ")

### regular expression I

##meta character
^i think
morning$

##character classes with[]
# all the forms of bush
[Bb][Uu][Ss][Hh]
##all character in[] are interchangeable
^[Ii] am

^[0-9][a-zA-Z]
##search for any sentence without ? .  mark at the end
[^?.]$
        
### regular expression II
flood|fire
flood|earthquake|hurricane|coldfire

##more character
# good or bad inthe biginning of the line
^([Gg]ood|[Bb]ad)
() shows that this charcter could be 
[Gg]eorge( [Ww]\.)? [Bb]ush
# * means non of the items + means at least one of items
(.*)

[0-9]+ (.*)[0-9]+
        
[Bb]ush( +[^ ]+ +){1,5} debate  

+([a-zA-Z]+) +\1 +
        
^s(.*)s

^s(.*?)s$
#         working with dates

d1 = date()
d1

class(d1)

d2 = Sys.Date()
d2

class(d2)

format(d2,"%a %b %d")

x = c("1jan1960", "2jan1960", "31mar1960", "30jul1960"); 
z = as.Date(x, "%d%b%Y")
z

z[1] - z[2]

as.numeric(z[1]-z[2])

weekdays(d2)

months(d2)

julian(d2)

###lubricate simple function for working with dates
library(lubridate); ymd( "20140108")

mdy("08/04/2013")

dmy("03-04-2013")

ymd_hms("2011-08-03 10:15:03" )

ymd_hms("2011-08-03 10:15:03" ,tz="Pacific/Auckland" )

?Sys.timezone

x = dmy(c("1jan2013", "2jan2013", "31mar2013", "30jul2013"))
wday(x[1])
wday(x[1],label=TRUE)

