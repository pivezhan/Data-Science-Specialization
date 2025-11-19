
library(RMySQL)
ucscDb <- dbConnect(MySQL(),user= "genome", host= "genome-mysql.cse.ucsc.edu" )
result <- dbGetQuery(ucscDb, "show databases;" ); dbDisconnect(ucscDb);

result

######

hg19 <- dbConnect(MySQL(),user= "genome", db="hg19", host= "genome-mysql.cse.ucsc.edu" )
allTables <- dbListTables(hg19)
length(allTables)

allTables[1:5]
######

dbListFields(hg19, "affyU133Plus2" )

dbGetQuery(hg19, "select count(*) from affyU133Plus2" )

query <- dbSendQuery(hg19, "select * from affyU133Plus2 where misMatches between 1 and 3" )
affyMis <- fetch(query); quantile(affyMis$misMatches)

affyMisSmall <- fetch(query,n= 10); dbClearResult(query);

dim(affyMisSmall)
dbDisconnect(hg19)


##### reading from the web

con = url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en" )
htmlCode = readLines(con)
close(con)
htmlCode


library(XML)
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes= T)
xpathSApply(html, "//title", xmlValue)
xpathSApply(html, "//td[@id='col-citedby']" , xmlValue)


library(httr); html2 = GET(url)
content2 = content(html2,as= "text")
parsedHtml = htmlParse(content2,asText= TRUE)
xpathSApply(parsedHtml, "//title", xmlValue)


google = handle( "http://google.com" )
pg1 = GET(handle=google,path= "/")
pg2 = GET(handle=google,path= "search")


pg2 = GET("http://httpbin.org/basic-auth/user/passwd" ,
          authenticate( "user","passwd"))
pg2


google = handle( "http://google.com" )
pg1 = GET(handle=google,path= "/")
pg2 = GET(handle=google,path= "search")


######### reading APIs


myapp = oauth_app( "twitter",key= "yourConsumerKeyHere" ,secret="yourConsumerSecretHere" )
sig = sign_oauth1.0(myapp,token = "yourTokenHere" ,token_secret = "yourTokenSecretHere" )
homeTL = GET( "https://api.twitter.com/1.1/statuses/home_timeline.json" , sig)



myapp = oauth_app( "twitter",key= "yourConsumerKeyHere" ,secret="yourConsumerSecretHere" )
sig = sign_oauth1.0(myapp,token = "yourTokenHere" ,token_secret = "yourTokenSecretHere" )
homeTL = GET( "https://api.twitter.com/1.1/statuses/home_timeline.json" , sig)


json1 = content(homeTL)
json2 = jsonlite::fromJSON(toJSON(json1))
json2[1,1:4]


# quiz2 question1
library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications;
#    Use any URL you would like for the homepage URL (http://github.com is fine)
#    and http://localhost:1410 as the callback url
#
#    Insert your client ID and secret below - if secret is omitted, it will
#    look it up in the GITHUB_CONSUMER_SECRET environmental variable.
myapp <- oauth_app("github", "56b637a5baffac62cad9")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
content(req)

# OR:
req <- with_config(gtoken, GET("https://api.github.com/users/jtleek/repos"))
stop_for_status(req)
content(req)

myapp <- oauth_app("github",key="43cb305e93285a9d519a",secret = "9e674afb3f37971db6f8f729e398726968b13252") 
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp) 
gtoken <- config(token = github_token)


# quiz2 question2

acs<- read.csv("C:/Users/lenovo/Desktop/Rlearning/Getting and Cleaning Data/week2/getdata_data_ss06pid.csv")


# quiz 2 question 4

library(XML)
theurl <- "http://biostat.jhsph.edu/~jleek/contact.html"
tables <- readHTMLTable(theurl)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))


# quiz 2 question 5

url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"

