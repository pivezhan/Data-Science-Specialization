d<-read.csv("annual_all_2013.csv")
sub<-subset(d, Parameter.Name %in% c("PM2.5-Local Condition","Ozone")
& Pullutant.Standard %in% c("Ozone 8-Hour 2008","PM25 Annual 2006"),
c(Longitude,Latitude, Parameter.Name, Arithmetic.Mean))

head(sub)

pollavg<-aggregate(sub[,"Arithmetic.Mean"],sub[,c("Longitude","Latitude","Parameter.Name")],
                  mean,na.rm=TRUE)
names(pollavg)[4]<-"level"
pollavg<-transform(pollavg,Parameter.Name~factor(Parameter.Name))
rm(d,sub)
monitors<-data.matrix(pollavg[,c("Longitude","Latitude")])


##Input is data frame with
## lon: longitude
## lat: latitude
## radius: Radus in miles for finding

library(fields)
pollutant<-function(df){
x<-data.matrix(df[,c("lon","lat")])
r<-df$radius
d<-rdist.earth(monitors,x)
use<-lapply(seq_len(ncol(d)),function(i){
        which(d[,i]<r[i])
})
levels<-sapply(use,function(idx){
        with(pollavg[idx,],tapply(level,Parameter.Name,mean))
})
dlevels<-as.data.frame(t(levels))
data.frame(df,dlevels)
}
pollutant(data.frame(lon=-76.61,lat=39.28,radius=40))

install.packages("yhatr")

####using yhatr library
library(yhatr)

model.require <- function() {
        library(fields)
        
}

model.transform <- function(df) {
        df##just return the input
}

model.predict <- function(df) {
        data.frame(version = R.version.string, stringsAsFactors = FALSE)
}

yhat.config  <- c(
        username="rdpeng@gmail.com",
        apikey="90d2a80bb532cabb2387aa51ac4553cc",
        env="http://sandbox.yhathq.com/"
)


yhat.deploy("environment")

############################################
library(yhatr)

model.require <- function() {
        
}

model.transform <- function(df) {
        transform(df, Wind = as.numeric(as.character(Wind)),
                  Temp = as.integer(as.character(Temp)))
}

model.predict <- function(df) {
        result <- data.frame(Ozone = predict(fit, newdata = df))
        cl <- data.frame(clWind = class(df$Wind), clTemp = class(df$Temp))
        data.frame(result, Temp = as.character(df$Temp),
                   Wind = as.character(df$Wind), cl)
}

fit <- lm(Ozone ~ Wind + Temp, data = airquality)

yhat.config  <- c(
        username="rdpeng@gmail.com",
        apikey="90d2a80bb532cabb2387aa51ac4553cc",
        env="http://sandbox.yhathq.com/"
)


yhat.deploy("ozone")
############################################needs login username

df<-data.frame(lat=-39.28,lan=-76.61,radius=60)

yhat.config  <- c(
        username="rdpeng@gmail.com",
        apikey="90d2a80bb532cabb2387aa51ac4553cc",
        env="http://sandbox.yhathq.com/"
)

yhat.predict("pollutant",df)

#################access yhat in cURL
curl -X POST -H "Content-type:application/json" --user rdpeng@gmail.com:
        90d2a80bb532cabb2387aa51ac4553cc 
--data'{"lan":-76.61,"lat":39.28,"radius":60}' "http://sandbox.yhathq.com/" 
