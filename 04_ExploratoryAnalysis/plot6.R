setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\exploratory data analysis\\project2")
# http://stackoverflow.com/questions/1181060

NEI <-readRDS("summarySCC_PM25.rds")
SCC <-readRDS("Source_Classification_Code.rds")
mergedData = merge(NEI,SCC,by.x="SCC" ,by.y="SCC",all=TRUE)
MotorData <- mergedData[grep("motorcycles", tolower(as.character(mergedData$Short.Name))),]
MotorData1 <- MotorData[(!is.na(MotorData$year)),]
MotorData2 <- MotorData1[!is.na(MotorData1$Emissions),]
MotorData2Baltimore <- subset(MotorData2, subset=(fips=="24510"))
MotorData2LosAngeles <- subset(MotorData2, subset=(fips=="06037"))

MDBY<-group_by(MotorData2Baltimore,year)
MDLY<-group_by(MotorData2LosAngeles,year)
MDBYS<-summarize(MDBY, TotalEmission=sum(Emissions))
MDLYS<-summarize(MDLY, TotalEmission=sum(Emissions))

par(mfrow = c( 2, 1), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(MotorData2, {
plot(MDBYS$year, MDBYS$TotalEmission, xlab="year", 
     ylab= "Total Emission",
     col = "blue",type="l",
     main="Emission of MotorCycles in Baltimore City", lwd = 10)
plot(MDLYS$year, MDLYS$TotalEmission, xlab="year", 
     ylab= "Total Emission",
     col = "red",type="l",
     main="Emission of MotorCycles in Los Angeles City", lwd = 10)
})
dev.copy(png, file = "plot6.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!
