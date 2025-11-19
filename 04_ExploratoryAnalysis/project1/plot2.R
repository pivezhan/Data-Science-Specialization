setwd(file.path("D:", "sbu", "RLearning", "exploratory data analysis","project1"))

EPC <- read.csv("household_power_consumption.txt",
                header=T, sep=';', na.strings="?", 
                nrows=2075259, check.names=F, 
                stringsAsFactors=F, comment.char="",
                quote='\"')
EPC$Date <- as.Date(EPC$Date, format="%d/%m/%Y")

data <- subset(EPC, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
rm(EPC)
data$mixeddata <- as.POSIXct(paste(data$Date,data$Time))
plot(data$Global_active_power~data$mixeddata, type="l",
     ylab="Global Active Power (kilowatts)", xlab="", main="")

dev.copy(png, file = "unnamed-chunk-3.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!