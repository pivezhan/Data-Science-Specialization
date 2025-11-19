setwd(file.path("D:", "sbu", "RLearning", "exploratory data analysis","project1"))

EPC <- read.csv("household_power_consumption.txt",
                header=T, sep=';', na.strings="?", 
                nrows=2075259, check.names=F, 
                stringsAsFactors=F, comment.char="",
                quote='\"')
EPC$Date <- as.Date(EPC$Date, format="%d/%m/%Y")

data <- subset(EPC, subset=( (Date <= "2007-02-02") & (Date >= "2007-02-01")))
rm(EPC)

data$mixeddata <- as.POSIXct(paste(data$Date,data$Time))
par(mfrow = c( 2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(data, {
        plot(Global_active_power~mixeddata, type="l", 
             ylab="Global Active Power", xlab="")
        
        plot(Voltage~mixeddata, type="l", 
             ylab="Voltage", xlab="datetime")
        
        plot(Sub_metering_1~mixeddata, type="l", 
             ylab="Energy sub metering", xlab="")
        lines(Sub_metering_2~mixeddata,col='Red')
        lines(Sub_metering_3~mixeddata,col='Blue')
        legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2,inset = .05,
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),trace=T,bty = "n")
        
        plot(Global_reactive_power~mixeddata, type="l", 
             ylab="Global_rective_power",xlab="datetime")
})

dev.copy(png, file = "unnamed-chunk-5.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!