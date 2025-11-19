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
with(data, {
        plot(Sub_metering_1~mixeddata, type="l", 
        ylab="Energy sub metering", 
        xlab="")
        lines(Sub_metering_2~mixeddata,col='Red')
        lines(Sub_metering_3~mixeddata,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lty=1, lwd=1,cex = .8,bg = par("bg"),
       box.lwd = par("lwd"), box.lty = par("lty"),
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),trace=T)
dev.copy(png, file = "unnamed-chunk-4.png") ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!
#png(filename = "plot3.png", width = 480, height = 480, units = "px")
#with(power_small, plot(time, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering"))
#with(power_small, points(time, Sub_metering_2, type = "l", col = "red"))
#with(power_small, points(time, Sub_metering_3, type = "l", col = "blue"))
#legend("topright", pch = 151, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
#dev.off()
