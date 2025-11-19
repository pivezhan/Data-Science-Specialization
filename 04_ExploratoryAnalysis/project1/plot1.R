setwd(file.path("D:", "sbu", "RLearning", "exploratory data analysis","project1"))

EPC <- read.csv("household_power_consumption.txt",
                      header=T, sep=';', na.strings="?", 
                      nrows=2075259, check.names=F, 
                      stringsAsFactors=F, comment.char="",
                      quote='\"')
EPC$Date <- as.Date(EPC$Date, format="%d/%m/%Y")

data <- subset(EPC, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
rm(EPC)
hist(data$Global_active_power, col = "red", xlab="Global Active Power(kilowatts)", 
     ylab="Frequency", main="Global Active Power")

dev.copy(png, file = "unnamed-chunk-2.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!