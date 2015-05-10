 data_full <- read.csv("household_power_consumption.txt", header=T, sep=';', na.strings="?", 
						nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"') 
 data_full$Date <- as.Date(data_full$Date, format="%d/%m/%Y") 
 data <- subset(data_full, subset=(Date >= "2007-02-01" & Date <= "2007-02-02")) 
 datetime <- paste(as.Date(data$Date), data$Time) 
 data$Datetime <- as.POSIXct(datetime) 
 png("plot2.png", width=480, height=480)
 plot(data$Global_active_power~data$Datetime, type="l", ylab="Global Active Power (kilowatts)", xlab="") 
 dev.off() 