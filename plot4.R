plot4 <- function (){
   
    library(dplyr)
    
    #extract names
    names <- read.table("household_power_consumption.txt", sep = ";", nrows = 1)
    
    # assign classes vector
    colClasses <- c("character", "character", "numeric", "numeric", "numeric", "numeric", 
                    "numeric", "numeric", "numeric")
    
    # find the first and last appearance of the dates that we need
    first <- grep("^1/2/2007", readLines("household_power_consumption.txt"))[1]
    last <- last(grep("^2/2/2007", readLines("household_power_consumption.txt")))
    
    #how many rows to skip and how many to read
    skip <- first - 1
    nrows <- last - first + 1
    
    
    
    #ctreate one POSIX column from the two given
    library("lubridate")
    timeString <- paste(pcons$Date, pcons$Time)
    timePOSIX <- dmy_hms(timeString)
    
    #replacing Date and Time column with new Time column of class POSIX
    pcons <- select(pcons, 3:9) %>% mutate(Time = timePOSIX) %>% select(Time, 1:7)
    
    
    #checking for NA
    na <- sapply(pcons, function(x) any(is.na(x)))
    
    
    png(file="plot4.png", width=500,height=500)
    
    #make a plot
    par(mfcol = c(2,2))
    
    # plot 4.1 same as plot 2:
    plot(pcons$Time, pcons$Global_active_power, type = "l", xlab = "", 
         ylab = "Global Active Power (kilowatts)", font = 2, font.lab = 2)
    
    # plot 4.2 same as plot 3:
    plot(pcons$Time, pcons$Sub_metering_1, xlab = "", ylab = "Energy sub metering", 
         type = "l", font = 2, font.lab = 2)
    lines(pcons$Time, pcons$Sub_metering_2, col = "red")
    lines(pcons$Time, pcons$Sub_metering_3, col = "blue")
    legend("topright", col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n",
           lwd = 2)
    
    #plot 4.3
    plot(pcons$Time, pcons$Voltage, type = "l", xlab = "datetime", ylab = "Voltage", 
         font = 2, font.lab = 2)
    
    #plot 4.4
    plot(pcons$Time, pcons$Global_reactive_power, type = "l", xlab = "datetime", 
         ylab = "Global_reactive_power", font = 2, font.lab = 2, col = "grey35")
   
    
    dev.off()
    
}