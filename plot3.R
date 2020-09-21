plot3 <- function (){
   
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
    
    pcons <- read.table("household_power_consumption.txt", sep = ";", skip=skip, 
                        col.names = names, colClasses = colClasses, comment.char = "", 
                        nrows = nrows)
    
    #ctreate one POSIX column from the two given
    library("lubridate")
    timeString <- paste(pcons$Date, pcons$Time)
    timePOSIX <- dmy_hms(timeString)
    
    #replacing Date and Time column with new Time column of class POSIX
    pcons <- select(pcons, 3:9) %>% mutate(Time = timePOSIX) %>% select(Time, 1:7)
    
    
    #checking for NA
    na <- sapply(pcons, function(x) any(is.na(x)))
    
   
    #make a plot
    par(mfrow = c(1,1))
    plot(pcons$Time, pcons$Sub_metering_1, xlab = "", ylab = "Energy sub metering", 
         type = "l")
        lines(pcons$Time, pcons$Sub_metering_2, col = "red")
    lines(pcons$Time, pcons$Sub_metering_3, col = "blue")
    legend("topright", lwd = 2, col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    #copy to file
    dev.copy(png, file = "plot3.png")
    dev.off()
    
    
}