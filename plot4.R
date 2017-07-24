plot4 <- function(filetype="png"){
        
        ## This function creates four plots using Household Power Consumption
        ## data in the specified bitmap format graphics devices "bmp", "jpeg", 
        ## "png", or "tiff". If nothing is specified, a "png" file is generated.
        
        ## Create temporary file and download zip file to it
        temp <- tempfile()
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
                      temp)
        
        ## Unzip file and read table
        unziped <- unz(temp, "household_power_consumption.txt")
        power_data <- read.table(unziped, header=T, sep=";", na.strings = "?", 
                                 stringsAsFactors = F)
        
        ## Remove temp file
        unlink(temp)
        
        ## Subset data to the dates 2007-02-01 and 2007-02-02 
        data_subset <- power_data[power_data$Date %in% c("1/2/2007", "2/2/2007"), ]
        
        ## Convert the Date and Time character variables to Date/Time class 
        ## and place values into datetime column
        data_subset$datetime <- strptime(paste(data_subset$Date, data_subset$Time), 
                                         format = "%d/%m/%Y %H:%M:%S")
        
        ## Create file of specified type at 480 x 480 pixels
        fun_call <- get(filetype) ## Gets function from filetype name
        fun_call(filename = paste("plot4.",filetype, sep = ""), 
                 width = 480, height = 480, units = "px")
        
        ## Change plot parameters to allow 4 plots
        par(mfcol = c(2, 2))
        
        ## Create all 4 plots
        ## Top Left Plot: Global Active Power over time
        plot(data_subset$datetime, data_subset$Global_active_power, 
             type = "l", xlab = "", ylab = "Global Active Power")
        
        ## Bottom Left Plot: 3 sets of sub metering data over time 
        ## Open plot and create background with labeled axes
        with(data_subset, plot(datetime, y = Sub_metering_1, type = "n",
                               xlab = "", ylab = "Energy sub metering" ))
        
        ## Add data for each subset of Sub metering
        with(data_subset, points(datetime, y = Sub_metering_1, type = "l", 
                                 col = "black"))
        with(data_subset, points(datetime, y = Sub_metering_2, type = "l", 
                                 col = "red"))
        with(data_subset, points(datetime, y = Sub_metering_3, type = "l", 
                                 col = "blue"))
        
        ## Add legend
        legend("topright", 
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               col = c("black", "red", "blue"), lty = 1, bty = "n")
        
        ## Top Right Plot: Voltage over time
        with(data_subset, plot(datetime, Voltage, 
             type = "l", ylab = "Voltage"))
        
        ## Bottom Right Plot: Global reactive power over time
        with(data_subset, plot(datetime, Global_reactive_power, 
             type = "l"))
        
        ## Complete and close plot
        dev.off()
}