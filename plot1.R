plot1 <- function(filetype="png"){
        
        ## This function creates a histogram of "Global Active Power" in the 
        ## specified bitmap format graphics devices "bmp", "jpeg", "png", or 
        ## "tiff". If nothing is specified, a "png" file will be generated.
        
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
        fun_call(filename = paste("plot1.", filetype, sep = ""), 
                 width = 480, height = 480, units = "px")
        
        ## Create histogram
        hist(data_subset$Global_active_power, 
             col = "red",
             main = "Global Active Power",
             xlab = "Global Active Power (kilowatts)")
        
        ## Complete and close plot
        dev.off()
}