pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    setwd(directory)
    
    file_list <- sprintf("%03d.csv", id)
    
    for (file in file_list) {
        
        # if the merged dataset doesn't exist, create it
        if (!exists("pollutiondata")) {
            pollutiondata <- read.csv(file)
        }
        
        # if the merged dataset does exist, append to it
        else {
            temp_pollutiondata <- read.csv(file)
            pollutiondata <- rbind(pollutiondata, temp_pollutiondata)
            rm(temp_pollutiondata)
        }
    }
    setwd("..")

    mean(pollutiondata[, pollutant], na.rm = TRUE)
        
}