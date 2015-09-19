complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    setwd(directory)
    
    file_list <- sprintf("%03d.csv", id)
    
    for (file in file_list){
        
        # if the merged dataset doesn't exist, create it
        if (!exists("pollutiondata")) {
            pollutiondata <- read.csv(file)
        }
        
        # if the merged dataset does exist, append to it
        else {
            temp_pollutiondata <-read.csv(file)
            pollutiondata <- rbind(pollutiondata, temp_pollutiondata)
            rm(temp_pollutiondata)
        }
    }
    setwd("..")
    
    good <- complete.cases(pollutiondata$sulfate, pollutiondata$nitrate)
    pollutiondatacc <- pollutiondata[good, ]
    
    nobsfram <- as.data.frame(table(pollutiondatacc$ID))
    
    names(nobsfram) <- c("id", "nobs")

    nobsfram2 <- nobsfram[match(id, nobsfram$id), ]
    
    nobsfram2
    
}