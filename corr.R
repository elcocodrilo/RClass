corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    setwd(directory)
    
    file_list <- list.files()
    
    for (file in file_list){
        
        ## if the merged dataset doesn't exist, create it
        if (!exists("pollutiondata")) {
            pollutiondata <- read.csv(file)
        }
        
        ## if the merged dataset does exist, append to it
        else {
            temp_pollutiondata <-read.csv(file)
            pollutiondata <- rbind(pollutiondata, temp_pollutiondata)
            rm(temp_pollutiondata)
        }
    }
    setwd("..")
    
    ## logical vector for complete cases
    good <- complete.cases(pollutiondata$sulfate, pollutiondata$nitrate)
    
    ## new data frame comprised only of complete cases
    pollutiondatacc <- pollutiondata[good, ]
    
    ## create data frame showing the number of complete cases for each id
    nobsfram <- as.data.frame(table(pollutiondatacc$ID))
    
    ## name the columns of the dataframe
    names(nobsfram) <- c("id", "nobs")
    
    ## create a vector showing all ids present in data frame of complete cases
    ids <- as.vector(nobsfram$id)
    
    ## logical vector showing if greater number of complete cases than threshold
    idstocor <- as.vector(nobsfram$nobs > threshold)
    
    idsover <- ids[idstocor]
    
    corrlist <- numeric()
    
    for (cid in idsover) ## for loop for all ids with greater than the threshold for complete cases
        {
            temp_corrlist <- cor(pollutiondatacc[pollutiondatacc$ID == cid, "nitrate"], pollutiondatacc[pollutiondatacc$ID == cid, "sulfate"])
            corrlist <- c(corrlist, temp_corrlist) ## create vector showing correlations
            rm(temp_corrlist)
        }
        

    print(corrlist)
## rm(nobsfram, pollutiondata, pollutiondatacc, cid, corrlist, cr, file, file_list, good, ids, idstocor, threshold)  
}