rankall <- function(outcome, num = "best") {
    ## Read outcome data
    
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    outcomedata[, 11] <- as.numeric(outcomedata[, 11])
    outcomedata[, 17] <- as.numeric(outcomedata[, 17])
    outcomedata[, 23] <- as.numeric(outcomedata[, 23])
    
    ## Check that outcome are valid
    
    if (outcome %in% c("heart attack", "heart failure", "pneumonia")) { 
    }
    else {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    
    ## Select the column corrosponding to the input for the outcome argument
    if (outcome == "heart attack") {
        outcomecolumn <- 11
    }
    
    if (outcome == "heart failure") {
        outcomecolumn <- 17
    }
    
    if (outcome == "pneumonia") {
        outcomecolumn <- 23
    }
    
    states <- sort(unique(outcomedata$State))
    
    dfrank <- data.frame()
    
    ## For loop to go through all of the states
    
    for (state in states) {
        
        selectstate <- as.vector(outcomedata$State == state)
        
        outcomedataforstate <- outcomedata[selectstate, ]
        
        ## create logical vector to remove rows with missing values
        rowswithmissingvalues <- is.na(outcomedataforstate[, outcomecolumn])
        
        ## create new table with missing values removed
        outcomedataforstatermmv <- outcomedataforstate[!rowswithmissingvalues, ]
        
        
        ## Order the table based on the outcome requested in the argument "outcome"
        
        if (outcome == "heart attack") {
            outcomeranked <- outcomedataforstatermmv[order(outcomedataforstatermmv$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcomedataforstatermmv$Hospital.Name), ]
        }
        
        if (outcome == "heart failure") {
            outcomeranked <- outcomedataforstatermmv[order(outcomedataforstatermmv$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, outcomedataforstatermmv$Hospital.Name), ]
        }
        
        if (outcome == "pneumonia") {
            outcomeranked <- outcomedataforstatermmv[order(outcomedataforstatermmv$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, outcomedataforstatermmv$Hospital.Name), ]
        }
        
        ## Print the result requested in the argument "num"
        
        temp_dfrank <- data.frame()
        
        if (class(num) != "character") {
            temp_dfrank[1, 1] <- outcomeranked$Hospital.Name[num]
            temp_dfrank[1, 2] <- state
            }
        
        if (num == "best") {
            temp_dfrank[1, 1] <- outcomeranked$Hospital.Name[1]
            temp_dfrank[1, 2] <- state
            }
        
        if (num == "worst") {
            temp_dfrank[1, 1] <- tail(outcomeranked$Hospital.Name, n = 1)
            temp_dfrank[1, 2] <- state
            }
        
        dfrank <- rbind(dfrank, temp_dfrank)
        rm(temp_dfrank, selectstate, outcomedataforstate, rowswithmissingvalues, outcomedataforstatermmv)
            
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    names(dfrank) <- c("hospital", "state")
    print(dfrank)
    
}