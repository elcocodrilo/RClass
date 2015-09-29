best <- function(state, outcome) {
    ## Read outcome data
    
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    outcomedata[, 11] <- as.numeric(outcomedata[, 11])
    outcomedata[, 17] <- as.numeric(outcomedata[, 17])
    outcomedata[, 23] <- as.numeric(outcomedata[, 23])
    
    ## Check that state and outcome are valid
    if (state %in% outcomedata$State) { 
        }
    else {
        stop("invalid state")
        }
    
    if (outcome %in% c("heart attack", "heart failure", "pneumonia")) { 
    }
    else {
        stop("invalid outcome")
    }
    
    ## create logical vector selecting states 
    ## which matches argument value stored in"state"
    
    selectstate <- as.vector(outcomedata$State == state)
    
    outcomedataforstate <- outcomedata[selectstate, ]
    
    if (outcome == "heart attack") {
        outcomecolumn <- 11
    }
    
    if (outcome == "heart failure") {
        outcomecolumn <- 17
    }
    
    if (outcome == "pneumonia") {
        outcomecolumn <- 23
    }
    
    ## remove rows with missing values
    rowswithmissingvalues <- is.na(outcomedataforstate[, outcomecolumn])
    outcomedataforstatermmv <- outcomedataforstate[!rowswithmissingvalues, ]
    
    minvalue <- min(outcomedataforstatermmv[, outcomecolumn])
    hospitalwithhighestvalue <- as.vector(outcomedataforstatermmv[, outcomecolumn] == minvalue)
    besthospitalname <- outcomedataforstatermmv[hospitalwithhighestvalue, "Hospital.Name"]
    print(besthospitalname)
}