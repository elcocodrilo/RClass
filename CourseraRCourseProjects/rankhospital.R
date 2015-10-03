rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    selectstate <- as.vector(outcomedata$State == state)
    
    outcomedataforstate <- outcomedata[selectstate, ]
    
    
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
    
    
    ## create logical vector to remove rows with missing values
    rowswithmissingvalues <- is.na(outcomedataforstate[, outcomecolumn])
    
    ## create new table with missing values removed
    outcomedataforstatermmv <- outcomedataforstate[!rowswithmissingvalues, ]
    
    
    ## Unfinished below, try to make if statements for each condition to order the table appropriately
    attach(outcomedataforstatermmv)
    
    if (outcome == "heart attack") {
        outcomeranked <- outcomedataforstatermmv[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name), ]
    }
    
    if (outcome == "heart failure") {
        outcomeranked <- outcomedataforstatermmv[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name), ]
    }
    
    if (outcome == "pneumonia") {
        outcomeranked <- outcomedataforstatermmv[order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name), ]
    }
    
    detach(outcomedataforstatermmv)
    
    if (class(num) != "character") {
        print(outcomeranked$Hospital.Name[num])
    }
    
    if (num == "best") {
        print(outcomeranked$Hospital.Name[1])        
    }
        
    if (num == "worst") {
        print(tail(outcomeranked$Hospital.Name, n = 1))        
    }
    
    
}