## This function loads in the data from the "outcome-of-care-measures.csv" file
## and sets the class of each column appropriately

loadOutcomes <- function(){
    ## Read the csv data
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
    
    ## Capture the column names so we can re-assign them post coercing the numeric
    ## data columns to numeric class
    colNameOutcome <- names(outcome)
    
    ## Coerce the numeric columns to the numeric class
    outcome <- data.frame(
        outcome[,1:7],
        as.numeric(outcome[, 8]),
        outcome[, 9:10],
        as.numeric(outcome[, 11]),
        outcome[, 12],
        as.numeric(outcome[, 13]),
        as.numeric(outcome[, 14]),
        as.numeric(outcome[, 15]),
        outcome[, 16],
        as.numeric(outcome[, 17]),
        outcome[, 18],
        as.numeric(outcome[, 19]),
        as.numeric(outcome[, 20]),
        as.numeric(outcome[, 21]),
        outcome[, 22],
        as.numeric(outcome[, 23]),
        outcome[, 24],
        as.numeric(outcome[, 25]),
        as.numeric(outcome[, 26]),
        as.numeric(outcome[, 27]),
        outcome[, 28],
        as.numeric(outcome[, 29]),
        outcome[, 30],
        as.numeric(outcome[, 31]),
        as.numeric(outcome[, 32]),
        as.numeric(outcome[, 33]),
        outcome[, 34],
        as.numeric(outcome[, 35]),
        outcome[, 36],
        as.numeric(outcome[, 37]),
        as.numeric(outcome[, 38]),
        as.numeric(outcome[, 39]),
        outcome[, 40],
        as.numeric(outcome[, 41]),
        outcome[, 42],
        as.numeric(outcome[, 43]),
        as.numeric(outcome[, 44]),
        as.numeric(outcome[, 45]),
        outcome[, 46]
    )
    
    names(outcome) <- colNameOutcome
    
    outcome
}