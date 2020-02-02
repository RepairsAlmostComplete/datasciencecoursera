## This function gets the best performing hospital for a given U.S. State and
## medical outcome
##
## It takes 2 arguments as described below:
## state        The U.S State 2 letter abbrievation
## outcome      The medical outcome:
##                  heart attack
##                  heart failure
##                  pneumonia

best <- function(state, outcome) {
    ## Turn of those warnings that are not valid
    oldWarns <- getOption("warn")
    options(warn = -1)
    
    ## Read the data
    source("loadOutcomes.r")
    outcomeData <- loadOutcomes()
    
    ## Turn the warnings back on
    options(warn = oldWarns)
    
    ## Check that state and outcome are valid
    ## Check if state exists first, if not then exit function with error
    state <- toupper(state) ## Convert state to upper case
    if (!(state %in% outcomeData$State)) {
        return(cat("Error in best(\"", state, "\", \"", outcome,
                   "\") : invalid state", sep = ""))
    }
    
    ## Remove data except from requested state
    outcomeData <- outcomeData[outcomeData$State == state, ]
    
    ## Check if the outcome is valid, if not then exit function with error
    outcome <- tolower(outcome) ## Convert outcome to lower case
    if (!(outcome) %in% c("heart attack", "heart failure", "pneumonia")) {
        return(cat("Error in best(\"", state, "\", \"", outcome,
                   "\") : invalid outcome", sep = ""))
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    
    ## Extract the relevant column corresponding the the requested outcome
    if (outcome == "heart attack"){
        selData <- data.frame(outcomeData$Hospital.Name, outcomeData$State,
                              outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    } else if (outcome == "heart failure") {
        selData <- data.frame(outcomeData$Hospital.Name, outcomeData$State,
                              outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    } else {
        selData <- data.frame(outcomeData$Hospital.Name, outcomeData$State,
                              outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    }
    
    ## Remove all incomplete cases from the dataset subset
    selData <- selData[complete.cases(selData), ]
    
    ## Find the lowest value in the dataset for the chosen outcome
    minOutcome <- min(selData[3], na.rm = TRUE)
    
    ## Find all the rows that have the same value as the lowest value found above
    minRows <- selData[selData[3] == minOutcome, ]
    
    ## Check if zero matches, one match, or more than one match is found
    if (nrow(minRows) == 0) {
        ## Zero matches, advise nothing found
        message("No results found matching requsted state and condition")
        NA
    } else if (nrow(minRows) == 1) {
        ## One match, return the match as a character vector with lenght of 1
        as.character(minRows$outcomeData.Hospital.Name)
    } else {
        ## More than one match found
        ## Sort into alphabetical order
        reordRows <- minRows[order(minRows[1]), ]
        as.character(reordRows[1, ]$outcomeData.Hospital.Name)
    }
}