## ** Part 3 - Ranking Hospitals by Outcome in a State
## This function gets the best performing hospital for rank and outcome from
## the selected U.S. State.
##
## It takes 2 arguments as described below:
## state        The U.S State 2 letter abbrievation
## outcome      The medical outcome:
##                  heart attack
##                  heart failure
##                  pneumonia
## num          Requested rank

rankhospital <- function(state, outcome, num = "best") {
    ## Turn of those warnings that are not valid
    oldWarns <- getOption("warn")
    options(warn = -1)
    
    ## ** Read the data **
    source("loadOutcomes.r")
    outcomeData <- loadOutcomes()
    
    ## Turn the warnings back on
    options(warn = oldWarns)
    
    ## ** Check that state and outcome are valid **
    
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
    
    ## ** Return hospital name in that state with the given rank 30-day death rate **
    
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
    
    ## Handle if num set to "best" or "worst"
    if (class(num) == "character") {
        if (tolower(num) == "best") {
            num <- 1;
        } else if (tolower(num) == "worst") {
            num <- nrow(selData)
        }
    }
    
    ## Check that num does not exceed the number of hospitals in the dataset
    ## after incomplete cases removed
    if (num > nrow(selData)) {
        return(NA)
    }
    
    ## Re-order dataset by Risk and then Hospital Name alphabetically
    selData <- selData[order(selData[3], selData[1]), ]
    
    ## Select the requested rank
    as.character(selData[num, ]$outcomeData.Hospital.Name)
}