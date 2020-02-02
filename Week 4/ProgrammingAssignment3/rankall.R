## ** Part 4 - Ranking hospitals in all states **
## This function gets the a list of hospitals for each state that meet that rank.
##
## It takes 2 arguments as described below:
## outcome      The medical outcome:
##                  heart attack
##                  heart failure
##                  pneumonia
## num          Requested rank

rankall <- function(outcome, num = "best") {
    ## Turn of those warnings that are not valid
    oldWarns <- getOption("warn")
    options(warn = -1)
    
    ## ** Read the data **
    source("loadOutcomes.r")
    outcomeData <- loadOutcomes()
    
    ## Turn the warnings back on
    options(warn = oldWarns)
    
    ## ** Check that the outcome is valid **
    
    ## Check if the outcome is valid, if not then exit function with error
    outcome <- tolower(outcome) ## Convert outcome to lower case
    if (!(outcome) %in% c("heart attack", "heart failure", "pneumonia")) {
        return(cat("Error in best(\"", state, "\", \"", outcome,
                   "\") : invalid outcome", sep = ""))
    }
    
    ## Get all states abbrievations
    states <- unique(outcomeData$State)
    
    dataOut <- data.frame(hospital = character(), state = character())
    
    for (state in states) {
        ## Remove data except from requested state
        stateData <- outcomeData[outcomeData$State == state, ]
        
        ## ** Return Return a data frame with the hospital names and the **
        ## ** abbreviated state name                                     **
        
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
            if (num == "best") {
                num <- 1;
            } else if (num == "worst") {
                num <- nrow(selData)
            }
        }
        
        ## Check that num does not exceed the number of hospitals in the dataset
        ## after incomplete cases removed
        if (num > nrow(selData)) {
            hospital <- NA
        }
        
        ## Re-order dataset by Risk and then Hospital Name alphabetically
        selData <- selData[order(selData[3], selData[1]), ]
        
        ## Select the requested rank
        hospital <- as.character(selData[num, ]$outcomeData.Hospital.Name)
        
        ## Create a data.frame f
        
        ## Add to the dataFrame dataOut
        dataOut <- rbind(dataOut, c(hospital, state))
    }
    
    dataOut
}