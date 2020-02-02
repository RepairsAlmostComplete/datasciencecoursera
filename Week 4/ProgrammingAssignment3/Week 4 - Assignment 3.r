## ** Part 1 - Plot the 30-day Mortality Rates for Heart Attack **

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

outcome <- loadOutcomes()

## Create histogram of Heart Attack mortality rates
hist(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
     xlab = "Risk Adjusted Rate (%)",
     ylab = "Number of Hospitals",
     main = "30 Day Mortalitiy Rates from Heart Attack")

## ** Part 2 - Finding the Best Hospital in a State **
## Create a function to find the best hospital in the state.
## i.e. The hospitals with the lowest mortality rates for a particular outcome

best <- function(state, outcome) {
    ## Turn of those warnings that are not valid
    oldWarns <- getOption("warn")
    options(warn = -1)
    
    ## Read the data
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

## ** Part 3 - Ranking Hospitals by Outcome in a State

rankhospital <- function(state, outcome, num = "best") {
    ## Turn of those warnings that are not valid
    oldWarns <- getOption("warn")
    options(warn = -1)
    
    ## ** Read the data **
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
        if (num == "best") {
            num <- 1;
        } else if (num == "worst") {
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