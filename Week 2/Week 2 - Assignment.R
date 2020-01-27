# **** Week 2 - Programming Assignment ****
#
# Using the specdata.zip file. Each csv file in this zip file has the following
# columns:
# Date, sulfate, nitrate, ID
#
# The format/unit for each column is:
# Date: YYYY-MM-DD
# sulfate: µg/m^3
# nitrate: µg/m^3
# ID: ID Number of the monitoring station
#
# Each CSV file contains data for one station
#
# Part 1: Write a function named 'pollutantmean' that calculates the mean of a
# pollutant (sulfate or nitrate) across a speficied list of monitors.

leadZeros <- function(inStr, len){
    ## This function pads a string with leading zeros
    ##
    ## 'instr' is a character vector of lenght 1 passed into the function with
    ## the number that is to be padded
    ##
    ## 'len' is a number vector with a length of 1 for the number of zeros to
    ## add to the start of 'inStr'
    
    if (nchar(inStr) < len){
        for (i in 1:(len - nchar(inStr))){
            inStr <- paste("0", inStr, sep = "");
        }
    }
    inStr
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating the location
    ## of the CSV files
    ##
    ## 'pollutant' is a character vector of lenght 1 indicating the name of the
    ## pollutant for which we will calculate the mean; either "sulfate" or
    ## "nitrate".
    ##
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    ##
    ## Return the mean of the pollutant across all monitors listed in the 'id'
    ## vector (ignoring NA values)
    ##
    ## NOTE: The result is not rounded.

    ## We need a variable to store the cumulate values for pollutant
    pollVals <- c();
    
    ## We need to iterate through the id's, load the corresonding CSV file
    ## calculating the mean for each requested station.
    for (currID in id){
        ## First we need to load the CSV file
        csvName <- paste(directory, "/", leadZeros(currID, 3), ".csv", sep = "");
        pollData <- read.csv(csvName, header = TRUE, sep = ",")
        
        ## Now we need to extract the relevent column
        reqPoll <- pollData[, pollutant];
        
        ## Remove the NA's
        goodPoll <- complete.cases(reqPoll);
        reqPoll <- reqPoll[goodPoll];
        
        ## Add pollutant values to the pollVals vector
        pollVals <- c(pollVals, reqPoll);
    }
    
    ## Finally, we need to calculate the overall mean for the requested stations
    ## if more than one station requested.
    mean(pollVals)
}

# Write a function that reads a directory fill of files and reports the number
# of completly observed cases in each data file.
# The function should return a data frame where the first column is the name of
# the file, and the second column is the number of complete cases.

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating the location of
    ## the CSV files
    ##
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    ##
    ## Return a data frame of the form:
    ## id  nobs
    ## 1   117
    ## 2   1041
    ##
    ## Where 'id' is the monitor ID number and 'nobs' is the number of complete
    ## cases
    
    ## We need a data frame to put the data in
    compEntries <- data.frame(id = numeric(),
                              nobs = numeric());
    
    ## Itterate through each of the defined files
    for (currID in id){
        ## First we need to load the CSV file
        csvName <- paste(directory, "/", leadZeros(currID, 3), ".csv", sep = "");
        pollData <- read.csv(csvName, header = TRUE, sep = ",")
        
        ## Find the complete entries
        goodPoll <- complete.cases(pollData);
        
        ## Count complete cases
        sumGood <- sum(goodPoll);
        
        ## Add to compEntries
        compEntries <- rbind(compEntries,
                             data.frame(id = currID, nobs = sumGood));
    }
    
    compEntries
}

## DEBUG
complete("specdata", 1:10)