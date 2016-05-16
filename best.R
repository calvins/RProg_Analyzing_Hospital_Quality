# Write a function called best that take two arguments: the 2-character abbreviated name of a state and
# an outcome name. The function reads the outcome-of-care-measures.csv and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specfied outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of "heart attack", "heart failure", or "pneumonia". Hospitals that do not have data on a
# particular outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names
# should be sorted in alphabetical order and the first hospital in that set should be chosen
# (i.e. if hospitals "b", "c", and "f" are tied for best, then hospital "b" should be returned).
# 
# The function should check the validity of its arguments. If an invalid state value is passed to best,
# the function should throw an error via the stop function with the exact message "invalid state".
# If an invalid outcome value is passed to best, the function should throw an error via the stop
# function with the exact message "invalid outcome".

# UNIT TESTS
# source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome
#

best <- function(state, outcome) {
    ## Check that state and outcome are valid        
    validStates <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC", "FL","GA","HI","ID","IL","IN","IA","KS",
     "KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY",
     "NC","ND","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VT","VI","VA","WA","WV",
     "WI","WY","GU")
    validOutcomes <- c("heart attack", "pneumonia", "heart failure")    
    if (!state %in% validStates) {
        stop("invalid state")
    }
    if (!outcome %in% validOutcomes) {
        stop("invalid outcome")
    }
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ##allocate space for results
    results <- vector("character")
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    if (outcome == "heart attack") {
        ss <- subset(outcomeData[c(2, 7, 11)], (State == state) & (!outcomeData[11] == "Not Available"))
        ss[,3] <- as.numeric(ss[,3])
        ss <- ss[order(ss$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", ss$"Hospital.Name", decreasing = FALSE),]
    } else if (outcome == "heart failure") {
        ss <- subset(outcomeData[c(2, 7, 17)], (State == state) & (!outcomeData[17] == "Not Available"))
        ss[,3] <- as.numeric(ss[,3])
        ss <- ss[order(ss$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", ss$"Hospital.Name", decreasing = FALSE),]
    } else if (outcome == "pneumonia") {
        ss <- subset(outcomeData[c(2, 7, 23)], (State == state) & (!outcomeData[23] == "Not Available"))
        ss[,3] <- as.numeric(ss[,3])
        ss <- ss[order(ss$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", ss$"Hospital.Name", decreasing = FALSE),]
    }
    results <- ss[1,1]
    results
}
