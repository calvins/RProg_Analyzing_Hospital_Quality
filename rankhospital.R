# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv and returns a character vector with the name
# of the hospital that has the ranking specfied by the num argument. For example, the call
# rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death
# rate for heart failure. The num argument can take values "best", "worst", or an integer indicating the
# ranking (smaller numbers are better). If the number given by num is larger than the number of hospitals
# in that state, then the function should return NA. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name. For example, in Texas ("TX"),
# the hospitals with lowest 30-day mortality rate for heart failure are shown here.
# 
# > head(texas)
# Hospital.Name Rate Rank
# 3935 FORT DUNCAN MEDICAL CENTER 8.1 1
# 4085 TOMBALL REGIONAL MEDICAL CENTER 8.5 2
# 4103 CYPRESS FAIRBANKS MEDICAL CENTER 8.7 3
# 3954 DETAR HOSPITAL NAVARRO 8.7 4
# 4010 METHODIST HOSPITAL,THE 8.8 5
# 3962 MISSION REGIONAL MEDICAL CENTER 8.8 6
# 
# The function should check the validity of its arguments. If an invalid state value is passed to best,
# the function should throw an error via the stop function with the exact message "invalid state".
# If an invalid outcome value is passed to best, the function should throw an error via the stop function
# with the exact message "invalid outcome".
#
# source("rankhospital.R")
# UNIT TESTS
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA

rankhospital <- function(state, outcome, num = "best") {
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
        
    ## allocate space for results
    results <- vector("character")
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
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
        #         ss <- ss[order(ss$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", decreasing = FALSE),]
    }

    if (num == "best") {
        results <- ss[1,]
    } else if (num == "worst") {
        results <- ss[nrow(ss),]
    } else {
        results <- ss[num,]        
    }
    results <- results[1,1]
    results
}
