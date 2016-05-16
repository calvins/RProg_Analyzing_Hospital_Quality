# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital
# ranking (num). The function reads the outcome-of-care-measures.csv and returns a 2-column data frame
# containing the hospital in each state that has the ranking specfied in num. For example the function
# call rankall("heart attack", "best") would return a data frame containing the names of the hospitals
# that are the best in their respective states for 30-day heart attack death rates. The function should
# return a value for every state (some may be NA). The first column in the data frame is named hospital,
# which contains the hospital name, and the second column is named state, which contains the 2-character
# abbreviation for the state name. Hospitals that do not have data on a particular outcome should be
# excluded from the set of hospitals when deciding the rankings.
# 
# Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
# that the rankhospital function handles ties.
# 
# The function should check the validity of its arguments. If an invalid outcome value is passed to
# rankall, the function should throw an error via the stop function with the exact message
# "invalid outcome". The num variable can take values "best", "worst", or an integer indicating the
# ranking (smaller numbers are better). If the number given by num is larger than the number of hospitals
# in that state, then the function should return NA.
#
# source("rankall.R")
# UNIT TESTS
# > head(rankall("heart attack", 20), 10)
# hospital state
# AK <NA> AK
# AL D W MCMILLAN MEMORIAL HOSPITAL AL
# AR ARKANSAS METHODIST MEDICAL CENTER AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
# CA SHERMAN OAKS HOSPITAL CA
# CO SKY RIDGE MEDICAL CENTER CO
# CT MIDSTATE MEDICAL CENTER CT
# DC <NA> DC
# DE <NA> DE
# FL SOUTH FLORIDA BAPTIST HOSPITAL FL
# > tail(rankall("pneumonia", "worst"), 3)
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
# WV PLATEAU MEDICAL CENTER WV
# WY NORTH BIG HORN HOSPITAL DISTRICT WY
# > tail(rankall("heart failure"), 10)
# hospital state
# TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
# TX FORT DUNCAN MEDICAL CENTER TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
# VA SENTARA POTOMAC HOSPITAL VA
# VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
# VT SPRINGFIELD HOSPITAL VT
# WA HARBORVIEW MEDICAL CENTER WA
# WI AURORA ST LUKES MEDICAL CENTER WI
# WV FAIRMONT GENERAL HOSPITAL WV
# WY CHEYENNE VA MEDICAL CENTER WY

rankall <- function(outcome, num = "best") {
    ## Check that outcome is valid
    validOutcomes <- c("heart attack", "pneumonia", "heart failure")    
    if (!outcome %in% validOutcomes) {
        stop("invalid outcome")
    }
    
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ##allocate space for results
    results <- data.frame()
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    if (outcome == "heart attack") {
        ss <- subset(outcomeData[c(2, 7, 11)], (!outcomeData[11] == "Not Available"))
        states <- as.list(unique(ss[,2]))
        ss[,3] <- as.numeric(ss[,3])
        for (s in states) {
            # get hospitals for s and rank 
            sss <- subset(ss, (ss[2] == s))
            sss <- sss[order(sss$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", sss$"Hospital.Name", decreasing = FALSE),]            
            # add hospital by num into results
            # if NA add state and NA
            if (num == "best") {
                if (is.na(sss[1,1])) {
                    results <- rbind(results, data.frame(Hospital.Name="<NA>", State=s))
                } else {
                    results <- rbind(results, sss[1,1], sss[1,2])                    
                }
            } else if (num == "worst") {
                if (is.na(sss[nrow(sss),1])) {
                    results <- rbind(results, data.frame(Hospital.Name="<NA>", State=s))
                } else {
                    results <- rbind(results, sss[nrow(sss),1], sss[nrow(sss),2])                    
                }
            } else {
                if (is.na(sss[num,1]))  {
                    results <- rbind(results, data.frame(Hospital.Name="<NA>", State=s))
                } else {
                    results <- rbind(results, sss[num, 1:2])                    
                }                
            }
        }
        
    } else if (outcome == "heart failure") {
        ss <- subset(outcomeData[c(2, 7, 17)], (!outcomeData[17] == "Not Available"))
        states <- as.list(unique(ss[,2]))
        ss[,3] <- as.numeric(ss[,3])
        for (s in states) {
            #            get hospitals for s and rank 
            sss <- subset(ss, (ss[2] == s))
            sss <- sss[order(sss$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", sss$"Hospital.Name", decreasing = FALSE),]            
            # add hospital by num into results
            # if NA add state and NA
            if (num == "best") {
                if (is.na(sss[1,1])) {
                    results <- rbind(results, data.frame(Hospital.Name = "<NA>", State = s))
                } else {
                    results <- rbind(results, sss[1,1:2])                    
                }
            } else if (num == "worst") {
                if (is.na(sss[nrow(sss),1])) {
                    results <- rbind(results, data.frame(Hospital.Name = "<NA>", State = s))
                } else {
                    results <- rbind(results, sss[nrow(sss),1:2])                    
                }
            } else {
                if (is.na(sss[num,1]))  {
                    results <- rbind(results, data.frame(Hospital.Name = "<NA>", State = s))
                } else {
                    results <- rbind(results, sss[num,1:2])                    
                }                
            }
        }        
    } else if (outcome =="pneumonia") {
        ss <- subset(outcomeData[c(2, 7, 23)], (!outcomeData[23] == "Not Available"))
        states <- as.list(unique(ss[,2]))
        ss[,3] <- as.numeric(ss[,3])
        for (s in states) {
            #            get hospitals for s and rank 
            sss <- subset(ss, (ss[2] == s))
            sss <- sss[order(sss$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", sss$"Hospital.Name", decreasing = FALSE),]            
            # add hospital by num into results
            # if NA add state and NA
            if (num == "best") {
                if (is.na(sss[1,1])) {
                    results <- rbind(results, data.frame(Hospital.Name = "<NA>", State = s))
                } else {
                    results <- rbind(results, sss[1,1:2])                    
                }
            } else if (num == "worst") {
                if (is.na(sss[nrow(sss),1])) {
                    results <- rbind(results, data.frame(Hospital.Name = "<NA>", State = s))
                } else {
                    results <- rbind(results, sss[nrow(sss),1:2])                    
                }
            } else {
                if (is.na(sss[num,1]))  {
                    results <- rbind(results, data.frame(Hospital.Name = "<NA>", State = s))
                } else {
                    results <- rbind(results, sss[num,1:2])                    
                }                
            }
        }
    }
    results <- results[order(results$"State", decreasing = FALSE),]
    names(results) <- c("hospital", "state")
    results
}
