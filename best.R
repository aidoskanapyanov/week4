best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (sum(data[,7]==state)<1) {
        stop("invalid state")
    }
    
    if (sum(c("heart attack","heart failure","pneumonia")==outcome)<1) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    if (outcome == "heart attack") {
        # lowest heart attack mortality
        min_heart = min(as.numeric(data[data[,7]==state,11]), na.rm = TRUE)
        ret = levels(factor(data[data[,7]==state,][as.numeric(data[data[,7]==state,11])==min_heart,2]))[1]
        return(ret)
    }
    
    if (outcome == "heart failure") {
        # lowest heart failure mortality
        min_heart = min(as.numeric(data[data[,7]==state,17]), na.rm = TRUE)
        ret = levels(factor(data[data[,7]==state,][as.numeric(data[data[,7]==state,17])==min_heart,2]))[1]
        return(ret)
    }
    
    if (outcome == "pneumonia") {
        # lowest pneumonia mortality
        min_heart = min(as.numeric(data[data[,7]==state,23]), na.rm = TRUE)
        ret = levels(factor(data[data[,7]==state,][as.numeric(data[data[,7]==state,23])==min_heart,2]))[1]
        return(ret)
    }
}