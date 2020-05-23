rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (sum(data[,7]==state)<1) {
        stop("invalid state")
    }
    
    if (sum(c("heart attack","heart failure","pneumonia")==outcome)<1) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    if (num == 'best') {
        num = 1
    }
    
    if (outcome == "heart attack") {
        data_new = data[data[,7]==state,c(2,11)]
        data_new = data_new[!is.na(as.numeric(data_new[,2])),]
        
        if (num == 'worst') {
            num = length(data_new[,1])
        }
        ret = data_new[order(as.numeric(data_new[,2]),data_new[,1]),][num,1]
        return(ret)
    }
    
    if (outcome == "heart failure") {
        data_new = data[data[,7]==state,c(2,17)]
        data_new = data_new[!is.na(as.numeric(data_new[,2])),]
        
        if (num == 'worst') {
            num = length(data_new[,1])
        }
        ret = data_new[order(as.numeric(data_new[,2]),data_new[,1]),][num,1]
        return(ret)
    }
    
    if (outcome == "pneumonia") {
        data_new = data[data[,7]==state,c(2,23)]
        data_new = data_new[!is.na(as.numeric(data_new[,2])),]
        
        if (num == 'worst') {
            num = length(data_new[,1])
        }
        ret = data_new[order(as.numeric(data_new[,2]),data_new[,1]),][num,1]
        return(ret)
    }
}