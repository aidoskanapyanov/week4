rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    
    if (sum(c("heart attack","heart failure","pneumonia")==outcome)<1) {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    if (num == 'best') {
        num = 1
    }
    
    if (outcome == "heart attack") {
        data_new = data[,c(2,7,11)]
        data_new[,3] = as.numeric(data_new[,3])
        data_new = data_new[!is.na(data_new[,3]),]
        
        data_new = lapply(split(data_new,data_new[,2]), function(x) x[order(x[,3],x[,1]),])
        
        if (num == 'worst') {
            hospital = sapply(data_new, function(x) x[length(x[,1]),1])
        }
        else{
            hospital = sapply(data_new, function(x) x[num,1])   
        }
        df = data.frame(hospital = hospital,state = names(data_new))
        return(df)
    }
    
    if (outcome == "heart failure") {
        data_new = data[,c(2,7,17)]
        data_new[,3] = as.numeric(data_new[,3])
        data_new = data_new[!is.na(data_new[,3]),]
        
        data_new = lapply(split(data_new,data_new[,2]), function(x) x[order(x[,3],x[,1]),])
        
        if (num == 'worst') {
            hospital = sapply(data_new, function(x) x[length(x[,1]),1])
        }
        else{
            hospital = sapply(data_new, function(x) x[num,1])   
        }
        df = data.frame(hospital = hospital,state = names(data_new))
        return(df)
    }
    
    if (outcome == "pneumonia") {
        data_new = data[,c(2,7,23)]
        data_new[,3] = as.numeric(data_new[,3])
        data_new = data_new[!is.na(data_new[,3]),]
        
        data_new = lapply(split(data_new,data_new[,2]), function(x) x[order(x[,3],x[,1]),])
        
        if (num == 'worst') {
            hospital = sapply(data_new, function(x) x[length(x[,1]),1])
        }
        else{
            hospital = sapply(data_new, function(x) x[num,1])   
        }
        df = data.frame(hospital = hospital,state = names(data_new))
        return(df)
    }
}