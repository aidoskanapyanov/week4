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
}