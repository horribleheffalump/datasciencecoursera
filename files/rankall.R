rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    ## Check that state and outcome are valid
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% outcomes)) {
        stop(message <- "invalid outcome")
    }
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    state_data <- split(data_outcome, data_outcome$State)
    state_data <- lapply(state_data, function(d) {
        if (outcome == "heart attack") {
            d <- d[!is.na(as.numeric(d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
            ordered_d <- d[order(as.numeric(d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), d$Hospital.Name),]$Hospital.Name
        } else if (outcome == "heart failure") {
            d <- d[!is.na(as.numeric(d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
            ordered_d <- d[order(as.numeric(d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), d$Hospital.Name),]$Hospital.Name
        } else {
            d <- d[!is.na(as.numeric(d$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
            ordered_d <- d[order(as.numeric(d$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), d$Hospital.Name),]$Hospital.Name
        }
        if (num == "best")
            ordered_d[1]
        else if (num == "worst")
            ordered_d[length(ordered_d)]
        else
            ordered_d[num]
    })

    state_data
#print(ordered_state_data)
## Return hospital name in that state with the given rank
## 30-day death rate


}