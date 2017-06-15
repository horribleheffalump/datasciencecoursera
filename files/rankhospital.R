rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    ## Check that state and outcome are valid
    states <- unique(data$State)
    outcomes <- c("heart attack", "heart failure", "pneumonia")

    if (!(state %in% states)) {
        stop(message <- "invalid state")
    }
    if (!(outcome %in% outcomes)) {
        stop(message <- "invalid outcome")
    }

    state_data <- split(data_outcome, data_outcome$State)[[state]]

    if (outcome == "heart attack") {
        state_data <- state_data[!is.na(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
        ordered_state_data <- state_data[order(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), state_data$Hospital.Name),]$Hospital.Name
    } else if (outcome == "heart failure") {
        state_data <- state_data[!is.na(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
        ordered_state_data <- state_data[order(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), state_data$Hospital.Name),]$Hospital.Name
    } else {
        state_data <- state_data[!is.na(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
        ordered_state_data <- state_data[order(as.numeric(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), state_data$Hospital.Name),]$Hospital.Name
    }

    #print(ordered_state_data)
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (num == "best")
        ordered_state_data[1]
    else if (num == "worst")
        ordered_state_data[length(ordered_state_data)]
    else
        ordered_state_data[num]
}