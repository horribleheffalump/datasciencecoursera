best <- function(state, outcome) {
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

    state_mask <- data_outcome$State == state
    if (outcome == "heart attack") {
        min_rate <- min(as.numeric(data_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[state_mask]), na.rm = TRUE)
        min_rate_mask <- as.numeric(data_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == min_rate
    } else if (outcome == "heart failure") {
        min_rate = min(as.numeric(data_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[state_mask]), na.rm = TRUE)
        min_rate_mask <- as.numeric(data_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == min_rate
    } else {
        min_rate = min(as.numeric(data_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[state_mask]), na.rm = TRUE)
        min_rate_mask <- as.numeric(data_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == min_rate
    }

    mask_united <- min_rate_mask & state_mask
    hospitals <- sort(data_outcome$Hospital.Name[mask_united])

    ## Return hospital name in that state with lowest 30-day death
    ## rate
    hospitals[1]

}