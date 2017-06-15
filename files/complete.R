complete <- function(directory, id = 1:332) {
    p = vector()
    for (i in id) {
        if (i < 10)
            filename <- paste("00", as.character(i), sep = "")
        else if (i < 100)
            filename <- paste("0", as.character(i), sep = "")
        else
            filename <- as.character(i)
        filename <- paste(filename, ".csv", sep = "")
        #print(filename)
        data <- read.csv(paste(directory, "/", filename, sep = ""))
        sulfate_mask <- !is.na(data$sulfate)
        nitrate_mask <- !is.na(data$nitrate)
        and_mask <- sulfate_mask & nitrate_mask
        count <- as.integer(and_mask)
        #print(sum(count))
        p = c(p, sum(count))
    }
    df <- data.frame(id = id, nobs = p)
    df
}