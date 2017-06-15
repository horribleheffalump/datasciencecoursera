corr <- function(directory, threshold = 0) {
    p = vector(mode = "numeric")
    compl <- complete(directory)
    for (i in 1:nrow(compl)) {
        if (compl$nobs[i] > threshold) {
            id <- compl$id[i]
            if (id < 10)
                filename <- paste("00", as.character(id), sep = "")
            else if (id < 100)
                filename <- paste("0", as.character(id), sep = "")
            else
                filename <- as.character(id)
            filename <- paste(filename, ".csv", sep = "")
            #print(filename)
            data <- read.csv(paste(directory, "/", filename, sep = ""))
            sulfate_mask <- !is.na(data$sulfate)
            nitrate_mask <- !is.na(data$nitrate)
            and_mask <- sulfate_mask & nitrate_mask
            sulfate <- data$sulfate[and_mask]
            nitrate <- data$nitrate[and_mask]
            correlation_id = cor(sulfate, nitrate)
            #print(sum(count))
            p = c(p, correlation_id)
        }
    }
    p
}