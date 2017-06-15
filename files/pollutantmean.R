pollutantmean <- function(directory, pollutant, id = 1:332) {
    p = vector()
    for (i in id) {
        if (i<10)
            filename <- paste("00", as.character(i), sep = "")
        else if (i<100)
            filename <- paste("0", as.character(i), sep = "")
        else
            filename <- as.character(i)
        filename <- paste(filename, ".csv", sep = "")
        #print(filename)
        data <- read.csv(paste(directory, "/", filename, sep = ""))
        pi <- data[[pollutant]]
        pi_masc <- is.na(pi)
        pi <- pi[!pi_masc]
        p = c(p,pi)
    }
    mean(p)
}