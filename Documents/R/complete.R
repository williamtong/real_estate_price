complete <- function(directory =  "Documents/R/specdata", id = 1:332) {
        nobs <- c()
        readings <- vector(mode="numeric", length=0)
        for (id_ind in id) {   
                idstr <- if (id_ind >= 100) {
                        idstr <- toString(id_ind)
                        } else if (id_ind >= 10) {
                                idstr <- paste("0", toString(id_ind), sep = "")
                                } else {
                                        idstr <- paste("00", toString(id_ind), sep = "")
                                        }
                filepath <- paste(directory, "/", idstr, ".csv", sep = "")
                mydata <- read.csv(file = filepath, header = TRUE, sep = ",")
                filledinrows <- sum(as.integer(!is.na(mydata['nitrate'])) * as.integer(!is.na(mydata['sulfate'])))
                nobs <- c(nobs, filledinrows)
        }
        nobs_dataframe <- data.frame(id, nobs)
        nobs_dataframe
}


