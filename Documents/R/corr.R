corr <- function(directory =  "Documents/R/specdata", threshold = 0) {
        id = 1:332
        ids_used = c()
        correlations = c()
        for (id_ind in id) {   
                if (complete(id = id_ind)[2] > threshold) {
                        ids_used = c(ids_used, id_ind)
                        idstr <- if (id_ind >= 100) {
                                idstr <- toString(id_ind)
                        } else if (id_ind >= 10) {
                                idstr <- paste("0", toString(id_ind), sep = "")
                        } else {
                                idstr <- paste("00", toString(id_ind), sep = "")
                        }
                        
                        filepath <- paste(directory, "/"
                                          , idstr, ".csv", sep = "")
                        mydata <- read.csv(file = filepath, header = TRUE, sep = ",")
                        correlation <- cor(mydata['sulfate'], mydata['nitrate'], use = 'complete.obs')
                        correlations <- c(correlations, correlation)
                }
        }
        correlations
}