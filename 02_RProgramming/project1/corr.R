corr <- function(directory, threshold = 0) {
#         setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\r programming\\pexercise1")
                if(grep("specdata", directory) == 1) {
                directory <- ("./specdata/")
        }
        # get the complete table
        complete_table <- complete2("specdata", 1:332)
        nobs <- complete_table$nobs
        # find the valid ids
        ids <- complete_table$id[nobs > threshold]
        # get the length of ids vector
        id_len <- length(ids)
        corr_vector <- rep(0, id_len)
        # find all files in the specdata folder
        all_files <- as.character( list.files(directory) )
        file_paths <- paste(directory, all_files, sep="")
        j <- 1
        for(i in ids) {
                current_file <- read.csv(file_paths[i], header=T, sep=",")
                corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
                j <- j + 1
        }
        result <- corr_vector
        return(result)   
        }