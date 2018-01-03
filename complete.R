complete <- function(directory, id = 1:332){
	## 'directory' is a character vector of length 1 indicating
	## the localization of the CSV files

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return a data frame of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the 
	## number of complete cases
    
    ## setting parent directory
    setwd("~/Documentos/Github_projects/Quiz02_Coursera")
    
    convert_name <- function(ID)
    { ## converts an ID into a readable name
        if(ID/10 < 1){
            name <- paste("00",ID, sep = "")
        }
        else if(ID/100 < 1){
            name <- paste("0",ID, sep = "")
        }
        else{
            name <- toString(ID)
        }
        name
    }
    
    ## initialize an empty data frame
    nobs <- rep(0L, length(id))
    nob_data <- data.frame(id,nobs)
    index <- 1
    for (file_id in id){
        ## read file dataframe
        file_name <- convert_name(file_id)
        data <- read.csv(paste(directory,'/',file_name,'.csv', sep = ""))
        
        ## identify amount of availiable data
        n_nobs <- length(data["ID"][!(is.na(data["sulfate"]) | is.na(data["nitrate"]))])
        nob_data[index,"nobs"] <- n_nobs
        index <- index+1
    }
    nob_data
}
