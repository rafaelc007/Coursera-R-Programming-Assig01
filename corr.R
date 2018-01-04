corr <- function(directory, threshold = 0){
	## 'directory' is a character vector of length 1 indicating
	## the location of the CVS files

	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0

	## Return a numeric vector of correlations
	## NOTE: Do not round the result!
  
  ## setting parent directory
  setwd("~/Documentos/R_studio_WD/Quiz02_data")
  
  ## initialize an empty data frame
  cr_data <- numeric()
  index <- 1
  all_files <- list.files(paste(directory,'/', sep = ""))
  for (file_id in all_files){
      ## read file dataframe
      data <- read.csv(paste(directory,'/',file_id, sep = ""))
      
      ## identify what is the avaliable  sulfate data
      sulf_data <- data["sulfate"][!(is.na(data["sulfate"]) | is.na(data["nitrate"]))]
      
      if (length(sulf_data) > threshold){
        ## identify what is the avaliable nitrate data
        nitr_data <- data["nitrate"][!(is.na(data["sulfate"]) | is.na(data["nitrate"]))]
        
        ## calculate and store correlation
        cr_data[index] <- cor(sulf_data,nitr_data)
        index <- index+1
      }
  }
  cr_data
}
