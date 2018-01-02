pollutantmean <- function(directory, pollutant, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate"

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return the mean of the pollutant across all monitors list 
	## in the 'id' vector (ignoring NA values)
	## NOTE: Do not round the result!
  
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
  
  ## setting work directory
  setwd("~/Documentos/R_studio_WD/Quiz02_data")
  calc_mean <- 0
  for (fileID in id){
    file_name <- convert_name(fileID)
    data <- read.csv(paste(directory,'/',file_name,'.csv', sep = ""))
    calc_mean <- calc_mean+mean(data[pollutant][!is.na(data[pollutant])])
  }
  calc_mean
}
