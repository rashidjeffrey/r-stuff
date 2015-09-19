corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  # Write a function that takes a directory of data files 
  # and a threshold for complete cases 
  #
  # and calculates the correlation between sulfate and nitrate 
  # for monitor locations 
  # where the number of completely observed cases (on all variables) 
  # is greater than the threshold. 
  #
  # The function should return a vector of correlations for the monitors 
  # that meet the threshold requirement. 
  #
  # If no monitors meet the threshold requirement, 
  # then the function should return a numeric vector of length 0.
  

  files_full <- list.files(directory, full.names=TRUE)
  file_cnt <- length(files_full)
  #print(file_cnt)
  
  #nobs <- data.frame()
  #nobs <- complete(directory, i)
  #nobs <- complete(directory)
  
  my_vector <- vector()
  
  for (i in 1:file_cnt) {
    
    nobs <- complete(directory, i)
    #print(nobs[,2])
    
    # where the number of completely observed cases (on all variables) 
    # is greater than the threshold...
    if (nobs[,2] > threshold) {

      # monitor does meets the threshold requirement...
      # so calculate the correlation between sulfate and nitrate 
      dat <- read.csv(files_full[i])
      good <- complete.cases(dat)
      my_cor <- cor(dat[good,2], dat[good,3])
      #print(my_cor)
      
      # return a vector of correlations for the monitors 
      my_vector <- c(my_vector, my_cor)
    } else {
      # monitor does not meet the threshold requirement...
      # so return a numeric vector of length 0.
      #print(0)
    }
    
   
  }
  
  my_vector
  
}