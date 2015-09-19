#library(dplyr)

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files_full <- list.files(directory, full.names=TRUE)
  #print(files_full)
  
  dat <- data.frame()
  nobs <- data.frame()
  

  
  for (i in id) {
    #dat <- rbind(dat, read.csv(files_full[i]))
    dat <- read.csv(files_full[i])
    good <- complete.cases(dat)
    cnt <- nrow(dat[good,])
    nobs <- rbind(nobs, c(i, cnt))
  }
  
  colnames(nobs) <- c("id", "nobs")
  #print(nobs)
  nobs
  #dat <- rbind(dat, read.csv(files_full[1]))
  
  #dat
  
  #nobs <- data.frame(..., row.names = c("id", "nobs"))
  #names(nobs) <- c("id", "nobs")
  
  #print(names(nobs)[2:3])
  
  #bad <- is.na(dat[,2:3])
  #print(bad)  
  #good <- dat[!bad]
  #print(good)
  
  #print(nrow(dat))
  #print(nrow(bad))
  #print(nrow(good))
  
  #dat[complete.cases(dat[,2:3]),]
  #nrow(dat)
  
  #good <- complete.cases(dat)
  #print(dat[good, ])
  #print(nrow(dat[good,]))
}
