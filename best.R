best <- function(state, outcome) {
  ## Read outcome data
  dat <- read.csv("/home/rashid/R/coursera/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(is.na(state)) {
    stop("invalid state")
  }
    
  if(is.na(outcome)) {
    stop("invalid outcome")
  }
    
  ## Return hospital name in that state with lowest 30-day death rate
  
  #The hospital name is the name provided in the Hospital.Name variable. 
  #The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”.
  
  
  
  if (outcome == 'heart attack') {
    #x <- outcome[, 13]
    #best <- as.numeric(outcome_data[, 13])
    #out <- dat[, c(2,13)]
    out <- dat[, c(2,11)]
  } else if (outcome == 'heart failure') {
    #x <- outcome[, 19]
    #best <- as.numeric(outcome_data[, 19])
    #out <- dat[, c(2,19)]
    out <- dat[, c(2,17)]
  } else if (outcome == 'pneumonia') {
    #x <- outcome[, 25]
    #best <- as.numeric(outcome_data[, 25])
    #out <- dat[, c(2,25)]
    out <- dat[, c(2,23)]
  }
  
  good <- complete.cases(out)
  stuff <- out[good,]
  #dim(stuff)
  #stuff[order(stuff[,2]),] 
  
  sort.stuff <- stuff[order(2, 1), ]
  


  head(stuff)
  
  #Handling ties. If there is a tie for the best hospital for a given outcome, 
  #then the hospital names should be sorted in alphabetical order 
  #and the first hospital in that set should be chosen 
  #(i.e. if hospitals “b”, “c”,                                                                                     
  #and “f” are tied for best, then hospital “b” should be returned).
  
}
