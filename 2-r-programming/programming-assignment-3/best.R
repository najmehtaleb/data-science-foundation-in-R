best <- function(state, outcome){
  
  # [2]  "Hospital.Name" 
  # [7]  "State"
  # [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  # [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
  # [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
  
  library(dplyr)
  
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!any(state==unique(df$State))){
    stop("invalid state")}

  column <- if(outcome=="heart attack"){11}
    else if(outcome=="heart failure"){17}
    else if(outcome=="pneumonia") {23}
    else {stop("invalid outcome")}
  
  
  df[, column] <- suppressWarnings(as.numeric(df[, column]))
  
  st <- split(df[, c(2, 7, column)], df$State)[state]
  min_val <- min(st[[1]][3], na.rm = TRUE)
  temp <- filter(df, df[, 7]==state & df[, column]==min_val)
  names <- temp[,2]
  sorted_names <- sort(names)
  names[1]

}