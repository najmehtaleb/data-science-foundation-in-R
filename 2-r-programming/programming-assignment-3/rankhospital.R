rankhospital <- function(state, outcome, num){
  
  # [2]  "Hospital.Name" 
  # [7]  "State"
  # [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  # [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
  # [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
  
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!any(state==unique(df$State))){
    stop("invalid state")}
  
  column <- if(outcome=="heart attack"){11}
  else if(outcome=="heart failure"){17}
  else if(outcome=="pneumonia") {23}
  else {stop("invalid outcome")}
  
  df[, column] <- suppressWarnings(as.numeric(df[, column]))
  
  splitted_state <- split(df[, c(2, 7, column)], df$State)[state]
  df2 <- data.frame(splitted_state[[1]][1] , splitted_state[[1]][2] , splitted_state[[1]][3])
  df3 <- df2[order(df2[,3], df2[,1], na.last = NA), ]
  
  if(num=="best"){val <- df3[1, ][1]}
  else if (num=="worst") {val <- df3[dim(df3)[1], ][1]}
  else {val <- df3[num, ][1]}
  
  val[[1]]
}