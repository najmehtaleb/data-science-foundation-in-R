rankall <- function(outcome, num = "best"){
  
  # [2]  "Hospital.Name" 
  # [7]  "State"
  # [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  # [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
  # [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
  
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  column <- if(outcome=="heart attack"){11}
  else if(outcome=="heart failure"){17}
  else if(outcome=="pneumonia") {23}
  else {stop("invalid outcome")}
  
  df[, column] <- suppressWarnings(as.numeric(df[, column]))
  
  df2 <- df[, c(2, 7, column)]
  df3 <- lapply( split(df2, df2$State), function(lis) { lis[order(lis[3], lis[1], na.last = NA), ] })
  
  f <- function(lis){
    if (num=="best") {num <- 1}
    else if(num=="worst") {num <- dim(lis)[1]}
    lis[num, c(1, 2)]
    
  }
  
  # df4 <- lapply(df3, function(lis) {lis[num, c(1, 2)]})
  
  df4 <- lapply(df3, f)
  
  df4

  
}