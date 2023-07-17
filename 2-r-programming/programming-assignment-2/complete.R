complete <- function(directory, id=1:332){
  library(readr, dplyr)
  csv_files <- list.files(paste0(getwd(), "/", directory))
  nobs = NULL
  for (i in id){
    df <- read_csv(paste0(getwd(),"/", directory, "/", csv_files[i]), 
                   col_types = ("Dnni"))
    nob <- filter(df, !is.na(df$sulfate) & !is.na(df$nitrate))
    nobs <- append(nobs, dim(nob)[1])
  }
  df <- data.frame(nobs, id)
  df
}