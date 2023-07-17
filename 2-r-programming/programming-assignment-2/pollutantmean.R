pollutantmean <- function(directory, pollutant, id=1:332){
  library(readr)
  csv_files <- list.files(paste0(getwd(), "/", directory))
  df <- read_csv(paste0(getwd(),"/", directory, "/", csv_files[id]), 
                 col_types = ("Dnni"))
  mean <- mean(df[[pollutant]], na.rm=TRUE)
  mean
}