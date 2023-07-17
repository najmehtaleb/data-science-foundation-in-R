corr <- function(directory, threshold=0){
  library(readr, dplyr)
  csv_files = list.files(paste0(getwd(), "/", directory))
  cr <- NULL
  for(file in csv_files){
    df = read_csv(paste0(getwd(),"/", directory, "/" , file), 
                  col_types = ("Dnni"))
    df2 = na.omit(df)
    if(dim(df2)[1] > threshold){
      temp <- cor(df$sulfate, df$nitrate, use="pairwise.complete.obs")
      cr <- append(cr, temp)
      problems(df)
    }
    }
  cr
}

