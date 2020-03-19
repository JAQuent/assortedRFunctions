mean_SD_str <- function(m, s, digits){
  # This function takes a mean and a SD to create a string like 0.10 (.02) for tables
  if(missing(digits)){
    digits <- 2
  }
  return(paste0(round(m, digits), ' (', round(s, digits), ')'))
}
