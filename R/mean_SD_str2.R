mean_SD_str2 <- function(x, type, digits){
  # This function calculates the mean and a SD to create strings like this
  # Type 1 M = 0.49 (SD = 0.26)
  # Type 2 0.10 (.02)
  if(missing(digits)){
    digits <- 2
  }
  m <- round(mean(x), digits)
  s <- round(sd(x), digits)

  # Create string
  if(type == 1){
    result_string <- paste0('M = ',round(m, digits), ' (SD = ', round(s, digits), ')')
  } else if (type == 2){
    result_string <- paste0(round(m, digits), ' (', round(s, digits), ')')
  } else {
    stop('Wrong type')
  }

  return(result_string)
}
