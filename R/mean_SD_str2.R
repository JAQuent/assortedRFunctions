mean_SD_str2 <- function(x, type, digits, rounding_type, measure){
  # This function calculates the mean and a SD to create strings like this
  # Type 1 M = 0.49 (SD = 0.26)
  # Type 2 0.10 (.02)
  # rounding_type either round or signif (default)
  # Default value for digits
  if(missing(digits)){
    digits <- 2
  }

  # Default rounding
  if(missing(rounding_type)){
    rounding_type <- 'round'
  }

  # Default measure just empty
  if(missing(measure)){
    measure <- '' # First measure of string
  } else {
    measure <- paste0(" ", measure)
  }

  # Rounding
  if(rounding_type == 'signif'){
    m <- signif(mean(x, na.rm = TRUE), digits)
    s <- signif(sd(x, na.rm = TRUE), digits)
  } else if (rounding_type == 'round'){
    m <- round(mean(x, na.rm = TRUE), digits)
    s <- round(sd(x, na.rm = TRUE), digits)
  } else {
    stop('Wrong rounding type. Choose signif or round.')
  }

  # Create string
  if(type == 1){
    result_string <- paste0('M = ',m, measure, ' (SD = ', s, measure, ')')
  } else if (type == 2){
    result_string <- paste0(m, measure, ' (', s, measure, ')')
  } else {
    stop('Wrong type')
  }

  return(result_string)
}
