mean_SD_str2 <- function(x, type, digits, rounding_type){
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

  # Rounding
  if(rounding_type == 'signif'){
    m <- signif(mean(x), digits)
    s <- signif(sd(x), digits)
  } else if (rounding_type == 'round'){
    m <- round(mean(x), digits)
    s <- round(sd(x), digits)
  } else {
    stop('Wrong rounding type. Choose signif or round.')
  }

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