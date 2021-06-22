mean_SD_str <- function(m, s, digits, rounding_type, measure){
  # This function takes a mean and a SD to create a string like 0.10 (.02) for tables
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
   return_string <- paste0(signif(m, digits), measure, ' (', signif(s, digits), measure, ')')
  } else if (rounding_type == 'round'){
    return_string <- paste0(round(m, digits), measure, ' (', round(s, digits), measure, ')')
  } else {
    stop('Wrong rounding type. Choose signif or round.')
  }

  # Return
  return(return_string)
}
