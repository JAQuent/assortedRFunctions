mean_SD_str <- function(m, s, digits, rounding_type){
  # This function takes a mean and a SD to create a string like 0.10 (.02) for tables
  if(missing(digits)){
    digits <- 2
  }

  # Default rounding
  if(missing(rounding_type)){
    rounding_type <- 'round'
  }

  # Rounding
  if(rounding_type == 'signif'){
   return_string <- paste0(signif(m, digits), ' (', signif(s, digits), ')')
  } else if (rounding_type == 'round'){
    return_string <- paste0(round(m, digits), ' (', round(s, digits), ')')
  } else {
    stop('Wrong rounding type. Choose signif or round.')
  }

  # Return
  return(return_string)
}
