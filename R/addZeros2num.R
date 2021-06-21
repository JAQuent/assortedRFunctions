addZeros2num <- function(x, digits){
  if(nchar(x) == 1 & x > 0){
    # One digit integer
    result_string <- paste0(x, '.', paste0(rep('0', digits), collapse = ''))

  } else if (nchar(x) == 2 & x > 0){

  } else if (x == 0){
    result_string <- paste0(x, '.', paste0(rep('0', digits), collapse = ''))
  } else {
    result_string <- as.character(x)
  }

  return(result_string)
}

