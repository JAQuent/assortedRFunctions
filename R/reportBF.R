reportBF <- function(x, digits, rounding_type){
  # This function extracts a BF from a BF BayesFactor object and rounds it
  if(missing(digits)){
    digits <- 2
  }

  # Default rounding
  if(missing(rounding_type)){
    rounding_type <- 'round'
  }

  # Rounding
  if(rounding_type == 'signif'){
    signif(as.numeric(as.vector(x)), digits)
  } else if (rounding_type == 'round'){
    round(as.numeric(as.vector(x)), digits)
  } else {
    stop('Wrong rounding type. Choose signif or round.')
  }

}
