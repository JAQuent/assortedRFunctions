brms_fixef_report <- function(x, digits = 2, rounding_type){

  # Default rounding
  if(missing(rounding_type)){
    rounding_type <- 'round'
  }

  # Rounding
  if(rounding_type == 'signif'){
    return_string <-   paste(' = ',
                             signif(x[1], digits),
                             ' (95 % CI [',
                             signif(x[3], digits),
                             ', ',
                             signif(x[4], digits),
                             '])' ,
                             sep = '')
  } else if (rounding_type == 'round'){
    return_string <-   paste(' = ',
                             round(x[1], digits),
                             ' (95 % CI [',
                             round(x[3], digits),
                             ', ',
                             round(x[4], digits),
                             '])' ,
                             sep = '')
  } else {
    stop('Wrong rounding type. Choose signif or round.')
  }

  # Return
  return(return_string)
}
