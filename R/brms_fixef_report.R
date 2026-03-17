brms_fixef_report <- function(x, digits = 2, rounding_type, pre_string){

  # Default rounding
  if(missing(rounding_type)){
    rounding_type <- 'round'
  }

  # Default pre string
  if(missing(pre_string)){
    pre_string <- ''
  }

  # Rounding
  if(rounding_type == 'signif'){
    return_string <-   paste0(pre_string, ' = ',
                             signif(x[1], digits),
                             ' (95 % CI [',
                             signif(x[3], digits),
                             ', ',
                             signif(x[4], digits),
                             '])' )
  } else if (rounding_type == 'round'){
    return_string <-   paste0(pre_string, ' = ',
                             round(x[1], digits),
                             ' (95 % CI [',
                             round(x[3], digits),
                             ', ',
                             round(x[4], digits),
                             '])')
  } else {
    stop('Wrong rounding type. Choose signif or round.')
  }

  # Return
  return(return_string)
}
