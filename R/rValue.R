rValue <-function(x){
  # To report a correlation coeffiecient in text
  if (inherits(x, "lm")){
    r.squared <- summary(x)$r.squared
    x.converted <- paste('=',substr(as.character(round(r.squared, 3)), 2,5))
  } else {
    if (x < 0){
      x.converted <- paste('= -',substr(as.character(abs(round(x, 3))), 2,5), sep = '')
    } else {
      x.converted <- paste('=',substr(as.character(abs(round(x, 3))), 2,5))
    }
  }
  return(x.converted)
}
