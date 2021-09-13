arcsine_transform <- function(x, reverse = FALSE){
  if(reverse){
    (sin(x) + 1)/2
  } else {
    if(any(x < 0) | any(x > 1)){
      stop('Value not between zero and one. Either forgot reverse = FALSE or wrong value. ')
    } else {
      asin(x*2 - 1)
    }
  }
}
