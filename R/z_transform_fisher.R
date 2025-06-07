z_transform_fisher <- function(r, reverse = FALSE){
  # Z-transformation for correlation coefficient
  if(reverse){
    tanh(r)
  } else {
    log((1 + r) / (1 - r))/2
  }
}
