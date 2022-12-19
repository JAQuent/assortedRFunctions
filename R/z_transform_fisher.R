z_transform_fisher <- function(r){
  # Z-transformation for correlation coeffecient
  log((1 + r) / (1 - r))/2
}
