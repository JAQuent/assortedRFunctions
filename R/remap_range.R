remap_range <- function(x, targetRange){
  x           <- x - min(x)
  x_range     <- range(x)
  rangeScalar <- abs(targetRange[1] - targetRange[2])/x_range[2]
  y           <- rangeScalar*x + targetRange[1]
  return(y)
}
