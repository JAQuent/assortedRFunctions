mad_outlier <- function(x, times = 2){
  # This function calculates the Median Absolute Deviation to detect outlier
  
  # Calculate lower and up limit with median and mad
  ll <- median(x) - times*mad(x)
  ul <- median(x) + times*mad(x)
  
  # Detect outlier
  outlier <- rep(0, length(x))
  outlier[x < ll] <- 1
  outlier[x > ul] <- 1
  
  return(outlier)
}