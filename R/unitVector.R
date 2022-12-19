unitVector <- function(x){
  # Function to convert to unit vector
  x / sqrt(sum(x^2))
}
