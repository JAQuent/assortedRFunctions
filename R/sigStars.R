sigStars <- function(x){
  # Adding stars to indicate significance
  stars <- rep("", length(x))
  stars[x <= 0.1   & x > 0.05]   <- '.' # trend
  stars[x <= 0.05  & x > 0.01]   <- '*'
  stars[x <= 0.01  & x > 0.001]  <- '**'
  stars[x <= 0.001]              <- '***'
  return(stars)
}
