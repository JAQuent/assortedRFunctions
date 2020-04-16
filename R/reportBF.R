reportBF <- function(x, digits){
  # This function extracts a BF from a BFBayesFactor object and rounds it
  if(missing(digits)){
    digits <- 2
  }
  round(as.numeric(as.vector(x)), digits)
}