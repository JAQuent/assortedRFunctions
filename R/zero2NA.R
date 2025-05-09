# Function to set zero to NA
zero2NA <- function(x){
  return(ifelse(x == 0, NA, x))
}
