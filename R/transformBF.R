transformBF <- function(bf){
  # This function transforms BF for plotting evidence for H1 and H0 on same scale
  if(bf < 1){
    -1/bf + 1
  } else if(bf == 1){
    0
  } else {
    bf - 1
  }
}