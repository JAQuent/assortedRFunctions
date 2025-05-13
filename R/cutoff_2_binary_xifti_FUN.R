cutoff_2_binary_xifti_FUN <- function(x, cutOff, reverse = FALSE){
  if(reverse){
    ifelse(x < cutOff, 0, 1)
  } else {
    ifelse(x < cutOff, 1, 0)
  }
}
