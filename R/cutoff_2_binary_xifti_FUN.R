cut_off_2_binary_xifti_FUN <- function(x, cut_off, reverse = FALSE){
  if(reverse){
    ifelse(x < cut_off, 0, 1)
  } else {
    ifelse(x < cut_off, 1, 0)
  }
}
