get_all_points_from_xifti <- function(xifti){
  c(xifti$data$cortex_left, xifti$data$cortex_right, xifti$data$subcort)
}