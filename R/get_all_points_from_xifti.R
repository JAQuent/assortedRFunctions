get_all_points_from_xifti <- function(xifti){
  # Initialise the grayordinate values
  grayordinate_values <- c()

  # Loop through data list of the xifti object
  for(i in 1:length(xifti$data)){
    # Get the grayordinate values
    grayordinate_values <- c(grayordinate_values, xifti$data[[i]])
  }

  return(grayordinate_values)
}
