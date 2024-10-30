# This function can calculate periods based on run length encoding
calc_periods <- function(x){
  # Do run length encoding
  x_rle <- rle(x)

  # Periods and return
  return(rep(1:length(x_rle$lengths), times = x_rle$lengths))
}
