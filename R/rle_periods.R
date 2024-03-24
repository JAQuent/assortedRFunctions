# Function using run length encoding to calculate periods in the data that consecutively have the same state
# So A A B A would become 1 1 2 3
rle_periods <- function(x){
  # Run length encoding
  x_rle <- rle(x)

  # Get periods
  periods <- rep(1:length(x_rle$lengths), times = x_rle$lengths)

  # Return
  return(periods)
}
