changecrit <- function(x, crit){
  # This function is design to change the criterion for sequential bayesian simulation
  # Get every point where it's over crit
  overCrit <- which(x > crit)
  
  # Return value
  if(length(overCrit) > 0){
    # Return earliest point where over crit if there is a point at all
    return(min(overCrit))
  } else {
    # If no point is over crit, return last
    return(length(x))
  }
}
