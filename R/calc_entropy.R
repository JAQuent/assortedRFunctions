calc_entropy <- function(states2analyse){
  # Number of states that were present
  presentStates <- unique(states2analyse)
  k             <- length(presentStates)

  # Initialise pj
  pj <- rep(0, k)

  # Loop through presentStates to calculate probability
  for(j in 1:k){
    pj[j] <- sum(states2analyse == presentStates[j])/length(states2analyse)
  }

  # Log transform with base of 2 to make units bits
  log_pj <- log2(pj)

  # Return entropy value
  return(-sum((pj*log_pj)))
}
