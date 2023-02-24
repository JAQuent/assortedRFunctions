calc_KL_D <- function(states2analyse, predictedStates){
  # Check if all states in states2analyse are also included in predictedStates
  if(any(!(states2analyse %in% predictedStates))){
    stop("There are states in states2analyse that are not included in predictedStates.")
  }

  # Number of states that were present
  presentStates <- unique(predictedStates)
  k             <- length(presentStates)

  # Initialise pj & qj
  pj <- rep(0, k)
  qj <- rep(0, k)

  # Loop through presentStates to calculate probability
  for(j in 1:k){
    pj[j] <- sum(states2analyse == presentStates[j])/length(states2analyse)
    qj[j] <- sum(predictedStates == presentStates[j])/length(predictedStates)
  }

  # Exclude zero-values because the log of 0 is -Inf
  qj <- qj[pj > 0] # Order is important
  pj <- pj[pj > 0]

  # Log transform with base of 2 to make units bits
  log_qj_by_pj <- log2(qj/pj)

  # Return Kullback–Leibler divergence value
  return(-sum((pj*log_qj_by_pj)))
}
