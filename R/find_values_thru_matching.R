find_values_thru_matching <- function(sourceID, sourceVar, targetID){
  # Number of values to find in other DF
  nVals <- length(targetID)

  # Initialise variable where the results will be saved
  targetVar <- rep(NA, nVals)

  # Loop through target_ID to find all values
  for(i in 1:nVals){
    # Check if current ID is in source_ID (otherwise current targetVar will stay NA)
    if(any(sourceID == targetID[i])){
      # Select the corresponding entry in sourceVar
      targetVar[i] <- sourceVar[sourceID == targetID[i]]
    }
  }

  return(targetVar)
}
