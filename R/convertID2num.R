convertID2num <- function(ppid){
  # Get all unique IDs
  unique_ppid <- unique(ppid)

  # Number of IDS
  numIDs <- length(unique_ppid)

  # Calculate how many digits the number of unique ids has
  numDigits <- floor(log10(numIDs)) + 1

  # Create new variable
  new_ppid <- rep(NA, length(ppid))

  # Loop through unique_ppid
  for(i in 1:numIDs){
    new_ppid[ppid == unique_ppid[i]] <- sprintf(paste0("%0", numDigits,"d"), i)
  }

  return(new_ppid)
}
