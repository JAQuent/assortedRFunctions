custom_rbinder_addSubject <- function(x){
  # x = e.g. lapply(allFiles, read.csv)
  
  # How many entries does this list have?
  n <- length(x)
  
  # Loop trough all entries of the list
  for(i in 1:n){
    # Get DF from list
    tempDF <- x[[i]]
    
    # Add subject
    tempDF$subject <- as.character(i)
    
    # Put the updated DF back into the list
    x[[i]] <- tempDF
  }
  
  # Bind rows
  x <- do.call(rbind, x)
  
  return(x)
}