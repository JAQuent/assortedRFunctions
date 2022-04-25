custom_rbinder_addIndexColumn <- function(x, columnName = "index"){
  # x = e.g. lapply(allFiles, read.csv)
  
  # How many entries does this list have?
  n <- length(x)
  
  # Loop trough all entries of the list
  for(i in 1:n){
    # Get DF from list
    tempDF <- x[[i]]
    
    # Create index DF and assign column name
    index <- data.frame(rep(i, times = nrow(tempDF)))
    names(index) <- columnName
    
    # Add index to custom column name
    tempDF <- cbind(tempDF, index)
    
    # Put the updated DF back into the list
    x[[i]] <- tempDF
  }
  
  # Bind rows
  x <- do.call(rbind, x)
  
  return(x)
}