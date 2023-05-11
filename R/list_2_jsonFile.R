list_2_jsonFile <- function(x, fileName){
  # Takes a list with named elements that are not nested and converts it into json
  numElements <- length(x)
  namElements <- names(x)
  
  # Create json file to write in
  sink(fileName)
  
  # Start of the file
  cat('{\n')
  
  # Function to convert element into string
  convert_listEntry_2_jsonValue <- function(entry){
    # Check if this element has length that is not zero
    if(length(entry) == 0){
      # Close file
      sink()
      stop('Error: One element of the list has a length of zero.')
    }
    
    # Check which class the entry is
    if(class(entry) == 'numeric' | class(entry) == "integer"){
      entry_string <- paste0(entry, collapse = ', ')
    } else if(class(entry) == 'logical'){
      entry_string <- paste0(entry, collapse = ', ')
      entry_string <- gsub(pattern = 'TRUE', replacement = 'true', entry_string)
      entry_string <- gsub(pattern = 'FALSE', replacement = 'false', entry_string)
    } else if(class(entry) == 'character'){
      entry_string <- paste0('"', paste0(entry, collapse = '", "'), '"')
      entry_string <- gsub(pattern = '\n', replacement = '\\\\n', entry_string)
    } else {
      # Close file
      sink()
      stop('Class not supported. ')
    }
    
  
    # Check if entry is longer than one in which case we need to [] around it
    if(length(entry) > 1){
      entry_string <- paste0("[", entry_string, "]")
    } 
    
    return(entry_string)
  }
  
  # Loop through all elements of the list
  for(i in 1:numElements){
    if(i < numElements){
      cat(paste0('\t"', namElements[i], '": ', convert_listEntry_2_jsonValue(x[[i]]), ',\n'))
    } else {
      cat(paste0('\t"', namElements[i], '": ', convert_listEntry_2_jsonValue(x[[i]]), '\n'))
    }
  }
  
  # End of the file
  cat('}\n')
  
  # Close file
  sink()
}
