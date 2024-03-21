create_fileList <- function(path, recursive = FALSE){
  # Creates a list of all files/folders in a directory
  # so it can be pasted into a README file
  if(missing(path)){
    # Choosing the directory
    path <- choose.dir()
  }
  # Printing the location
  cat(paste0('Location: "', path, '"\n\n'))

  # Getting the file names
  fileList <- list.files(path, recursive = recursive)

  # Printing the files names
  cat('List of all files and folders:\n')
  cat(paste(fileList, sep = '\n', collapse = '\n'))

  # Return
  return(paste(fileList, sep = '\n', collapse = '\n'))
}
