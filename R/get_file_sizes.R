get_file_sizes <- function(path, sortResult = TRUE){
  # Creates a list of all files/folders in a directory
  # so it can be pasted into a README file
  if(missing(path)){
    # Choosing the directory
    path <- choose.dir()
  }

  # Get all files
  allFiles      <- data.frame(file = list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE))
  allFiles$size <- file.size(allFiles$file)/1000000

  # Sort if true
  if(sortResult){
    allFiles <- allFiles[order(allFiles$size, decreasing = TRUE), ]
  }

  # Return
  return(allFiles)
 }
