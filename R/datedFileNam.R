datedFileNam <- function(fileName, fileEnding){
  string <- paste0(fileName,
                  '_',
                  format(Sys.time(), '%Y%m%d_%H%M%S'),
                  fileEnding)

  # Print string
  cat(paste("\r Filename created:", string, "\n"))
  return(string)
}
