datedFileNam <- function(fileName, fileEnding){
  return(paste(fileName,
               '_',
               format(Sys.time(), '%Y%m%d_%H%M%S'),
               fileEnding,
               sep = ''))
}
