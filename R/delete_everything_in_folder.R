delete_everything_in_folder <- function(path2folder, verbose = TRUE){
  # Check if the path ends on /
  lastChar <- substr(path2folder, nchar(path2folder), nchar(path2folder))
  if(lastChar == "/"){
    # Remove
    path2folder <- substr(path2folder, 1, nchar(path2folder)-1)
  }


  # List all files
  allFiles   <- list.files(path2folder, recursive = TRUE)
  allFolders <- list.dirs(path2folder, recursive = TRUE)

  # Exclude folder itself
  allFolders <- allFolders[allFolders != path2folder]

  # Remove all
  unlink(paste0(path2folder, "/", allFiles), recursive = TRUE)
  unlink(allFolders, recursive = TRUE)

  # Report
  if(verbose){
    cat("\n ", length(allFiles), " file(s) and ", length(allFolders), " folder(s) were deleted. \n")
  }
}
