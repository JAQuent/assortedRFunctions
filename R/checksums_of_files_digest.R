# Function 1: Using digest package
checksums_of_files_digest <- function(file_paths) {
  # Load required package
  if (!require(digest, quietly = TRUE)) {
    stop("Install digest package: install.packages('digest')")
  }

  # Calculate MD5 checksums using digest package
  sapply(file_paths, function(f) digest::digest(file = f, algo = "md5"))
}
