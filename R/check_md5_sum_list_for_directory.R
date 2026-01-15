check_md5_sum_list_for_directory <- function(directory, filename) {
  # Load required package
  if (!require(digest, quietly = TRUE)) {
    stop("Install digest package: install.packages('digest')")
  }

  # Check inputs
  if (!file.exists(filename)) stop("File not found: ", filename)
  if (!dir.exists(directory)) stop("Directory not found: ", directory)

  # Clean directory path
  directory <- sub("/$", "", directory)

  # Read saved checksums
  saved <- read.csv(filename, stringsAsFactors = FALSE)

  # Get current files
  current_files <- list.files(directory, recursive = TRUE, full.names = TRUE)
  current_files <- current_files[file_test("-f", current_files)]

  # Get relative paths
  current_rel <- sub(paste0("^", directory, "/?"), "", current_files)

  # Find differences
  missing <- setdiff(saved$relative_path, current_rel)
  extra   <- setdiff(current_rel, saved$relative_path)

  # Check checksums for common files
  common  <- intersect(saved$relative_path, current_rel)
  changed <- character(0)

  if (length(common) > 0) {
    for (file in common) {
      saved_checksum <- saved$checksum[saved$relative_path == file]
      current_checksum <- digest::digest(file = file.path(directory, file),
                                         algo = "md5")
      if (saved_checksum != current_checksum) {
        changed <- c(changed, file)
      }
    }
  }

  # Print results
  cat("\nVerification results for:", directory, "\n")
  cat("Missing files:", length(missing), "\n")
  if (length(missing) > 0) cat(paste(" ", missing, collapse = "\n"), "\n")

  cat("Extra files:", length(extra), "\n")
  if (length(extra) > 0) cat(paste(" ", extra, collapse = "\n"), "\n")

  cat("Changed files:", length(changed), "\n")
  if (length(changed) > 0) cat(paste(" ", changed, collapse = "\n"), "\n")
}

# Usage check_md5_sum_list_for_directory(directory, filename)
