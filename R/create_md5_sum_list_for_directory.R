create_md5_sum_list_for_directory <- function(directory, filename) {
  # Load required package
  if (!require(digest, quietly = TRUE)) {
    stop("Install digest package: install.packages('digest')")
  }

  # Clean directory path
  directory <- sub("/$", "", directory)

  # List all files
  all_files <- list.files(directory, recursive = TRUE, full.names = TRUE)
  all_files <- all_files[file_test("-f", all_files)]

  # Get relative paths
  rel_paths <- sub(paste0("^", directory, "/?"), "", all_files)

  # Calculate checksums
  checksums <- sapply(all_files, function(f) digest::digest(file = f, algo = "md5"))

  # Create and save data frame
  md5_df <- data.frame(relative_path = rel_paths, checksum = checksums,
                       stringsAsFactors = FALSE)
  write.csv(md5_df, filename, row.names = FALSE)

  cat("Saved checksums for", nrow(md5_df), "files to:", filename, "\n")
  invisible(md5_df)
}

# Usage create_md5_sum_list_for_directory(directory, filename)
