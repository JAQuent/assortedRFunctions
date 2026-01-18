create_md5_sum_list_for_directory <- function(directory, filename = "md5_checksums.csv") {
  # Clean directory path
  directory <- sub("/$", "", directory)

  # List all files
  all_files <- list.files(directory, recursive = TRUE, full.names = TRUE)
  all_files <- all_files[file_test("-f", all_files)]

  if (length(all_files) == 0) {
    warning("No files found")
    return(NULL)
  }

  # Get relative paths
  rel_paths <- sub(paste0("^", directory, "/?"), "", all_files)

  # Choose method based on operating system
  if (.Platform$OS.type == "unix") {
    # Unix/Linux/Mac: Use parallel md5sum
    cat("Using parallel md5sum (Unix system)...\n")
    checksums <- checksums_of_files_md5sum_parallel(all_files)
  } else {
    # Windows: Use digest package
    cat("Using digest package (Windows system)...\n")
    checksums <- checksums_of_files_digest(all_files)
  }

  # Create and save data frame
  md5_df <- data.frame(relative_path = rel_paths, checksum = checksums,
                       stringsAsFactors = FALSE)
  write.csv(md5_df, filename, row.names = FALSE)

  cat("Saved checksums for", nrow(md5_df), "files to:", filename, "\n")
  invisible(md5_df)
}
