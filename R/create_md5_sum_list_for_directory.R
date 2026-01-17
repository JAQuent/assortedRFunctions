check_md5_sum_list_for_directory <- function(directory, filename = "md5_checksums.csv") {
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
    # Create absolute paths for common files
    common_files_absolute <- file.path(directory, common)

    # Use the same method as creation for verification
    if (.Platform$OS.type == "unix") {
      # Unix: Use parallel md5sum
      cat("Verifying with parallel md5sum...\n")
      current_checksums <- checksums_of_files_md5sum_parallel(common_files_absolute)
    } else {
      # Windows: Use digest package
      cat("Verifying with digest package...\n")
      current_checksums <- checksums_of_files_digest(common_files_absolute)
    }

    # Match saved checksums by relative path
    saved_checksums <- saved$checksum[match(common, saved$relative_path)]

    # Find changed files
    changed <- common[saved_checksums != current_checksums]
  }

  # Print results
  cat("\nVerification results for:", directory, "\n")
  cat("Missing files:", length(missing), "\n")
  if (length(missing) > 0) cat(paste(" ", missing, collapse = "\n"), "\n")

  cat("Extra files:", length(extra), "\n")
  if (length(extra) > 0) cat(paste(" ", extra, collapse = "\n"), "\n")

  cat("Changed files:", length(changed), "\n")
  if (length(changed) > 0) cat(paste(" ", changed, collapse = "\n"), "\n")

  # Return summary
  return(invisible(list(
    all_match = (length(missing) == 0 & length(extra) == 0 & length(changed) == 0),
    missing = missing,
    extra = extra,
    changed = changed
  )))
}
