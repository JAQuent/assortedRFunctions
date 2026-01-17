checksums_of_files_md5sum_parallel <- function(file_paths) {
  # Load required package
  if (!require(parallel, quietly = TRUE)) {
    stop("Install parallel package: install.packages('parallel')")
  }

  # Detect number of cores
  n_cores <- detectCores() - 1
  n_cores <- max(1, n_cores)  # At least 1 core

  # Split files into chunks for parallel processing
  chunks <- split(file_paths, cut(seq_along(file_paths), breaks = n_cores, labels = FALSE))

  # Process each chunk in parallel
  results <- mclapply(chunks, function(chunk) {
    # Create temporary file with file list
    tmp <- tempfile()
    writeLines(chunk, tmp)

    # Run md5sum on the chunk
    output <- system(paste("xargs -a", tmp, "md5sum 2>/dev/null"), intern = TRUE)
    unlink(tmp)

    return(output)
  }, mc.cores = n_cores)

  # Combine and parse results
  output <- unlist(results)

  # Parse output: split on spaces and take first part (checksum)
  checksums <- sapply(strsplit(output, " "), function(x) x[1])

  return(checksums)
}
