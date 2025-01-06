rearrange_matrix <- function(mat, pattern, margin) {
  # Check if the input is a matrix
  if (!is.matrix(mat)) {
    stop("Input must be a matrix.")
  }

  # Check if the margin is either 1 (rows) or 2 (columns)
  if (!margin %in% c(1, 2)) {
    stop("Margin must be either 1 (for rows) or 2 (for columns).")
  }

  # Perform the rearrangement based on the specified margin
  if (margin == 1) {  # Rearrange rows
    # Check if the number of rows matches the pattern length
    if (nrow(mat) != length(pattern)) {
      stop("Number of rows in the matrix must match the pattern length.")
    }
    rearranged_mat <- mat[pattern, ]
  } else {  # margin == 2, Rearrange columns
    # Check if the number of columns matches the pattern length
    if (ncol(mat) != length(pattern)) {
      stop("Number of columns in the matrix must match the pattern length.")
    }
    rearranged_mat <- mat[, pattern]
  }

  return(rearranged_mat)
}
