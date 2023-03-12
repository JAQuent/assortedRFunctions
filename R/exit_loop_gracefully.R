exit_loop_gracefully <- function(fileName) {
  cat("\nSaving results so far...\n")
  cat("Start   index =", startIndex, "\n")
  cat("Current index =", i, "\n")
  save.image(fileName)
}
