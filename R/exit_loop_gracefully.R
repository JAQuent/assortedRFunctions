exit_loop_gracefully <- function(fileName, startIndex = 0, i = 0) {
  cat("\nSaving results so far...\n")
  if(startIndex > 0){
    cat("Start   index =", startIndex, "\n")
  }
  if(i > 0){
    cat("Current index =", i, "\n")
  }
  save.image(fileName)
}
