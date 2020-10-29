getFilename <- function() {
  # Get file name of script run via Rscript in terminal/console
  # Source: https://stackoverflow.com/questions/64575206/saving-history-for-script-run-rscript-through-terminal-console
  c_args <- commandArgs()
  r_file <- c_args[grepl("\\.R$", c_args, ignore.case = TRUE)]
  r_file <- gsub("--file=", "", r_file)
  r_file <- normalizePath(r_file)
  return(r_file)
}
