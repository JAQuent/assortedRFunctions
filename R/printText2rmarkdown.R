printText2rmarkdown <- function(text){
  # Got it from https://stackoverflow.com/questions/29575768/r-markdown-format-text-in-code-chunk-with-new-lines
  cat(gsub(pattern = "\n", replacement = " \n", x = text))
}
