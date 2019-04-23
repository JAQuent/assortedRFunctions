createAnonStr <- function(strLength){
  charPool  <- c(as.character(0:9), LETTERS)
  return(paste(sample(charPool, strLength, replace = TRUE), sep = '', collapse = ''))
}
