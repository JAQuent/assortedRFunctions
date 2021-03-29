# From https://stackoverflow.com/questions/18800108/password-generator-function-in-r
generate_password <- function(LENGTH){
  punct <- c("!",  "#", "$", "%", "&", "(", ")", "*",  "+", "-", "/", ":",
             ";", "<", "=", ">", "?", "@", "[", "^", "_", "{", "|", "}", "~")
  nums <- c(0:9)
  chars <- c(letters, LETTERS, punct, nums)
  p <- c(rep(0.0105, 52), rep(0.0102, 25), rep(0.02, 10))
  pword <- paste0(sample(chars, LENGTH, TRUE, prob = p), collapse = "")
  return(pword)
}
