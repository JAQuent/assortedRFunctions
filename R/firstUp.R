firstUp <- function(x){
  # From https://stackoverflow.com/questions/18509527/first-letter-to-upper-case
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}