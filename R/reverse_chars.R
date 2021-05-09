# Taken from https://www.gastonsanchez.com/r4strings/reversing.html
# function that reverses a string by characters
reverse_chars <- function(string){
  # split string by characters
  string_split = strsplit(string, split = "")
  # reverse order
  rev_order = nchar(string):1
  # reversed characters
  reversed_chars = string_split[[1]][rev_order]
  # collapse reversed characters
  paste(reversed_chars, collapse = "")
}
