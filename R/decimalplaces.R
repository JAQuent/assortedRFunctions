decimalplaces <- function(x){
  if(!check.integer(x)){
    string1 <- as.character(x)
    string2 <- format(x, scientific = FALSE, digits = 15)

    if(nchar(string1) > nchar(string2)){
      return(nchar(unlist(strsplit(string1, "[.]"))[2]))
    } else {
      return(nchar(unlist(strsplit(string2, "[.]"))[2]))
    }
  } else {
    return(0)
  }
}
