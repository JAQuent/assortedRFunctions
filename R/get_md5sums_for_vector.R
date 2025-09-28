get_md5sums_for_vector <- function(x){
  # Load the library if it is not loaded already
  if(!any(search() == "package:digest")){
    require(digest)
  }

  # Initialise vector
  hashes <- rep(NA_character_, length(x))

  # Loop through x
  for(i in 1:length(x)){
    hashes[i] <- digest(x[i], algo = "md5")
  }

  # Return
  return(hashes)
}
