replace_all_subs <- function(vec2change, oldLabels, newLabels){
  # Replaces all entries in vec2change based on the oldLabels by
  # replacing them with newLabels. Useful for anonymise() if the same should
  # be done for more than 1 data frame.
  # For this to work oldLabels and newLabels need same ordering

  # Convert all to character
  vec2change <- as.character(vec2change)
  oldLabels  <- as.character(oldLabels)
  newLabels  <- as.character(newLabels)

  # Get unique values from vec2change
  vec2change_uni <- unique(vec2change)

  changed_vec <- rep(NA, length(vec2change))
  # Loop through all entries
  for(i in 1:length(vec2change_uni)){
    # Which 2 change?
    label2change <- vec2change_uni[i]

    # Replace
    changed_vec[vec2change == label2change] <-newLabels[oldLabels == label2change]
  }
  return(changed_vec)
}
