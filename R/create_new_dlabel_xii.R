create_new_dlabel_xii <- function(mat, keys, key_labels, key_colours){
  # Use CAB_NP for now but soon create new xifti
  xii           <- CAB_NP_xii
  levels_old    <- xii$meta$cifti$labels[[1]]$Key
  old_level_num <- length(levels_old)

  # Change the data to mat
  xii_new <- newdata_xifti(xii, newdata = mat)

  # Prepare levels
  num_new_keys <- length(keys)
  new_levels   <- c(keys, rep(0, old_level_num - num_new_keys))
  new_labels   <- c(key_labels, rep("???", old_level_num - num_new_keys))

  # Convert to dlabel
  xii_new_dlabel <- convert_to_dlabel(xii_new, levels_old = levels_old,
                                      levels = new_levels, labels = new_labels,
                                      colors = key_colours)
  # Return
  return(xii_new_dlabel)
}
