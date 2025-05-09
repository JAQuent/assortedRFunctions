# Function to create thresholded maps
threshold_xifti <- function(target_xii, upper_cutOff, threshold_xii_list){
  # threshold_maps_list must be a list of XIFTIs

  # Create temporary list to save individually thresholded maps
  thresholded_xii_list <- list()

  # Loop through threshold_maps_list and set everything that's below the cut-off to NA
  for(i in 1:length(threshold_maps)){
    # Convert target_xii to matrix
    target_mat <- as.matrix(target_xii)
    threshold_mat   <- as.matrix(threshold_xii_list[[i]])

    # Set to everything below cut off to 0
    target_mat[threshold_mat[, 1] < upper_cutOff, 1] <- 0

    # Save to list
    thresholded_xii_list[[i]] <- newdata_xifti(target_xii, target_mat)
  }

  # Merge the XIFTI
  merged_xii <- merge_xifti(xifti_list = thresholded_xii_list)

  # Calculate the sum to add maps together
  summed_xii <- apply_xifti(merged_xii, margin = 1, sum)

  # Function to set zero to NA
  zero2NA <- function(x){
    return(ifelse(x == 0, NA, x))
  }

  # Set 0 values to NA
  return(apply_xifti(summed_xii, margin = 1, zero2NA))
}
