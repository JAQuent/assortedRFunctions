recalculate_FDR_4_contrasts <- function(xii_uncp_list){
  # This function expects xifti objects with uncorrected p-values.
  # Example: xii_uncp_list <- list(p1 = GLM1_pMap1_xii, p2 = GLM1_pMap2_xii)

  ########### Get the points from the xifits
  log_uncp_values <- c()
  xifti_index     <- c()
  for(i in 1:length(xii_uncp_list)){
    # Get values from this xifti
    temp_values <- get_all_points_from_xifti(xii_uncp_list[[i]])

    # Create index and add to xifti_index so we can easily tell to which xifti the values belong
    xifti_index <- c(xifti_index, rep(i, length(temp_values)))

    # Add extracted values to log_uncp_values
    log_uncp_values <- c(log_uncp_values, get_all_points_from_xifti(xii_uncp_list[[i]]))
  }

  ########### Convert, correct & convert
  # Convert from -log10(p) back to raw p-value
  uncp_values     <- 10^-(log_uncp_values)

  # Calculate FDR correction over all values
  fdrp_values     <- p.adjust(uncp_values, method = "fdr")

  # Convert raw p-value to -log10(p)
  log_fdrp_values <- -log10(fdrp_values)

  ########### Create list of update xiftis
  # Create new list
  xii_fdrp_list <- xii_uncp_list

  # Loop over all xiftis
  for(i in 1:length(xii_fdrp_list)){
    # Get the current xifti
    current_xii <- xii_fdrp_list[[i]]

    # Get values meant for current xifti
    current_values <- log_fdrp_values[xifti_index == i]

    # Get indices for each brain part for the current xifti
    brainpart_index <- c()
    for(j in 1:length(current_xii$data)){
      # Get the grayordinate values
      brainpart_index <- c(brainpart_index, rep(j, length(current_xii$data[[j]])))
    }

    # Replace the values in the xifti object
    for(j in 1:length(current_xii$data)){
      # Subset current_values for this brain part only
      current_values_for_brainpart <- current_values[brainpart_index == j]

      # Get the number of rows
      n <- length(current_values_for_brainpart)

      # Get the grayordinate values
      xii_fdrp_list[[i]]$data[[j]] <- matrix(current_values_for_brainpart, nrow = n, ncol = 1)
    }
  }

  ########### Return
  return(xii_fdrp_list)
}


