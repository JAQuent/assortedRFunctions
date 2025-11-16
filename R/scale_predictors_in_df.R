# Function to create scaled variables in data frame
scale_predictors_in_df <- function(df, variables){
  # Loop over all variables
  for(i in 1:length(variables)){
    # Create new column with "s_" prefix
    new_name <- paste0("s_", variables[i])

    # Get old variable and scale it
    x        <- df[, variables[i]]
    x_scaled <- scale_m0_sd0.5(x)

    # Add to data frame
    df[, new_name] <- x_scaled
  }

  # Return updated data frame
  return(df)
}
