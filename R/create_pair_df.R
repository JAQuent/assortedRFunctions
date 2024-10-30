# This functions takes a data frame and expands it using expand.grid but also
# copies the values from the other columns when expanding it.
# Function below assumes that pair_column only contains unique values.
create_pair_df <- function(old_df, pair_column, delete_same_item_pairs = TRUE){
  # Get the names of all columns
  df_names <- names(old_df)
  other_columns <- df_names[df_names != pair_column]

  # Expand pair_column
  expanded <- expand.grid(old_df[, pair_column], old_df[, pair_column])

  # Create new data frame and add names with _1 and _2 suffix
  new_df <- as.data.frame(matrix(nrow = nrow(expanded), ncol = length(df_names) * 2))
  names(new_df) <- paste(rep(df_names, each = 2), rep(1:2, length(df_names)), sep = "_")

  # Replace the pair_column values with the expanded columns
  new_df[, paste0(pair_column, "_1")] <- expanded$Var1
  new_df[, paste0(pair_column, "_2")] <- expanded$Var2

  # Loop through each row and each column
  for(rowID in 1:nrow(new_df)){
    # Get the Boolean for the correct rows
    row_bool1 <- old_df[, pair_column] == new_df[rowID, paste0(pair_column, "_1")]
    row_bool2 <- old_df[, pair_column] == new_df[rowID, paste0(pair_column, "_2")]

    for(colID in 1:length(other_columns)){
      # Add _1 column value
      new_df[rowID, paste0(other_columns[colID], "_1")] <- old_df[row_bool1, other_columns[colID]]

      # Add _2 column value
      new_df[rowID, paste0(other_columns[colID], "_2")] <- old_df[row_bool2, other_columns[colID]]
    }
  }

  # Check if same item pairs should be deleted
  if(delete_same_item_pairs){
    same_pair_bool <- new_df[, paste0(pair_column, "_1")]  == new_df[, paste0(pair_column, "_2")]
    new_df <- new_df[!same_pair_bool, ]
  }

  # Return data frame
  return(new_df)
}
