insert_row <- function(index, df, newRow) {
  # Inspired by this post https://stackoverflow.com/questions/16249903/add-a-new-row-in-specific-place-in-a-dataframe
  # This allows you to inset ONE new row at a specific point in a data frame in R.
  # The row is always inserted at the index so that the previous row is moved one down.
  # Caution updates row names, which can be deleted via row.names() <- NULL

  # Get number of rows
  rows_df <- nrow(df)

  # Insert row
  newData<- rbind(df[0:(index - 1),], newRow ,df[(index):(rows_df),])
  return(newData)
}
