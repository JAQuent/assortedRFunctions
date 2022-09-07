insert_row <- function(index, df, newRow) {
  long <- nrow(df)
  newData<- rbind(df[1:index,], newRow ,df[(index + 1):(long),])
  return(newData)
} # Insert Row in index of dataframe

# Inspired by this post https://stackoverflow.com/questions/16249903/add-a-new-row-in-specific-place-in-a-dataframe
# This allows you to inset new row at a specific point in data frame in R
# Caution updates row names, which can be deleted via row.names() <- NULL
