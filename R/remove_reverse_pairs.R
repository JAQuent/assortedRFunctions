remove_reverse_pairs <- function(df){
  # Df must be created like df <- expand.grid(var, var)
  # Keep A B but remove B A pairs
  df$id <- 1:nrow(df)
  df <- ddply(df, c("id"), mutate, dub_finder  = paste(sort(c(Var1, Var2)), collapse = "_"))
  df <- df[!duplicated(df$dub_finder), ]

  # Remove ID and dub_finder
  df$id <- NULL
  df$dub_finder <- NULL

  # Return
  return(df)
}
