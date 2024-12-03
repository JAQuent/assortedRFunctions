# This a much more perfomant version than remove_reverse_pairs.R but it for it to
# work we need to use "stringsAsFactors = FALSE" see
# pairs <- expand.grid(Var1 = c("image1.jpg", "image2.png", "image3.bmp"),
#                      Var2 = c("image1.jpg", "image2.png", "image3.bmp"),
#                      stringsAsFactors = FALSE)

remove_symmetric_pairs <- function(df) {
  df <- df[df$Var1 < df$Var2, ]  # Keep only rows where Var1 < Var2
  return(df)
}
