convert_matrix_2_heatmap_df <- function(x){
  return(data.frame(row = rep(1:nrow(x), ncol(x)),
                    col = rep(1:ncol(x), each = nrow(x)),
                    value = c(x)))
}

