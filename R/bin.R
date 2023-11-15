bin <- function(x, breaks){
  x_cut <- cut(x, breaks = breaks)
  return(factor(x_cut, levels = levels(x_cut), labels = 1:breaks))
}