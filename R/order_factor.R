# Created ordered factor
order_factor <- function(x, order_var, decreasing = FALSE, ordered = FALSE){
  f_x <- factor(x, levels = x[order(order_var, decreasing = decreasing)], ordered = ordered)
  return(f_x)
}
