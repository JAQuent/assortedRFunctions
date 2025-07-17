# Can be used to prepare values to use for marginaleffects' datagrid
seq_min_2_max <- function(x, steps = 20){
  seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE), length.out = steps)
}
