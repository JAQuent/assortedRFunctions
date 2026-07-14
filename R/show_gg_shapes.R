# Show ggplot2 point shapes in a 5x5 grid
show_gg_shapes <- function() {
  df <- expand.grid(col = 1:5, row = 1:5)
  df$shape <- 1:25

  ggplot(df, aes(x = col, y = -row)) +
    geom_point(aes(shape = factor(shape)), size = 3) +
    scale_shape_manual(values = 1:25) +
    geom_text(aes(label = shape), hjust = -1, size = 2.5) +
    theme_void() +
    theme(legend.position = "none")
}
