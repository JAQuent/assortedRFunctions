# Function to visualise colour palettes to see if they work well as an overall
# scheme
visualise_colour_palette <- function(color_list, column_names) {
  # Determine the maximum number of colors in any vector
  max_colors <- max(sapply(color_list, length))

  # Create a data frame for plotting
  plot_data <- data.frame()

  for (i in seq_along(color_list)) {
    n_colors <- length(color_list[[i]])
    col_width <- 0.9  # Width of each color rectangle

    # Calculate positions with equal spacing and alignment
    xmin <- (i - 1) + (1 - col_width)/2
    xmax <- (i - 1) + col_width + (1 - col_width)/2

    # Create y positions centered in the column
    y_height <- 0.9 / max_colors  # Height of each color rectangle
    y_spacing <- (1 - (n_colors * y_height)) / (n_colors + 1)

    for (j in seq_along(color_list[[i]])) {
      ymin <- (j - 1) * (y_height + y_spacing) + y_spacing
      ymax <- ymin + y_height

      plot_data <- rbind(plot_data, data.frame(
        group = i,
        color = color_list[[i]][j],
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ))
    }
  }

  # Create the plot
  p <- ggplot(plot_data) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color),
              color = "white", linewidth = 0.5) +
    scale_fill_identity() +
    scale_x_continuous(breaks = seq_along(color_list) - 0.5,
                       labels = column_names,
                       expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    )

  return(p)
}

# Example usage:
# plasma_colours <- viridisLite::viridis(11, option = "plasma")
# baseColours4 <- plasma_colours[c(3, 5, 7, 9)]
# baseColours3 <- plasma_colours[c(3, 6, 9)]
# baseColours2 <- plasma_colours[c(3, 9)]
#
# colour_list <- list(plasma_colours, baseColours4, baseColours3, baseColours2)
# visualise_colour_palette(colour_list, c("gradient", "4 colours", "3 colours", "2 colours"))
