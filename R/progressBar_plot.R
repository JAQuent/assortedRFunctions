progressBar_plot <- function(i, n){
  # Create a window
  if(i == 1){
    dev.new(width = 300, height = 500, unit = "px")
  }


  # Plot progress
  barplot(round((i/n)*100),
          main = "Progress",
          ylab = "Percentage completed",
          ylim = c(0, 100),
          col = 'red')

  # Close window
  if(i == n){
    dev.off()
  }
}

