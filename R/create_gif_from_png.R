create_gif_from_png <- function(png_files,
                                output_file = "animation.gif",
                                delay = 10,
                                resize = NULL) {

  # Check required packages
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The 'magick' package is required. Please install it with install.packages('magick')")
  }

  if (length(png_files) == 0) {
    stop("No PNG files found in the specified directory with the given pattern.")
  }

  # Sort files naturally (so frame_2 comes after frame_1, not frame_10)
  png_files <- png_files[order(gsub("[^0-9]", "", png_files))]

  message(paste("Found", length(png_files), "PNG files to process..."))

  # Read images
  img_list <- lapply(png_files, magick::image_read)

  # Combine images
  animation <- magick::image_animate(
    magick::image_join(img_list),
    delay = delay,
    optimize = TRUE
  )

  # Apply resize if requested
  if (!is.null(resize)) {
    animation <- magick::image_resize(animation, paste0(resize, "%"))
  }

  # Set looping
  animation <- magick::image_write(animation,
                                   path = output_file,
                                   quality = 100)

  message(paste("GIF successfully created:", output_file))
}

# Usage: create_gif_from_png(png_files, output_file = "my_animation.gif", delay = 15)
