voronoi_tesselation_grid_binning_2d <- function(x, y, xLim, yLim, numSeeds, shape = "hexagon"){
  # This function bins spatial 2D data via voronoi tesselation, where x & y are coordinate values, xLim & yLim are the
  # limit values for the tessellation seeds and numSeeds is the number of seeds. There are two possible
  # shapes 'square' & 'hexagon'. For hexagons, the rows are shifted a bit slightly extending the xLim values.
  # How this function works: First, tessellation seeds are created and then each point is binned by finding the closest
  # seed (estimated by Euclidean distance)
  ### Checks of input
  if(numSeeds %% 2 != 0){
    stop("numSeeds is not an even number")
  }

  ### Generate the tessellation seeds from the input
  # Step 1: Generate the range of value for both dimensions
  xRange     <- seq(from = xLim[2], to = xLim[1], length.out = numSeeds)
  yRange     <- seq(from = yLim[2], to = yLim[1], length.out = numSeeds)
  seeds      <- data.frame(x = xRange[1], y = yRange)

  # Step 1: If necessary calculate shift value to generate hexagon-shaped bin
  if(shape == "hexagon"){
    xStepSize  <- xRange[1] - xRange[2] # Space between the points on the x-axis
    shiftValue <- xStepSize/4 # The value by which to shift the rows to get hexagon-shaped bins
  } else if(shape == "square"){
    seeds      <- data.frame(x = xRange[1], y = yRange)
    shiftValue <- 0
  } else {
    stop("Unkown bin shape")
  }
  # Step 3: Creating the alternating index for shifting
  shiftIndex <- rep(c(1, -1), numSeeds/2)

  # step 4: Shift 1st column
  seeds$x <- seeds$x + shiftIndex*shiftValue

  # Step 5: Loop to create the other columns and shift them laterally if necessary
  for(i in 2:length(xRange)){
    seeds <- rbind(seeds, data.frame(x = xRange[i] + shiftIndex*shiftValue, y = yRange))
  }

  # Step 5: Add bin information to seeds
  seeds$bin <- row.names(seeds)

  ### Find the closest seed and label it accordingly
  # Step 1: Distance function
  euclidDist <- function(x1, y1, x2, y2){
    sqrt((x1-x2)^2 + (y1 - y2)^2)
  }

  # Step 2:  Prepare the labels
  binLabels <- rep(NA, length(x))

  # Step 3: Loop through the points and find bins based on distance
  for(i in 1:length(x)){
    # Calculate distances
    dists <- euclidDist(x[i], y[i], seeds$x, seeds$y)

    # Save labels
    binLabels[i] <- seeds$bin[dists == min(dists)]
  }

  # Return
  return(binLabels)
}
