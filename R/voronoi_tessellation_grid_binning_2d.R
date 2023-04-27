voronoi_tessellation_grid_binning_2d <- function(x, y, limValues, numSeeds, shape = "hexagon", useParallelisation = FALSE){
  # This function bins spatial 2D data via Voronoi tessellation, where x & y are coordinate values, limValues (same for x & y) are the
  # limit values for the tessellation seeds and numSeeds desrcibe the number of seeds used for the x-axis. The y-values are set slightly
  # smaller *(sqrt(3)/2) to make sure that the lattice is in fact made up of equilateral triangles. There are two possible
  # shapes 'square' & 'hexagon'. For hexagons, the rows are shifted a bit slightly extending the xLim values and y step size is adjusted.
  # How this function works: First, tessellation seeds are created and then each point is binned by finding the closest
  # seed (estimated by Euclidean distance). Set useParallelisation to TRUE to use doParallel and foreach to speed things up.
  # In this case detectCores() - 2 cores are used.
  # Sort limit values
  xLim <- sort(limValues)
  yLim <- sort(limValues)

  # Calculate byValue from numSeeds
  byValue   <- ((xLim[2] - xLim[1])/(numSeeds - 1))

  ### Check if useParallelisation & then prepare everything
  if(useParallelisation){
    # Check if packages are attached
    if(!any(search() == "package:doParallel")){
      require(doParallel)
    }
    if(!any(search() == "package:foreach")){
      require(foreach)
    }

    # Create the cluster following https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
    my.cluster <- parallel::makeCluster(detectCores() - 2, type = "PSOCK")

    #register it to be used by %dopar%
    doParallel::registerDoParallel(cl = my.cluster)
  }

  ### Generate the tessellation seeds from the input
  # Step 1: Generate the range of value for both dimensions
  xRange     <- seq(from = xLim[1], to = xLim[2], by = byValue)

  # Step 2: If necessary calculate shift value to generate hexagon-shaped bin
  if(shape == "hexagon"){
    # Calculate the step size that is used in x and the the value by which reach row needs shifted to the side
    xStepSize    <- xRange[2] - xRange[1] # Space between the points on the x-axis
    x_shiftValue <- xStepSize/4 # The value by which to shift the rows to get hexagon-shaped bins

    # Based on this calculate the y-values that are needed to make equilateral triangles
    yRange <- seq(from = yLim[1], to = yLim[2], by = xStepSize * (sqrt(3)/2))

    # Only selected as many y-values as there are x-values
    yRange <- yRange[1:length(xRange)]

    # Centre the y-values
    yRange       <- yRange + (yLim[2] - max(yRange))/2 # centre

  } else if(shape == "square"){
    x_shiftValue <- 0
    yRange       <- seq(from = yLim[1], to = yLim[2], by = byValue)
  } else {
    stop("Unkown bin shape")
  }
  # Make one data frame
  seeds <- data.frame(x = xRange[1], y = yRange)

  # Step 3: Creating the alternating index for shifting
  shiftIndex <- rep(c(1, -1), length.out = nrow(seeds))

  # step 4: Shift 1st column
  seeds$x <- seeds$x + shiftIndex*x_shiftValue

  # Step 5: Loop to create the other columns and shift them laterally if necessary
  for(i in 2:length(xRange)){
    seeds <- rbind(seeds, data.frame(x = xRange[i] + shiftIndex*x_shiftValue, y = yRange))
  }

  # Step 6: Add bin information to seeds
  seeds$bin <- row.names(seeds)

  ### Find the closest seed and label it accordingly
  # Loop through the points and find bins based on distance
  if(useParallelisation){
    ########## Parallelisation
    binLabels <- foreach(i = 1:length(x), .combine = 'c') %dopar% {
      # Calculate Euclidean distances
      dists <- sqrt((x[i]-seeds$x)^2 + (y[i] - seeds$y)^2)

      # Check if there is more than one possibility, then select random
      if(sum(dists == min(dists)) == 1){
        return(seeds$bin[dists == min(dists)])
      } else {
        return(sample(seeds$bin[dists == min(dists)], 1))
      }
    }

    # Stop cluster again
    parallel::stopCluster(cl = my.cluster)
  } else {
    ########## No parallelisation
    # Prepare the labels
    binLabels <- rep(NA, length(x))

    for(i in 1:length(x)){
      # Calculate Euclidean distances
      dists <- sqrt((x[i]-seeds$x)^2 + (y[i] - seeds$y)^2)

      # Check if there is more than one possibility, then select random
      if(sum(dists == min(dists)) == 1){
        binLabels[i] <- seeds$bin[dists == min(dists)]
      } else {
        binLabels[i] <- sample(seeds$bin[dists == min(dists)], 1)
      }
    }
  }

  # Return
  return(binLabels)
}
