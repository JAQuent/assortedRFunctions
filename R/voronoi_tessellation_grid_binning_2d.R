voronoi_tessellation_grid_binning_2d <- function(x, y, xLim, yLim, numSeeds, shape = "hexagon", useParallelistion = FALSE){
  # This function bins spatial 2D data via Voronoi tessellation, where x & y are coordinate values, xLim & yLim are the
  # limit values for the tessellation seeds and numSeeds is the number of seeds per axis. There are two possible
  # shapes 'square' & 'hexagon'. For hexagons, the rows are shifted a bit slightly extending the xLim values.
  # How this function works: First, tessellation seeds are created and then each point is binned by finding the closest
  # seed (estimated by Euclidean distance). Set useParallelistion to TRUE to use doParallel and foreach to speed things up.
  # In this case detectCores() - 2 cores are used.
  ### Checks of input
  if(numSeeds %% 2 != 0){
    stop("numSeeds is not an even number")
  }

  ### Check if useParallelistion & then prepare everything
  if(useParallelistion){
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
  xRange     <- seq(from = xLim[2], to = xLim[1], length.out = numSeeds)
  yRange     <- seq(from = yLim[2], to = yLim[1], length.out = numSeeds)
  seeds      <- data.frame(x = xRange[1], y = yRange)

  # Step 1: If necessary calculate shift value to generate hexagon-shaped bin
  if(shape == "hexagon"){
    xStepSize  <- xRange[1] - xRange[2] # Space between the points on the x-axis
    shiftValue <- xStepSize/4 # The value by which to shift the rows to get hexagon-shaped bins
  } else if(shape == "square"){
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

  # Step 6: Add bin information to seeds
  seeds$bin <- row.names(seeds)

  ### Find the closest seed and label it accordingly
  # Loop through the points and find bins based on distance
  if(useParallelistion){
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
    parrallel:stopCluster(cl = my.cluster)

  } else {
    ########## No parallelisation
    # Prepare the labels
    binLabels <- rep(NA, length(x))

    for(i in 1:length(x)){
      # Calculate Eudlidean distances
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
