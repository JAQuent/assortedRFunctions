# To do:
# Can the clustering done by the function itself?
# Make it so that libraries are only loaded if they have + check for ciftiTool and path
# Check if coordinates are assigned correctly
# Check if other parcellations are possible
# Check if subcortical clusters can be across multiple regions

# Some background to check:
# https://groups.google.com/a/humanconnectome.org/g/hcp-users/c/_Bh-nNWpnlI/m/P3E9XuPDAAAJ
# https://groups.google.com/a/humanconnectome.org/g/hcp-users/c/_3rxkc0V7YE
# https://www.pnas.org/doi/10.1073/pnas.1801582115

# Function
cifti_cluster_report <- function(zMap_file, 
                                 clusterMap_file1, 
                                 surfLeft, 
                                 surfRight,
                                 parcellationFile,
                                 minClusterSize, 
                                 verbose = TRUE){
  # Libraries
  library(plyr)
  library(stringr)
  # Complain if cifti tool is not loaded
  
  # Load CIFTI files as XIFTIs
  ## Load the z map
  zMap_xifti <- read_cifti(zMap_file, brainstructures = "all", 
                           surfL_fname = surfLeft, 
                           surfR_fname = surfRight)
  
  ## Load the cluster map
  clusterMap_xifti <- read_cifti(clusterMap_file1, brainstructures = "all", 
                                 surfL_fname = surfLeft, 
                                 surfR_fname = surfRight)
  
  ## Load parcellation
  parcellation_xifti <- read_cifti(parcellationFile, brainstructures = "all", 
                          surfL_fname = surfLeft, 
                          surfR_fname = surfRight)
  
  ## Get and prepare parcel labels
  parcel_labels <- row.names(parcellation_xifti$meta$cifti$labels$`vertex areas`)
  parcel_labels <- str_remove(parcel_labels, "L_")
  parcel_labels <- str_remove(parcel_labels, "R_")
  parcel_labels <- str_remove(parcel_labels, "_ROI")
  
  # Loop through the different parts of the brain
  parts <- c("cortex_left", "cortex_right", "subcort")
  
  for(i in 1:length(parts)){
    # Select part of the brain
    part <- parts[i]
    
    # Add create temporary data frames to analyse the clusters
    # and coordinates & parcellations for cortex_left, cortex_right and subcort
    if(part == "cortex_left"){
      # Create df
      tempDF <- data.frame(part = part,
                           cluster = clusterMap_xifti$data$cortex_left, 
                           zValue = zMap_xifti$data$cortex_left)
      
      # Get the vertices
      verts       <- clusterMap_xifti$surf$cortex_left$vertices
      medial_wall <- clusterMap_xifti$meta$cortex$medial_wall_mask$left
      
      # Remove medial wall
      verts <- verts[medial_wall, ]
      
      # Add to temporary data frame
      tempDF$x <- verts[, 1]
      tempDF$y <- verts[, 2]
      tempDF$z <- verts[, 3]
      
      # Add parcel
      tempDF$parcel <- parcellation_xifti$data$cortex_left
      tempDF$label  <- parcel_labels[tempDF$parcel]
      
    } else if(part == "cortex_right"){
      # Create df
      tempDF <- data.frame(part = part,
                           cluster = clusterMap_xifti$data$cortex_right, 
                           zValue = zMap_xifti$data$cortex_right)
      
      # Get the vertices
      verts       <- clusterMap_xifti$surf$cortex_right$vertices
      medial_wall <- clusterMap_xifti$meta$cortex$medial_wall_mask$right
      
      # Remove medial wall
      verts <- verts[medial_wall, ]
      
      # Add to temporary data frame
      tempDF$x <- verts[, 1]
      tempDF$y <- verts[, 2]
      tempDF$z <- verts[, 3]
      
      # Add parcel
      tempDF$parcel <- parcellation_xifti$data$cortex_right
      tempDF$label  <- parcel_labels[tempDF$parcel]
    } else {
      # Subcortical
      # Create df
      tempDF <- data.frame(part = part,
                           cluster = clusterMap_xifti$data$subcort, 
                           zValue = zMap_xifti$data$subcort)
      
      # For further information on process to get the MNI coordinates
      # https://jaquent.github.io/2023/07/a-way-to-get-mni-coordinates-for-subcortical-voxels-of-a-cifti-file/
      # Get the IJK coordinates
      VoxelIndicesIJK <- which(clusterMap_xifti$meta$subcort$mask, arr.ind=TRUE) - 1
      
      # We need to add a column of 1s (no idea actually why) and transpose the matrix. 
      VoxelIndicesIJK_t <- t(cbind(VoxelIndicesIJK, 1))
      
      # We get the translation matrix from the meta data of the xifti variable.
      A <- clusterMap_xifti$meta$subcort$trans_mat
      
      # Convert from IJK to MNI
      MNI <- t(A %*% VoxelIndicesIJK_t)
      
      # Remove unnecessary 4th column
      MNI <- MNI[, -4]
      
      # Add to temporary data frame
      tempDF$x <- MNI[, 1]
      tempDF$y <- MNI[, 2]
      tempDF$z <- MNI[, 3]
      
      # Add parcel
      tempDF$parcel <- parcellation_xifti$data$subcort
      tempDF$label  <- parcel_labels[tempDF$parcel]
    }
    
    # Aggregate and calculate mean z value and size
    tempDF_agg1 <- ddply(tempDF, c("part", "cluster"), 
                        summarise, 
                        size = length(cluster),
                        zValue_mean = mean(zValue),
                        zValue_median = median(zValue),
                        zValue_max = max(zValue),
                        zValue_min = min(zValue), 
                        peak_x = x[zValue == max(zValue)],
                        peak_y = y[zValue == max(zValue)],
                        peak_z = z[zValue == max(zValue)],
                        peak_region = parcel[zValue == max(zValue)], 
                        num_regions = length_uniq(parcel))
    
    # Get the parcel number and labels for the clusters
    tempDF_agg2 <- ddply(tempDF, c("part", "cluster"), 
                         summarise, 
                         regions = paste(unique(parcel), collapse = ","),
                         labels = paste(unique(label), collapse = ","))
    
    # Remove cluster that are too small and 0, which is for vertices/voxels that 
    # are not part of a cluster. 
    tempDF_agg2 <- tempDF_agg2[tempDF_agg1$cluster != 0 & tempDF_agg1$size >= minClusterSize, ]
    tempDF_agg1 <- tempDF_agg1[tempDF_agg1$cluster != 0 & tempDF_agg1$size >= minClusterSize, ]
    
    # Add to main data frames
    if(i == 1){
      results1 <- tempDF_agg1
      results2 <- tempDF_agg2
    } else {
      results1 <- rbind(results1, tempDF_agg1)
      results2 <- rbind(results2, tempDF_agg2)
    }
  }
  
  # Print summary if verbose
  if(verbose){
    cat("\n")
    # Number of cluster per part
    cat("\nNumber of clusters per cortical hemisphere + subcortex: ")
    for(i in 1:length(parts)){
      # Prepare strings
      cluster_size_range <- range(results1$size[results1$part == parts[i]])
      unit_name <- ifelse(parts[i] == "subcort", " voxels", " vertices")
      cat(paste0("\n", 
                 parts[i], ": ", sum(results1$part == parts[i]), 
                 " | Cluster size range: ", cluster_size_range[1], " - ", cluster_size_range[2], unit_name))
    }
    
    # Largest z-values
    cluster_num <- results1$cluster[results1$zValue_max == max(results1$zValue_max)]
    cat(paste0("\n\nLargest z-value is ", round(max(results1$zValue_max), 3), " (Cluster number: ", cluster_num,")"))
  }
  
  # Return a list with the results
  return(list(cluster_values = results1, cluster_labels = results2))
}