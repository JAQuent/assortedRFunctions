prepare_brain_network_ridge_plot <- function(stat_map, p_map_list, cut_off, direction, network_map, network_label){
  # p_map_list should be positive (c1) & negative (c2) if there are two maps

  # Combine stat_map and network_map to a data frame
  df <- data.frame(stat = c(stat_map$data$cortex_left, stat_map$data$cortex_right),
                   network = c(network_map$data$cortex_left, network_map$data$cortex_right))

  # Prepare the network labels
  ## Use row names as region names
  network_label$Region <- row.names(network_label)
  row.names(network_label)   <- NULL

  ## Convert the RGB values to hexcodes
  network_label$hexcol <- rgb(network_label$Red, network_label$Green, network_label$Blue,
                            maxColorValue = 1)

  ## Sort alphabetically by region
  network_label <- network_label[order(network_label$Region), ]

  # Add network labels to the data frame
  df$net_label <- NA
  for(i in 1:nrow(network_label)){
    # Select all rows in df that have this key
    bool_index <- df$network == network_label$Key[i]
    df$net_label[bool_index] <- network_label$Region[i]
  }

  # Exclude vertices that don't have a net_label
  df <- df[!is.na(df$net_label), ]

  # Calculate average value for each network
  df_agg <- ddply(df, c("net_label"), summarise, avg_stat = mean(stat))

  # Order based on the mean
  network_label <- network_label[order(df_agg$avg_stat), ]
  df_agg        <- df_agg[order(df_agg$avg_stat), ]

  ## Make net_label a factor with the correct order
  df$net_label <- factor(df$net_label, levels = df_agg$net_label,  ordered = TRUE)

  # Determine the minimum and maximum stat that is significant
  # Here subcortical data will also be used.
  stat_values <- as.matrix(stat_map)
  if(length(p_map_list) == 2){
    # Convert maps to vectors
    p_values_neg <- as.matrix(p_map_list[[2]])
    p_values_pos <- as.matrix(p_map_list[[1]])

    # Get min stat
    if(direction == "greater"){
      min_stat    <- max(stat_values[p_values_neg > cut_off])
      max_stat    <- min(stat_values[p_values_pos > cut_off])
    } else if (direction == "smaller"){
      min_stat    <- max(stat_values[p_values_neg < cut_off])
      max_stat    <- min(stat_values[p_values_pos < cut_off])
    } else {
      stop(paste("direction must either be 'greater' or 'smaller'."))
    }
  } else if (length(p_map_list) == 1){
    # For subject-level analysis there is only one p-value map for positive and
    # negative effects, which need to be split.
    # Convert maps to vectors
    p_values <- as.matrix(p_map_list[[1]])

    # Get min stat
    if(direction == "greater"){
      min_stat    <- max(stat_values[p_values > cut_off & stat_values < 0])
      max_stat    <- min(stat_values[p_values > cut_off & stat_values > 0])
    } else if (direction == "smaller"){
      min_stat    <- max(stat_values[p_values < cut_off & stat_values < 0])
      max_stat    <- min(stat_values[p_values < cut_off & stat_values > 0])
    } else {
      stop(paste("direction must either be 'greater' or 'smaller'."))
    }
  } else {
    stop(paste("Error the number of p_map_list is not 2 or 1."))
  }

  # Return
  return(list(df = df, min_stat = min_stat, max_stat = max_stat, network_label = network_label))
}

# # Load CIFTIs
# con1_tstat_c1 <- read_cifti("results/group-level/ON/con1/con1_dat_tstat_c1.dscalar.nii", brainstructures = "all")
# con1_cFDRp_c1 <- read_cifti("results/group-level/ON/con1/con1_dat_tstat_cfdrp_c1.dscalar.nii", brainstructures = "all")
# con1_cFDRp_c2 <- read_cifti("results/group-level/ON/con1/con1_dat_tstat_cfdrp_c2.dscalar.nii", brainstructures = "all")
#
# # Prepare input for function
# network_label <- Yeo7_xii$meta$cifti$labels$parcels[-1, ]
# p_map_list    <- list(con1_cFDRp_c1, con1_cFDRp_c2)
#
# # Prepare data
# ride_data <- prepare_brain_network_ridge_plot(con1_tstat_c1, p_map_list, logp_cut_off, "greater", Yeo7_xii, network_label)
#
# ## Create plot
# ggplot(ride_data$df, aes(x = stat, y = net_label, fill = net_label)) +
#   geom_density_ridges() +
#   scale_fill_manual(values = ride_data$network_label$hexcol) +
#   geom_vline(xintercept = ride_data$min_stat, linetype = 2, linewidth = 0.3) +
#   geom_vline(xintercept = ride_data$max_stat, linetype = 2, linewidth = 0.3) +
#   theme_classic() +
#   coord_cartesian(ylim = c(0.5, 13)) +
#   theme(legend.position = "none", plot.margin = unit(c(1,1,1,1), "mm"))
