get_rslurm_results <- function(path, outputType = 'data.frame'){
  # This function loads the results of an rslurm job and either binds it a list of lists and binds
  # lists of lists to a data.frame if specified.

  # Get all results files in rslurm folder
  pattern        <- 'results_'
  files          <- list.files(path)
  results_files  <- files[grepl(pattern, files)]
  numFiles       <- length(results_files)

  # Bind to data.frame or list of lists
  if(outputType == 'data.frame'){
    # Load result files and bind rows to a data.frame
    for(i in 1:numFiles){
      if(i == 1){
        output <- dplyr::bind_rows(readRDS(paste0(path, results_files[i])))
      } else {

      }
      output <- rbind(output, dplyr::bind_rows(readRDS(paste0(path, results_files[i]))))
    }
  } else {
    # Load all result files and save in list
    output <- list()
    for(i in 1:numFiles){
      output[[i]] <- readRDS(paste0(path, results_files[i]))
    }
  }
  cat(paste('Loaded', numFiles, 'result files from rslurm folder:\n'))
  cat(path)
  return(output)
}
