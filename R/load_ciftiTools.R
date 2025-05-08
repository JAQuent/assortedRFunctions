load_ciftiTools <- function(possible_wb_paths){
  # Load the library if it is not loaded already
  if(!any(search() == "package:ciftiTools")){
    require(ciftiTools)
  }

  # Loop through possible paths
  wb_path_set <- FALSE
  for(i in 1:length(possible_wb_paths)){
    if(file.exists(possible_wb_paths[i])){
      ciftiTools.setOption("wb_path", possible_wb_paths[i])
      wb_path_set <- TRUE
    }
  }

  # Stop if there is no path that works
  if(!wb_path_set){
    stop("\n\nERROR: The variable possible_wb_paths does not contain a valid paths to a wb_command binary.")
  }
}

# Usage:
## Load ciftiTools and set workbench path
#possible_wb_paths <- c("/usr/bin/wb_command", "/home1/Jaquent/Toolboxes/workbench/bin_rh_linux64/")
#load_ciftiTools(possible_wb_paths)

