json_strings_from_df <- function(df, fileName){
  # Functions takes data.frame and creates json_strings with it

  # Get all variable names
  var_nam <- names(df)

  # Create Javascript file to write to
  sink(fileName)

  # Loop to create json strings and assign to .GlobalEnv
  for(i in 1:length(var_nam)){
    assign(paste0(var_nam[i], '_string'),
           create_json_variable_str(var_nam[i], df[, var_nam[i]]),
           envir = .GlobalEnv)

    # Write variable
    cat(get(paste0(var_nam[i], '_string')))

    # Line break
    cat('\n\n')

    # Write JSON.paste
    cat(paste0(var_nam[i], " = JSON.parse(",var_nam[i] , ");"))

    # Line break
    cat('\n\n')
  }

  # Close file
  sink()

  # Report back
  cat(paste('Created', length(var_nam), '_string variables.\n'))
  cat(paste('Javascript file', fileName, 'has been generated.\n'))
}



