clear_environment <- function(){
  # This function asks whether you want the (global) environment to be cleared

  # Ask
  cat('\rDo you want the environment to be cleared? (y/n)\r')

  # Get response
  response <- readline(prompt="Response: ")

  if(tolower(response) == 'yes' | tolower(response) == 'y'){
    # Yes
    cat('\r The environment will be cleared. \r')

    # Clear
    rm(list = ls(globalenv()), pos = globalenv())
  } else {
    # No in all other cases as safety feature
    cat('\r The environment  not will be cleared. \r')
  }
}
