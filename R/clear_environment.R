clear_environment <- function(){
  # This function asks whether you want the environment to be cleared

  # Ask
  cat('\rDo you want the enviorment to be cleared? (y/n)\r')

  # Get response
  response <- readline(prompt="Response: ")

  if(tolower(response) == 'yes' | tolower(response) == 'y'){
    # Yes
    cat('\r The enviroment will be cleared. \r')

    # Clear
    rm(list = ls())
  } else {
    # No in all other cases as safety feature
    cat('\r The enviroment  not will be cleared. \r')
  }
}
