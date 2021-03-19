clear_environment <- function(){
  # This function asks whether you want the (global) environment to be cleared

  # Get utils
  require(utils)

  # Ask via winDialog
  answer <- "NO"
  answer <- winDialog("yesno", "Do you want your global environment to be cleared?")

  # Use the response
  if (answer == 'YES'){
    # Yes
    cat('\r The environment will be cleared. \n')

    # Clear
    rm(list = ls(globalenv()), pos = globalenv())
  } else {
    cat('\r The environment  not will be cleared. \n')
  }

  # Wait short time (1 second) so it is completed
  Sys.sleep(1)
}
