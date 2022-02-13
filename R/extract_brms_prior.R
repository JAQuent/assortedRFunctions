# Prior extraction function
extract_brms_prior <- function(brms_model_prior_string){
  # This functions takes a string like this "student_t(7, 0, 1)"
  # First split the string by ,
  tempStr <- str_split(brms_model_prior_string , ",")[[1]]

  # Get the numbers
  params  <- as.numeric(str_extract(tempStr , "\\d+\\.*\\d*"))
  return(params)
}
