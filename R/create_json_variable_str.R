create_json_variable_str <- function(var_name, list_var, extraString){
  require(rjson)
  json <- toJSON(list_var)
  Json_string <- gsub('\"', '\\\\"', json) # adding \ so that " are printed right
  return(paste('var ', var_name, extraString,' = "', Json_string, '";', sep = ''))
}

