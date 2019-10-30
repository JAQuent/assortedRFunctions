create_json_variable_str <- function(var_name, list_var){
  require(rjson)
  json <- toJSON(question)
  Json_string <- gsub('\"', '\\\\"', json) # ading \ so that " are printed right
  return(paste('var ', var_name,' = "', Json_string, '";', sep = ''))
}

