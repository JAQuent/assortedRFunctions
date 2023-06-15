# This function does the following
# 0. Get the name of the object that was provided & calculate the hash for the input
# 1. Check if file hash_md5_table.csv exists, if not create & return FALSE
# 2. Check if there is an entry for the objectName if not add & return FALSE, if there is move to 3.
# 3. Check if the current hash matches the one in the able if not, return FALSE

check_if_md5_hash_changed <- function(x, hash_table_name = "md5_hash_table.csv"){
  # 0. Get the name of the object that was provided & calculate the hash for the input
  # See https://stackoverflow.com/questions/14577412/how-to-convert-variable-object-name-into-string
  objectName <- deparse(substitute(x))
  hash       <- digest::digest(x, algo = "md5")

  # 1. Check if file hash_md5_table.csv exists, if not create.
  if(file.exists(hash_table_name)){
    hash_md5_table <- read.csv(hash_table_name, header = TRUE)
  } else {
    hash_md5_table <- data.frame(objectName = objectName, md5_hash = hash)
    write.csv(hash_md5_table, file = hash_table_name, quote = FALSE, col.names = TRUE, row.names = FALSE)
    return(TRUE)
  }

  # 2. Check if there is an entry for the objectName if not add & return FALSE, if there is move to 3.
  index <- which(hash_md5_table$objectName == objectName)
  if(length(index) == 0){
    hash_md5_table <- rbind(hash_md5_table, data.frame(objectName = objectName, md5_hash = hash))
    write.csv(hash_md5_table, file = hash_table_name, quote = FALSE, col.names = TRUE, row.names = FALSE)
    return(TRUE)
  }

  # 3. Check if the current hash matches the one in the able if not, return FALSE
  # Get the reference hash from the table
  ref_hash <- hash_md5_table[hash_md5_table$objectName == objectName, 'md5_hash']
  if(ref_hash == hash){
    # Nothing changed
    return(FALSE)
  } else{
    # Update the table
    hash_md5_table[hash_md5_table$objectName == objectName, 'md5_hash'] <- hash
    write.csv(hash_md5_table, file = hash_table_name, quote = FALSE, col.names = TRUE, row.names = FALSE)
    return(TRUE)
  }
}

