# Function to create hash id
create_md5sum_hash_id <- function(id2hash, secret_key){
  # Get unique IDs
  unique_id <- unique(id2hash)

  # Combine unique_ids with secret
  unique_id_with_secret <- paste0(unique_id, secret_key)

  # Loop through all ids that need to be hashed
  unique_id_hash <- get_md5sums_for_vector(unique_id_with_secret)

  # Replace values so they match id2hash
  hashes <- rep(NA, length(id2hash))
  for(i in 1:length(unique_id)){
    hashes[id2hash == unique_id[i]] <- unique_id_hash[i]
  }

  # Return
  return(hashes)
}
