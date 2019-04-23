anonymize <- function(inVector, fileName = 'anonKey', strLength = 6){
  # Input converted to character
  inVector  <- as.character(inVector)

  # Get unique subjects
  subs      <- unique(inVector)
  numSub    <- length(subs)

  # Create anonymized strings for subects
  anonStr   <- apply(as.array(rep(strLength, numSub)), 1, createAnonStr)

  # Check whether all strings are unique, if not first try again,
  # then increase string length by 1 each attempt
  if(any(duplicated(anonStr))){
    anonStr   <- apply(as.array(rep(strLength, numSub)), 1, createAnonStr)
    while(any(duplicated(anonStr))){
      strLength <- strLength + 1
      anonStr   <- apply(as.array(rep(strLength, numSub)), 1, createAnonStr)
      print(paste('Increased string length to', strLength, 'because of duplicates.'), quote = FALSE)
    }
  }

  # Assign anonymized strings to vector and convert to factor
  # Solution for this problem found here
  df1       <- data.frame(a = inVector)
  df2       <- data.frame(a = subs, value = anonStr, stringsAsFactors = FALSE)
  outVector <- suppressMessages(plyr::join(df1, df2)$value)
  outVector <- as.factor(outVector)

  # Removing order of factor
  outVector <- factor(outVector,levels(outVector)[sample(1:numSub)])


  # Save list and create key
  deAnonKey <- data.frame(origID = subs,
                          newId  = anonStr,
                          stringsAsFactors = FALSE)

  # Create text strings and combine
  commentStr <- "#Hello"
  headerStr  <- paste(names(deAnonKey), collapse = '\t')
  keyStr     <- c()
  for(i in 1:numSub){
    keyStr   <- c(keyStr, paste(as.character(deAnonKey[i,]), collapse = '\t'))
  }
  keyStr    <- paste(keyStr, collapse = '\n')
  outputStr <- paste(commentStr, headerStr, keyStr, collapse = '\n', sep = '\n')

  # Write text string to gile
  cat(outputStr, file = datedFileNam(fileName, '.txt'))
  return(outVector)
}
