progressDisplay <- function(i, iterations, startTime){
  currentTime   <- Sys.time()
  elapsedTime   <- currentTime - startTime
  predictedTime <- elapsedTime * (1/(i/iterations))
  cat('\r Progress: |',
      rep('=',floor((i/iterations)*50)),
      rep(' ',50 - floor((i/iterations)*50)),
      '| Expected finish: ',
      as.character(startTime + predictedTime),
      sep = '')
}
