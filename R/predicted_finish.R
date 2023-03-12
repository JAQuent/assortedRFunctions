predicted_finish <- function(startIndex, i, n, startTime){
  # Get current time
  now <- Sys.time()

  # Get the time that elapsed since the start
  elapsedTime       <- difftime(now, startTime, units = 'hours')
  # Estimate how long one iteration takes
  timePerIter       <- elapsedTime/(i - startIndex + 1)
  # Calculate how many iterations are left before completion
  iterLeft          <- n - i
  # Estimate how many hours are left and add this to current time
  predictedTimeLeft   <- timePerIter * iterLeft
  predictedFinishTime <- now + predictedTimeLeft

  # Print results to the console
  cat(paste0("\n Iteration: ", i,"/", n, " | The process has ", signif(predictedTimeLeft, 4), " hours left | Predicted finished: ", predictedFinishTime, '\n'))
}
