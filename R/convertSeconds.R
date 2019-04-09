convertSeconds <- function(seconds){
  seconds <- round(seconds)
  if(seconds < 60){
    # Less than a minute
    time    <- paste(round(seconds), 'sec')
  }
  if(seconds >= 60 & seconds < 3600){
    # Less than an hour
    minutes <- (seconds - seconds %% 60)/60
    seconds <- seconds %% 60
    time    <- paste(minutes, 'min,', round(seconds), 'sec')
  }
  if(seconds >= 3600 & seconds < 86400){
    # Less than a day
    hours   <-  (seconds - seconds %% 3600)/3600
    minutes <-  ((seconds %% 3600) - (seconds %% 3600) %% 60)/60
    seconds <-  (seconds %% 3600) %% 60
    time    <- paste(hours, 'h,', minutes, 'min,', round(seconds), 'sec')
  }
  if(seconds >= 86400){
    # More than a day
    days    <- (seconds - seconds %% 86400)/86400
    hours   <- ((seconds %% 86400) - (seconds %% 86400) %% 3600)/3600
    minutes <- (((seconds %% 86400) %% 3600) - ((seconds %% 86400) %% 3600) %% 60)/60
    seconds <- ((seconds %% 86400) %% 3600) %% 60
    time    <- paste(days, 'd, ', hours, 'h,', minutes, 'min,', round(seconds), 'sec')
  }
  return(time)
}
