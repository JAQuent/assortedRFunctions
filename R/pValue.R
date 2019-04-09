pValue <- function(x, method = 'one'){
  if(method == 'multiple'){
    lowest      <- min(x[x > 0.05])
    x.converted <- substr(as.character(round(lowest, 3)), 2,5)
  } else {
    if (inherits(x, "lm")){
      s <- summary.lm(x)
      x <- pf(s$fstatistic[1L], s$fstatistic[2L], s$fstatistic[3L], lower.tail = FALSE)
      if(x > 1){
        stop("There is no p-value greater than 1")
      } else if(x < 0.001){
        x.converted <- '< .001'
      } else{
        x.converted <- paste('=',substr(as.character(round(x, 3)), 2,5))
      }
    } else {
      if(x > 1){
        stop("There is no p-value greater than 1")
      } else if(x < 0.001){
        x.converted <- '< .001'
      } else{
        x.converted <- paste('=',substr(as.character(round(x, 3)), 2,5))
      }
    }
  }
  return(x.converted)
}
