reportANOVAEffect <- function(x){
  string <- paste('F(',
                  x$DFn,
                  ',',
                  x$DFd,
                  ') = ',
                  round(x$F, 2),
                  ', p ',
                  pValue(x$p),
                  ', eta ',
                  paste('=',substr(as.character(round(x$ges, 3)), 2,5)),
                  sep = '')
  return(string)
}
