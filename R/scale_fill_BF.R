scale_fill_BF <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(rainbow(8), c('> 10', '> 6', '> 3', '> 1', '< 1', '< 1/3', '< 1/6', '< 1/10')), 
    ...
  )
}