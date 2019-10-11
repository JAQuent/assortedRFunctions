brms_fixef_report <- function(x, decimal = 2){
  paste(' = ',
        round(x[1], decimal),
        ' (95 % CI [',
        round(x[3], decimal),
        ', ',
        round(x[4], decimal),
        '])' ,
        sep = '')
}
