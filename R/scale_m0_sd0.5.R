scale_m0_sd0.5 <- function(x){
  (x - mean(x, na.rm = TRUE))/(sd(x, na.rm = TRUE)*2)
}
