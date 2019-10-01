cohens_d_raw <- function(x, y) {
  # copied from https://stackoverflow.com/questions/15436702/estimate-cohens-d-for-effect-size
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y)) ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd) ## common sd computation

  md/csd ## cohen's d
}
