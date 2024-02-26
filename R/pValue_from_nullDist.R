pValue_from_nullDist <- function(critVal, dist, alternative = "two.sided", verbose = FALSE){
  # This function uses Logspline Density Estimation to calculate the p-value from a null distribution.
  # critVal - the empirical value to be tested against the null distribution
  # dist - the null distribution. There can be problems, when the distribution is too large (e.g. 1,000,000 values).
  # alternative - a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".

  # Load package
  if (!any(search() == "package:polspline")) {
    require(polspline)
  }

  # Logspline Density Estimation
  fit.dist <- logspline(dist)

  # Integration function
  integrand <- function(x){
    dlogspline(x, fit.dist)
  }

  # Use integrate function to get p-value
  if(alternative == "greater"){
    p <- integrate(integrand, lower = critVal, upper = Inf)$value
  } else if(alternative == "less"){
    p <- integrate(integrand, lower = -Inf, upper = critVal)$value
  } else if(mean(dist) < critVal){ # The last two statements are for the two-tailed test
    p <- integrate(integrand, lower = critVal, upper = Inf)$value
  } else {
    p <- integrate(integrand, lower = -Inf, upper = critVal)$value
  }

  # Calculate the two-tailed p-value
  if(alternative == "two.sided"){
    p <- p * 2
    p <- ifelse(p > 1, 1, p)
  }

  # Report
  if(verbose){
    cat("\nNull distribution:", mean_SD_str2(dist, type = 1, digits = 2, rounding_type = "signif"),
        "\nEmpirical value:", signif(as.numeric(critVal), 2),
        paste0("\nP-value (", alternative, "):"), p, "\n")
  }

  # Return
  return(p)
}
