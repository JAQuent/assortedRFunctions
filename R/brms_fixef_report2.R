# Create a fixef string from the model directly
brms_fixef_report2 <- function (m, id, digits = 2, rounding_type, pre_string){
  # Get fixed effects
  x <- fixef(m)
  x <- x[id, ]

  if (missing(rounding_type)) {
    rounding_type <- "round"
  }
  if (missing(pre_string)) {
    pre_string <- ""
  }
  if (rounding_type == "signif") {
    return_string <- paste0(pre_string, " = ", signif(x[1],
                                                      digits), " (95 % CI [", signif(x[3], digits), ", ",
                            signif(x[4], digits), "])")
  }
  else if (rounding_type == "round") {
    return_string <- paste0(pre_string, " = ", round(x[1],
                                                     digits), " (95 % CI [", round(x[3], digits), ", ",
                            round(x[4], digits), "])")
  }
  else {
    stop("Wrong rounding type. Choose signif or round.")
  }
  return(return_string)
}
