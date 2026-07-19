# Auto scale y
scale_y_auto <- function(x, padding = 0.15, range_extend = c(0, 0), ...) {
  rng_data <- range(x, na.rm = TRUE)
  rng <- rng_data + c(-range_extend[1], range_extend[2])
  span <- rng[2] - rng[1]

  raw_step <- span / 2
  mag <- 10^floor(log10(raw_step))
  nice <- c(1, 1.25, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10)

  # Pick step, escalating through nice values until breaks cover the data
  step <- nice[which(nice * mag >= raw_step)[1]] * mag
  b_min <- floor(rng[1] / mag) * mag

  # If top tick doesn't cover data max, try shifting b_min down, then bump step
  while (b_min + 2 * step < rng_data[2]) {
    idx <- which(nice * mag > step)[1]
    if (is.na(idx)) {
      mag <- mag * 10
      step <- nice[1] * mag
    } else {
      step <- nice[idx] * mag
    }
    b_min <- floor(rng[1] / mag) * mag
  }

  breaks <- b_min + c(0, 1, 2) * step
  limits <- c(b_min - padding * step, b_min + 2 * step)

  scale_y_continuous(breaks = breaks, limits = limits, expand = c(0, 0), ...)
}
