getAngleInDegreesFromPoint <-function(x, y){
  theta <- atan2(y, x)  # Returns angle in radians between -π and π

  # Convert to degrees and normalize to 0-360
  angleDegree <- rad2deg(theta)
  angleDegree <- (angleDegree + 360) %% 360

  return(angleDegree)
}
