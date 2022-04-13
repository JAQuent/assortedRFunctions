getAngleInDegreesFromPoint <-function(x, y){
  # This function gives the angle in a circle, which means that 180 or 360 will be
  # added depending on which quadrant the angle is.
  # Assume centre is (0, 0)
  # Calculate theta
  theta <- atan(y/x)

  # Check which quadrant and add 180 or 360 got get full theta angle
  if(x > 0 & y > 0){
    #First Quadrant
    angleDegree <- rad2deg(theta)
  } else if(x < 0 & y > 0){
    # Second quadrant
    angleDegree <- rad2deg(theta) + 180
  } else if(x < 0 & y < 0){
    # Third quadrant
    angleDegree <- rad2deg(theta) + 180
  } else {
    # Forth quadrant
    angleDegree <- rad2deg(theta) + 360
  }

  return(angleDegree)
}
