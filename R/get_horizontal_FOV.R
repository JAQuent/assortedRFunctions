# This function is meant to calculate the effective viewing angle for unity based on the
# vertical field of view axis (Default 60 degrees). The output is the horizontal FOV in degrees
get_horizontal_FOV <- function(vertical_FOV, aspect_ratio){
  # Convert vertical_FOV to radians
  vertical_FOV <- deg2rad(vertical_FOV)

  # Calculate the horizontal FOV
  horizontal_FOV <- 2 * atan(tan(vertical_FOV/2) * aspect_ratio)

  # Convert radians back to degrees
  return(rad2deg(horizontal_FOV))
}

# This function was tested in Unity using the AngleCalculator.cs script and playing
# around with the aspect ratio of the editor. The values below were similar to what I got in Unity
# aspect_ratio <- 10/5
# vertical_FOV <- 60
# get_horizontal_FOV(vertical_FOV, aspect_ratio)
# Gives 98.21321
# aspect_ratio <- 16/9
# vertical_FOV <- 60
# get_horizontal_FOV(vertical_FOV, aspect_ratio)
# Gives 91.49284
