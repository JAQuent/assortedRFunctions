angularDifference <- function(angle1, angle2){
  # https://stackoverflow.com/questions/1878907/how-can-i-find-the-smallest-difference-between-two-angles-around-a-point
  ang_diff <- angle1 - angle2
  ang_diff <- (ang_diff + 180) %% 360 - 180
  return(ang_diff)
}
