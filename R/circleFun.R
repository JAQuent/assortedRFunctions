circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  # Source https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
