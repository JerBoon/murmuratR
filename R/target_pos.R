
target_pos <- function(t) {

  x <- sin(t/97)*200 + cos(t/217)*30
  y <- cos((t+53)/89)*200 + sin((47-t)/131)*30
  z <- cos((t+61)/41)*100 + sin((t+13)/91)*27+40

  return (c(x=x,y=y,z=z))
}
