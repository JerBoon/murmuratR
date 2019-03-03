min.separation <- 0.2

new_direction <- function(inf,b) {
  
  x1 <- birds[b,"x"]
  y1 <- birds[b,"y"]
  z1 <- birds[b,"z"]
  dx1 <- birds[b,"dx"]
  dy1 <- birds[b,"dy"]
  dz1 <- birds[b,"dz"]
  
  
  dx1 <- dx1 + (mean(inf$x) - x1)
  dy1 <- dy1 + (mean(inf$y) - y1)
  dz1 <- dz1 + (mean(inf$z) - z1)
  
  # push back against any birds too close
  if (any(inf$dist < min.separation)) {
    dx1 <- dx1 - (mean(inf[inf$dist<min.separation, "x"]) - x1)
    dy1 <- dy1 - (mean(inf[inf$dist<min.separation, "y"]) - y1)
    dz1 <- dz1 - (mean(inf[inf$dist<min.separation, "z"]) - z1)
    
  }  
  return(c(dx1,dy1,dz1))
}

inf
birds[10,]
new_direction(inf,10)