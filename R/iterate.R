no.neighbours = 10


find_neighbours <- function(birds,b) {
  d <- (birds$x - birds[b,"x"])^2 + (birds$y - birds[b,"y"])^2 + (birds$z - birds[b,"z"])^2
  d <- sqrt(d)
  
  return(order(d)[2:(1+no.neighbours)])
}

iterate <- function(birds) {
  
  no.birds <- nrow(birds)
  birds2 <- birds
  birds2$x <- birds$x + birds$dx
  birds2$y <- birds$y + birds$dy
  birds2$z <- birds$z + birds$dz
  
  for (i in 1:no.birds) {
    if(birds2[i,"alpha"]) {
      birds2[i,c("dx","dy","dz")] <- (unit_vector(target_pos(0)) - birds[i,c("dx","dy","dz")]) * speed
    } else {      
      n <- find_neighbours(birds,i)
      dx.n <- sum(birds[n,"dx"])
      dy.n <- sum(birds[n,"dy"])
      dz.n <- sum(birds[n,"dz"])
      #  print(dx.n)
      birds2[i,c("dx","dy","dz")] <- unit_vector(c(dx.n,dy.n,dz.n)) * speed * (runif(1)/20+1)
    }
  }

  attr(birds2,"ticks") <- attr(birds2,"ticks")+ 1
  return(birds2)
}
