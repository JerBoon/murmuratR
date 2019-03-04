
iterate_flock <- function(flk, n) {

  min_dist <- Inf
  max_dist <- 0

  for (it in 1:n) {

    #move the birds to next position
    flk$x  <- flk$x + flk$dx 
    flk$y  <- flk$y + flk$dy 
    flk$z  <- flk$z + flk$dz
  
    tick <- attr(flk,"ticks")
    t<- target_pos(tick)
  
    #calculate a unit directional vector to the cosmic influencer
    inf <- data.frame(xdir = t[1] - flk$x,
                      ydir = t[2] - flk$y,
                      zdir = t[3] - flk$z,
                      stringsAsFactors = FALSE)
    inf$dist <- sqrt(inf$xdir^2 +inf$ydir^2 + inf$zdir^2)
    inf$dist <- inf$dist + (inf$dist == 0)

    inf$xdir <- inf$xdir / inf$dist
    inf$ydir <- inf$ydir / inf$dist
    inf$zdir <- inf$zdir / inf$dist

    min_dist <- min(min_dist,min(inf$dist))    
    max_dist <- max(max_dist,max(inf$dist))    

    #Calculate the amount of influence the  influencer should have
    inf$inf <- pmax(pmin(100 / (inf$dist^2), 0.8),0.2)
    inf$inf[order(inf$dist)] <- seq(1, 0.2, length.out=nrow(inf)) ^1.8
  
    #Apply the influence vector versus the current direction vector
    flk$dx <- flk$dx * (1-inf$inf) + inf$xdir * inf$inf
    flk$dy <- flk$dy * (1-inf$inf) + inf$ydir * inf$inf
    flk$dz <- flk$dz * (1-inf$inf) + inf$zdir * inf$inf

    # scale out the resulting vector to 1  
    len <- sqrt(flk$dx^2 + flk$dy^2 + flk$dz^2)
    len <- len + (len == 0)

    flk$dx <- flk$dx / len
    flk$dy <- flk$dy / len
    flk$dz <- flk$dz / len
  
    attr(flk,"ticks") <- attr(flk,"ticks")+ 1
  }

  print(paste("Iteration - flock distance =",round(min_dist),"-",round(max_dist)))
  return(flk)
  
}
