
iterate <- function(birds) {
  
  no.birds <- nrow(birds)
  
  # caclualte base results based on last calculated direction
  birds2 <- birds
  birds2$x <- birds$x + birds$dx
  birds2$y <- birds$y + birds$dy
  birds2$z <- birds$z + birds$dz
  
  x.avg <- mean(birds2$x)
  y.avg <- mean(birds2$y)
  z.avg <- mean(birds2$z)
  
  # Loop through birds and calculate new direction vector  
  for (i in 1:no.birds) {
    
    #info of the current bird
    x1 <- birds[i,"x"]
    y1 <- birds[i,"y"]
    z1 <- birds[i,"z"]

    #inertia of ground birds (and a loop speed up)
    if (z1 == 0 & runif(1) > 0.1)
      next

    #calculate a direction to move in    
    if(birds2[i,"alpha"]) {
      birds2[i,c("dx","dy","dz")] <- target_pos(0) - birds[i,c("x","y","z")]
    } else {      
      inf <- find_influencers(birds,i)
      if (is.null(inf)) {
        #if no birds nearby, aim towards the flock
        birds2[i,c("dx","dy","dz")] <- c(x.avg,y.avg,z.avg) - birds[i,c("x","y","z")]
      } else {
        birds2[i,c("dx","dy","dz")] <- new_direction(inf,i)
      }
    }
  }
  
  # The speed direction vectors are all out of scale, 
  # Need to readjust for actual speeds
  sp <- sqrt(birds2$dx ^ 2 + birds2$dy ^ 2 + birds$dz ^ 2)  
  sp <- sp + (sp ==0)
  birds2$dx <- birds2$dx / sp * speed
  birds2$dy <- birds2$dy / sp * speed
  birds2$dz <- birds2$dz / sp * speed
  
  attr(birds2,"ticks") <- attr(birds2,"ticks")+ 1
  return(birds2)
}
