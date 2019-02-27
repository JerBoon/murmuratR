influence.radius <- 1
view.angle <- 135

# Find #s of any birds within the sphere of influence of bird b
find_influencers <- function(birds,b) {
  
  #distance^2 to other birds
  d2 <- (birds$x - birds[b,"x"])^2 + (birds$y - birds[b,"y"])^2 + (birds$z - birds[b,"z"])^2

  #those without sphere of influence^2
  inf <- which(d2 < influence.radius^2)
  inf <- inf[inf != b]

  #none found?
  if(length(inf) == 0)
    return(NA)

  #fetch data for those, and calculate some other stuff
  inf.data <- birds[inf,]
  inf.data$dist <- sqrt((inf.data$x - birds[b,"x"])^2 +
                        (inf.data$y - birds[b,"y"])^2 +
                        (inf.data$z - birds[b,"z"])^2 )
  inf.data$dirx <- inf.data$x - birds[b,"x"]
  inf.data$diry <- inf.data$y - birds[b,"y"]
  inf.data$dirz <- inf.data$z - birds[b,"z"]
  uv <- unit_vector(birds[b,c("dx","dy","dz")])
  
  for (i in 1:nrow(inf.data)) {
    inf.data[i,"angle"] <- sum(uv *
                               unit_vector(inf.data[i,c("dirx","diry","dirz")]))
  }
  inf.data$angle <- acos(inf.data$angle) /2 /pi *360
  inf.data <- inf.data[inf.data$angle <= view.angle,]

  #none found?
  if(nrow(inf.data) == 0)
    return(NA)
  
  return(inf.data)
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
      n <- find_influencers(birds,i)
      dx.n <- sum(birds[n,"dx"])
      dy.n <- sum(birds[n,"dy"])
      dz.n <- sum(birds[n,"dz"])
      #  print(dx.n)
      birds2[i,c("dx","dy","dz")] <- unit_vector(c(dx.n,dy.n,dz.n)) * speed * (runif(1)/20+1)
      #print(paste("influencesrs:",length(n)))
    }
  }

  attr(birds2,"ticks") <- attr(birds2,"ticks")+ 1
  return(birds2)
}
