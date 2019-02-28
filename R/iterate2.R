influence.radius <- 1
view.angle <- 135
max.influencers <- 10

# Find #s of any birds within the sphere of influence of bird b
find_influencers <- function(birds, b) {
  
  x1 <- birds[b,"x"]
  y1 <- birds[b,"y"]
  z1 <- birds[b,"z"]
  
  #distance^2 to other birds
  d2 <- (birds$x - x1)^2 + (birds$y - y1)^2 + (birds$z - z1)^2

  #those with in sphere of influence^2
  inf <- which(d2 < influence.radius^2)
  inf <- inf[inf != b]
  inf <- inf[inf <= max.influencers+1]
  
  #none found?
  if(length(inf) == 0)
    return(NULL)

  #fetch data for those, and calculate some other stuff
  inf.data <- birds[inf,]
  inf.data$dist <- sqrt((inf.data$x - x1)^2 +
                        (inf.data$y - y1)^2 +
                        (inf.data$z - z1)^2 )
  inf.data$dirx <- inf.data$x - x1
  inf.data$diry <- inf.data$y - y1
  inf.data$dirz <- inf.data$z - z1
  uv <- unit_vector(birds[b,c("dx","dy","dz")])
  
  for (i in 1:nrow(inf.data)) {
    inf.data[i,"angle"] <- sum(uv *
                               unit_vector(inf.data[i,c("dirx","diry","dirz")]))
  }
  inf.data$angle <- acos(inf.data$angle) /2 /pi *360
  inf.data <- inf.data[inf.data$angle <= view.angle,]

  #none found?
  if(nrow(inf.data) == 0)
    return(NULL)
  
  return(inf.data)
}

#--------------------------------------------

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
      if (!is.null(n)) {
        print(nrow(n))
        #dx.n <- sum(birds[n,"dx"])
        #dy.n <- sum(birds[n,"dy"])
        #dz.n <- sum(birds[n,"dz"])
        #  print(dx.n)
        #birds2[i,c("dx","dy","dz")] <- unit_vector(c(dx.n,dy.n,dz.n)) * speed * (runif(1)/20+1)
        #print(paste("influencesrs:",length(n)))
      }
    }
  }

  attr(birds2,"ticks") <- attr(birds2,"ticks")+ 1
  return(birds2)
}
