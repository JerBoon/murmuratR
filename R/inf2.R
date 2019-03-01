influence.radius <- 1
view.angle <- 135
max.influencers <- 10

# Find #s of any birds within the sphere of influence of bird b
find_influencers2 <- function(birds, b) {
  
  x1 <- birds[b,"x"]
  y1 <- birds[b,"y"]
  z1 <- birds[b,"z"]

  #quick select bounding box of neighbours
  inf <- birds[birds$x >= x1-influence.radius & birds$x <= x1+influence.radius &
               birds$y >= y1-influence.radius & birds$y <= y1+influence.radius &
               birds$z >= z1-influence.radius & birds$z <= z1+influence.radius,]  

  #distance to all neighbours
  inf$dist <- sqrt((inf$x - x1)^2 +
                   (inf$y - y1)^2 +
                   (inf$z - z1)^2 )

  #remove those outside influence distance, plus our bird
  inf <- inf[inf$dist <= influence.radius & inf$dist > 0,]

  #none found?
  if(nrow(inf) == 0)
    return(NULL)
  
  #calculate direction to each
  inf$dirx <- inf$x - x1
  inf$diry <- inf$y - y1
  inf$dirz <- inf$z - z1
  
  #find angle between direction of travel of our bird and the position of each bird
  #then filter out the birds behind
  dir <- unit_vector(birds[b,c("dx","dy","dz")])
  inf$angle <- 0
  for (i in 1:nrow(inf)) {
    inf[i,"angle"] <- sum(dir *
                          unit_vector(inf[i,c("dirx","diry","dirz")]))
  }
  inf$angle <- acos(inf$angle) /2 /pi *360
  inf <- inf[inf$angle <= view.angle,]

  #filter out the top n closest only  
  inf <- inf[order(inf$dist)[1:max.influencers],]

  
  #none found?
  if(nrow(inf) == 0)
    return(NULL)
  
  return(inf)
}
