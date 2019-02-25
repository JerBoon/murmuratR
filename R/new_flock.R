#' Initialise a data frame of birds
#'
#' @param n The number of birds in the flock
#'
#' @return A new data frame
#' @export
#'
#' @examples
#'   my_flock <- new_flock(1000)
new_flock <- function(n) {
  #Create a new flock, at ground level, on average 0.1m apart
  #This is denser than they might want to be in flight which makes thngs more interesting at takeof..
  df <- data.frame(x=runif(n)*sqrt(n)/10, y=runif(n)*sqrt(n)/10, z=0,
                   dx=0,dy=0,dz=0,
                   alpha=F)
  
  attr(df,"ticks") <- 0L
  return(df)
}