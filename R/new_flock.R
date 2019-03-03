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
  
  #Create a new aerial flock, on average 0.2m apart
  #This is denser than they might want to be in flight which makes thngs more interesting at takeof..
  sep <- 0.5
  mult <- (n ^ (1/3)) * sep
  df <- data.frame(x=rnorm(n)*mult + 10 +runif(1)*10,
                   y=rnorm(n)*mult + 10 +runif(1)*10,
                   z=rnorm(n)*mult + 10 +runif(1)*10,
                   dx=0,dy=0,dz=0)
  
  attr(df,"ticks") <- 0L
  return(df)
}