#' Title
#'
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
new_flock <- function(n) {
  #Create a new flock, at ground level, on average 0.1m apart
  #This is denser than they might want to be in flight which makes thngs more interesting at takeof..
  df <- data.frame(x=runif(n)*sqrt(n)/10, y=runif(n)*sqrt(n)/10, z=0,
                   dx=0,dy=0,dz=0,
                   alpha=F)
  
  return(df)
}