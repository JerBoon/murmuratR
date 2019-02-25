unit_vector <- function(c) {
  
  len <- sqrt(c[1]^2 + c[2]^2 + c[3]^2)
  if(len >0) {
     c <- c / len
  }
  return(c)
}
