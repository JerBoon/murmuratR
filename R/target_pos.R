
target_pos <- function(t) {

  x <- sin(t/97)*200 + cos(t/217)*30
  y <- cos((t+53)/29)*200 + sin((47-t)/13)*30
  z <- cos((t+61)/41)*100 + sin((t+13)/7)*27+40

  return (c(x=x,y=y,z=z))
}

# test code  --------


df <- data.frame(t(target_pos(0)))
for (i in 1:200) {
  df <- rbind(df, data.frame(t(target_pos(i*10))) )
}

plot(df$x,df$y, asp=1)
plot(df$x,df$z, asp=1)
plot(df$z,df$y, asp=1)