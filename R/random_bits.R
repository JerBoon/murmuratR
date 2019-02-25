sin(2)


df <- data.frame(n =1:10000)
df$x <- sin(df$n/97) + cos(df$n/217)
df$y <- cos((df$n+53)/89) + sin((47-df$n)/131)
df$r <- sin((55+df$n)/113)/2+0.5 
df$g <- sin((155+df$n)/111)/2+0.5 
df$b <- sin((255+df$n)/115)/2+0.5 
df
plot(df$x,df$y, axes=F, xlab=NA,ylab=NA, col=rgb(0,0,0,0.2),usr=c(0,100,-5,95))
plot(df$x,df$y, axes=F, xlab=NA,ylab=NA, col=rgb(df$r,df$g,df$b,0.4),usr=c(0,100,-5,95))
