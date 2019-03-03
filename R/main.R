
birds <- new_flock(100)

for (i in 1:16) {
  birds <- iterate_flock(birds,10)
  frame = birds[,c("x","z")]
  frame$i = i
  if(i==1) { results <- frame } else { results <- rbind(results,frame) }
}
ggplot2::ggplot(results, aes(x=x,y=z)) +
  ggplot2::geom_point(alpha=0.05) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~i) +
  ggplot2::coord_fixed()


plot_birds(birds)


t1 <- target_pos(0)
for (i in 1:1000) {
  t2 <- target_pos(i)
  d <- sqrt(sum((t1-t2)^2))
  print(d)
  t1 <- t2
}
