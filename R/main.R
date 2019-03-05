
birds <- new_flock(1000)

for (i in 1:100) {
  birds <- iterate_flock(birds,10)
  frame = birds[,c("x","y","z")]
  frame$i = i
  if(i==1) { results <- frame } else { results <- rbind(results,frame) }
}
ggplot2::ggplot(results, aes(x=x,y=z)) +
  ggplot2::geom_point(alpha=0.05) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~i) +
  ggplot2::coord_fixed()
ggplot2::ggplot(results, aes(x=y,y=z)) +
  ggplot2::geom_point(alpha=0.05) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~i) +
  ggplot2::coord_fixed()


plot_birds(birds)
plot(results$x,results$z)


# -------------------------

birds <- new_flock(1000)
for (i in 1:250) {
  birds <- iterate_flock(birds,3)

  ggplot2::ggplot(birds, aes(x=x,y=z),size=1) +
  ggplot2::geom_point(alpha=0.05) +
  ggplot2::theme_void() +
  ggplot2::coord_fixed() +
  ggplot2::scale_x_continuous(limits=c(-100,150)) +
  ggplot2::scale_y_continuous(limits=c(-20,60))
  ggsave(paste0("pics/pic",i+10000,".png"), width=5, height=2)
}

t1 <- target_pos(0)
for (i in 1:1000) {
  t2 <- target_pos(i)
  d <- sqrt(sum((t1-t2)^2))
  print(d)
  t1 <- t2
}

# -------------------------------

birds <- new_flock(10000)
for (i in 1:100) {
  birds <- iterate_flock(birds,10)
  
  gg <- ggplot2::ggplot(birds, ggplot2::aes(x=x,y=z)) +
    ggplot2::geom_point(alpha=0.1,shape=".") +
    ggplot2::theme_void() +
    ggplot2::coord_fixed() +
    ggplot2::scale_x_continuous(limits=c(-100,150)) +
    ggplot2::scale_y_continuous(limits=c(-20,60))
  print(gg)
}
