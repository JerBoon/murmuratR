
speed <- 0.1
birds <- new_flock(100)

birds$x <- rnorm(nrow(birds))
birds$y <- rnorm(nrow(birds))
birds$z <- rnorm(nrow(birds))

target_pos(attr(birds,"ticks"))

#launch one
birds[1,"alpha"] <- TRUE
birds[1,"z"] <- 0.0000000001
#birds[1,c("dx","dy","dz")] <- unit_vector(target_pos(0)) * speed


for (i in 1:100) {
  birds <- iterate(birds )
  plot_birds(birds)
}
plot_birds(birds)



system.time(for (i in 1:50) { birds <- iterate(birds )})
  