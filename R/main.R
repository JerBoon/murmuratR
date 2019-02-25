
speed <- 0.1
birds <- new_flock(1000)

birds$x <- rnorm(nrom(birds))
birds$y <- rnorm(nrom(birds))
birds$z <- rnorm(nrom(birds))

target_pos(attr(birds,"ticks"))

#launch one
birds[1,"alpha"] <- TRUE
#birds[1,c("dx","dy","dz")] <- unit_vector(target_pos(0)) * speed


for (i in 1:20) {
  birds <- iterate(birds )
  plot_birds(birds)
}
plot_birds(birds)