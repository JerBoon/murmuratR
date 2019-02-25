
speed <- 0.1
birds <- new_flock(100)

target_pos(attr(birds,"ticks"))

#launch one
birds[1,"alpha"] <- TRUE
#birds[1,c("dx","dy","dz")] <- unit_vector(target_pos(0)) * speed


for (i in 1:100) {
  birds <- iterate(birds )
  plot_birds(birds)
}