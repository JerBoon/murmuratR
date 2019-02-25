
plot_birds <- function(birds) {
  
  #need to do something cleverer!?
  ggplot2::ggplot(birds, aes(x=x,y=z)) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal()
}  