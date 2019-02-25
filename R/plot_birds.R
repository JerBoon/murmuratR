
plot_birds <- function(birds) {
  
  #need to do something cleverer!?
  ggplot2::ggplot(birds, aes(x=x,y=z)) +
    ggplot2::geom_point(alpha=0.2) +
    ggplot2::theme_minimal()
}  