library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)

# Define normal and Cauchy distribution in R^2
gaussian_density <- function(x,y){
  return( exp( -0.5 * ( x^2 + y^2 ) ) / (2*pi) )
}

cauchy_density <- function(x,y){
  return( 1 / ( pi^2 * (1+x^2) * (1+y^2) ) )
}

# Compute a grid and the density of each point
x_gauss = seq(-3,3,0.01)
y_gauss = seq(-3,3,0.01)
x_cauchy = seq(-10,10,0.01)
y_cauchy = seq(-10,10,0.01)

don_gauss <- expand_grid(x_gauss, y_gauss) %>%
  mutate( gauss_density = gaussian_density(x_gauss, y_gauss))
don_cauchy <- expand_grid(x_cauchy, y_cauchy) %>%
  mutate( cauchy_density = cauchy_density(x_cauchy, y_cauchy))

# Plot density
gauss_density_plot <- ggplot( data = don_gauss ) +
  aes( x = x_gauss, y = y_gauss, z = gauss_density ) +
  geom_raster( aes( fill = gauss_density ) ) +
  coord_fixed( ratio = 1 ) +
  theme_minimal() +
  xlab("X") +
  ylab("Y") +
  ggtitle("Density of a couple of gaussian independent random variables") +
  theme( plot.title = element_text(hjust = 0.5) )

cauchy_density_plot <- ggplot( data = don_cauchy %>% filter(abs(x_cauchy) < 5 & abs(y_cauchy) < 5) ) +
  aes( x = x_cauchy, y = y_cauchy, z = cauchy_density ) +
  geom_raster( aes( fill = cauchy_density ) ) +
  coord_fixed( ratio = 1 ) +
  theme_minimal() +
  xlab("X") +
  ylab("Y") +
  ggtitle("Density of a couple of Cauchy independent random variables") +
  theme( plot.title = element_text(hjust = 0.5) )

# Plot contour lines
gauss_contour_plot <- ggplot( data = don_gauss ) +
  geom_contour(aes( x = x_gauss, y = y_gauss, z = gauss_density ), colour = "darkblue", binwidth = 0.015) +
  geom_abline(slope = -1, intercept = 1.075, color = 'red2', size = 1.2, linetype = 'dashed') +
  geom_segment(x = 0, y = 0, xend = 1.5, yend = 1.5, color = "cornflowerblue", arrow = arrow()) +
  annotate("text", label = "X + Y \u2265 a", x = -0.3, y = 1.95, size = 7, colour = "red2") +
  coord_fixed( ratio = 1 ) +
  theme_minimal() +
  xlab("X") +
  ylab("Y") +
  ggtitle("Contour lines of a couple of gaussian independent random variables") +
  theme( plot.title = element_text(hjust = 0.5) )

cauchy_contour_plot <- ggplot( data = don_cauchy ) +
  geom_contour(aes( x = x_cauchy, y = y_cauchy, z = cauchy_density ), colour = "green4", binwidth = 0.003) +
  geom_abline(slope = -1, intercept = 4.125, color = 'red2', size = 1) +
  geom_segment(x = 0, y = 0, xend = 3.8, yend = 0, color = "forestgreen", arrow = arrow()) +
  geom_segment(x = 0, y = 0, xend = 0, yend = 3.8, color = "forestgreen", arrow = arrow()) +
  annotate("text", label = "X + Y \u2265 a", x = 3.5, y = 3.5, size = 7, colour = "red2") +
  #coord_fixed( ratio = 1 ) +
  coord_cartesian(xlim = c(-6, 6), ylim = c(-6, 6)) +
  theme_minimal() +
  xlab("X") +
  ylab("Y") +
  ggtitle("Contour lines of a couple of Cauchy independent random variables") +
  theme( plot.title = element_text(hjust = 0.5) )

# print(gauss_density_plot)
# print(cauchy_density_plot)
# print(gauss_contour_plot)
# print(cauchy_contour_plot)

ggsave("gauss_density_plot.png", gauss_density_plot, dpi = "retina")
ggsave("cauchy_density_plot.png", cauchy_density_plot, dpi = "retina")
ggsave("gauss_contour_plot.png", gauss_contour_plot, dpi = "retina")
ggsave("cauchy_contour_plot.png", cauchy_contour_plot, dpi = "retina")
      