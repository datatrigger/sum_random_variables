library(dplyr)
library(tidyr)
library(ggplot2)

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
  ggtitle("Density of a couple of gaussian independent random variables") +
  theme( plot.title = element_text(hjust = 0.5) )

cauchy_density_plot <- ggplot( data = don_cauchy %>% filter(abs(x_cauchy) < 5 & abs(y_cauchy) < 5) ) +
  aes( x = x_cauchy, y = y_cauchy, z = cauchy_density ) +
  geom_raster( aes( fill = cauchy_density ) ) +
  coord_fixed( ratio = 1 ) +
  theme_minimal() +
  ggtitle("Density of a couple of Cauchy independent random variables") +
  theme( plot.title = element_text(hjust = 0.5) )

# Plot contour lines
gauss_contour_plot <- ggplot( data = don_gauss ) +
  aes( x = x_gauss, y = y_gauss, z = gauss_density ) +
  geom_contour( colour = "black", binwidth = 0.015) +
  geom_abline(slope = -1, intercept = 1.075, color = 'red', size = 1.2, linetype = 'dashed') +
  coord_fixed( ratio = 1 ) +
  theme_minimal() +
  ggtitle("Contour lines of a couple of gaussian independent random variables") +
  theme( plot.title = element_text(hjust = 0.5) )

cauchy_contour_plot <- ggplot( data = don_cauchy ) +
  aes( x = x_cauchy, y = y_cauchy, z = cauchy_density ) +
  geom_contour( colour = "black", binwidth = 0.003) +
  geom_abline(slope = -1, intercept = 4.125, color = 'red', size = 1) +
  #coord_fixed( ratio = 1 ) +
  coord_cartesian(xlim = c(-6, 6), ylim = c(-6, 6)) +
  theme_minimal() +
  ggtitle("Contour lines of a couple of Cauchy independent random variables") +
  theme( plot.title = element_text(hjust = 0.5) )

print(gauss_density_plot)
print(cauchy_density_plot)
print(gauss_contour_plot)
print(cauchy_contour_plot)

# ggsave("gauss_density_plot.png", gauss_density_plot, dpi = "retina")
# ggsave("cauchy_density_plot.png", cauchy_density_plot, dpi = "retina")
# ggsave("gauss_contour_plot.png", gauss_contour_plot, dpi = "retina")
# ggsave("cauchy_contour_plot.png", cauchy_contour_plot, dpi = "retina")
