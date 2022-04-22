# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

source(here::here("R/functions/fx_weave.R"))

# Modifiable parameters --------------------------------------------------------

iteration_id <- "test_0006"
seed_num <- 127816
initial_grid_size <- 200
warp_factor <- 30 # lower = more warping
line_colour <- "#000000"
bg_colour <- "#FFFFFF"

# Make some noise --------------------------------------------------------------

# Generate data
grid <- weave(
  seed = seed_num, grid_min = 0, grid_max = 3, grid_length = initial_grid_size,
  warp_factor = warp_factor, geom_size_min = 0.5, geom_size_max = 3,
  noise_type = "cubic")

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_curve(
    data = grid,
    aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size),
    colour = line_colour, curvature = -0.2) +
  scale_size_identity() +
  scale_colour_identity() +
  coord_equal(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin     = margin(100,100,100,100, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  device = "png", width = 6000, height = 6000, units = "px", dpi = 600)
