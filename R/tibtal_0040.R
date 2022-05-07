# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Modifiable parameters --------------------------------------------------------

iteration_id <- "tibtal_0040"
seed_num <- 135740
initial_grid_size <- 70
warp_factor <- 20 # lower = more warping
line_colour <- "#999999"
dot_colour <- "#DDDDDD"
bg_colour <- "#222222"

# Make some noise --------------------------------------------------------------

# Generate data
set.seed(seed_num)
grid <- long_grid(
  seq(0, 1, length.out = initial_grid_size),
  seq(0, 1, length.out = initial_grid_size)) %>%
  mutate(
    curl = curl_noise(gen_simplex, x = x, y = y)) %>%
  purrr::reduce(data.frame) %>%
  rename(x = out, y = elt, curl_x = x, curl_y = y) %>%
  mutate(
    x_warped = x + (curl_x / warp_factor),
    y_warped = y + (curl_y / warp_factor),
    size = sample(seq(0.2, 1.5, by = 0.1), n(), replace = TRUE),
    subset = sample(1:20, n(), replace = TRUE))

# Build plot -------------------------------------------------------------------

ggplot() +
  #geom_path(
  #  data = grid,
  #  aes(x = x_warped, y = y_warped),
  #  colour = line_colour, size = 0.2, linetype = "dotted") +
  geom_point(
    data = grid,
    aes(x = x_warped, y = y_warped, size = size),
    shape = 16, colour = dot_colour) +
  scale_size_identity() +
  coord_equal(expand = TRUE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin     = margin(100,100,100,100, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  device = "png", width = 6000, height = 6000, units = "px", dpi = 600)
