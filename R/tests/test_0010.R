# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Modifiable parameters --------------------------------------------------------

iteration_id <- "test_0010"
seed_num <- 4878210
initial_grid_size <- 100
warp_factor <- 50 # lower = more warping
line_colour <- "#000000"
bg_colour <- "#FFFFFF"

# Make some noise --------------------------------------------------------------

# Generate data
set.seed(seed_num)
grid <- long_grid(
  seq(0, 3, length.out = initial_grid_size),
  seq(0, 3, length.out = initial_grid_size)) %>%
  mutate(
    curl = curl_noise(gen_perlin, x = x, y = y)) %>%
  purrr::reduce(data.frame) %>%
  rename(x = out, y = elt, curl_x = x, curl_y = y) %>%
  mutate(
    x_warped = x + (curl_x / warp_factor),
    y_warped = y + (curl_y / warp_factor),
    size = sample(seq(5, 15, by = 0.1), n(), replace = TRUE),
    subset = sample(1:20, n(), replace = TRUE),
    colour = sample(
      c("#3D348B", "#5C96AD", "#F7B801"), n(), replace = TRUE))

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_curve(
    data = grid,
    aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size,
        colour = colour),
    curvature = -0.2) +
  geom_point(
    data = grid,
    aes(x = x_warped, y = y_warped, size = size / 6),
    colour = bg_colour) +
  geom_point(
    data = grid %>% filter(subset >= 10),
    aes(x = x_warped, y = y_warped, size = size / 3, colour = colour),
    shape = 0) +
  geom_path(
    data = grid %>% filter(subset < 10),
    aes(x = x_warped, y = y_warped),
    colour = line_colour, size = 0.1, linetype = "dotted") +
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
