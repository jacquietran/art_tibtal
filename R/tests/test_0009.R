# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Modifiable parameters --------------------------------------------------------

iteration_id <- "test_0009"
seed_num <- 157109
initial_grid_size <- 500
warp_factor <- 100 # lower = more warping
line_colour <- "#999999"
dot_colour <- "#DDDDDD"
bg_colour <- "#000000"

# Make some noise --------------------------------------------------------------

# Generate data
set.seed(seed_num)
grid <- long_grid(
  seq(0, 20, length.out = initial_grid_size),
  seq(0, 20, length.out = initial_grid_size)) %>%
  mutate(
    curl = curl_noise(gen_value, x = x, y = y)) %>%
  purrr::reduce(data.frame) %>%
  rename(x = out, y = elt, curl_x = x, curl_y = y) %>%
  mutate(
    x_warped = x + (curl_x / warp_factor),
    y_warped = y + (curl_y / warp_factor),
    size = sample(seq(0.2, 1.5, by = 0.1), n(), replace = TRUE),
    subset = sample(1:20, n(), replace = TRUE))

set.seed(seed_num)
grid_cols <- grid %>%
  distinct(x) %>%
  mutate(
    colour = case_when(
      x < 0.4            ~ sample(
        c("#FFFFFF", "#FFFFFF", "#000000"), n(), replace = TRUE,
        prob = c(0.6, 0.3, 0.1)),
      x >= 0.4 & x < 0.8 ~ sample(
        c("#FFFFFF", "#FFFFFF", "#000000"), n(), replace = TRUE,
        prob = c(0.1, 0.6, 0.3)),
      TRUE               ~ sample(
        c("#FFFFFF", "#FFFFFF", "#000000"), n(), replace = TRUE,
        prob = c(0.3, 0.1, 0.6))))

grid_with_colours <- left_join(grid, grid_cols, by = "x")
      

# Build plot -------------------------------------------------------------------

ggplot() +
  #geom_path(
  #  data = grid,
  #  aes(x = x_warped, y = y_warped),
  #  colour = line_colour, size = 0.2, linetype = "dotted") +
  ggfx::as_reference(
    geom_path(
      data = grid_with_colours,
      aes(x = x_warped, y = y_warped, colour = colour, size = size)),
    id = "base") +
  ggfx::with_blend(
    geom_path(
      data = grid_with_colours,
      aes(x = x_warped, y = y_warped, colour = colour, size = size)),
    bg_layer = "base",
    blend_type = "multiply") +
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
