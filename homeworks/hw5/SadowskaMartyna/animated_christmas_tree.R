library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ambient)
library(ggthemes)
library(gifski)
library(patchwork)
library(tictoc)


# Współrzędne x i y

tree_x <- function(shift, n) {
  x <- c(
    -7, -3, -5, -2, -3.5, 0,
    3.5, 2, 5, 3, 7, -7
  ) + shift
  interpolated_x <- approx(seq_along(x), x, n = n)$y
  return(interpolated_x)
}

tree_y <- function(shift, n) {
  y <- c(
    0, 5, 5, 9, 9, 13,
    9, 9, 5, 5, 0, 0
  ) + shift
  interpolated_y <- approx(seq_along(y), y, n = n)$y
  return(interpolated_y)
}

star_x <- function(shift, n) {
  center_x <- 0.2
  r <- 1
  angles <- seq(0, 2 * pi, length.out = 11)
  radii <- rep(c(r, r / 2), length.out = 11)
  x <- center_x + radii * sin(angles) + shift
  interpolated_x <- approx(seq_along(x), x, n = n)$y
  return(interpolated_x)
}

star_y <- function(shift, n) {
  center_y <- 14
  r <- 1
  angles <- seq(0, 2 * pi, length.out = 11)
  radii <- rep(c(r, r / 2), length.out = 11)
  y <- center_y + radii * cos(angles) + shift
  interpolated_y <- approx(seq_along(y), y, n = n)$y
  return(interpolated_y)
}


# Funkcje generujące jeden element

tree <- function(x_shift, y_shift, r, seed = NULL, id = NA, w_min = 0, w_max = 4, rot = 0, n = 1000) {
  if(!is.null(seed)) set.seed(seed)
  shift <- runif(11, min = 0, max = 0.5)
  shift <- c(shift, shift[1])
  angle = seq(0, 2*pi, length.out = n)
  tibble(
    x = tree_x(shift, n),
    y = tree_y(shift, n),
    width = fracture(
      x = cos(angle + rot),
      y = sin(angle + rot),
      freq_init = 0.3,
      noise = gen_perlin,
      fractal = fbm,
      octaves = 2
    ) %>% normalise(to = c(w_min, w_max)),
    id = id
  )
}

bauble <- function(x_shift, y_shift, r, seed = NULL, id = NA, w_min = 0, w_max = 4, rot = 0, n = 1000) {
  if(!is.null(seed)) set.seed(seed)
  radius = 1 - runif(1, min = r, max = r+0.1)
  angle = seq(0, 2*pi, length.out = n)
  tibble(
    x = radius * cos(angle) + x_shift,
    y = radius * sin(angle) + y_shift,
    width = fracture(
      x = cos(angle + rot),
      y = sin(angle + rot),
      freq_init = 0.3,
      noise = gen_perlin,
      fractal = fbm,
      octaves = 2
    ) %>% normalise(to = c(w_min, w_max)),
    id = id
  )
}

star <- function(x_shift, y_shift, r, seed = NULL, id = NA, w_min = 0, w_max = 0.5, rot = 0, n = 1000) {
  if(!is.null(seed)) set.seed(seed)
  shift <- runif(10, min = 0, max = 0.1)
  shift <- c(shift, shift[1])
  angle = seq(0, 2*pi, length.out = n)
  tibble(
    x = star_x(shift, n),
    y = star_y(shift, n),
    width = fracture(
      x = cos(angle + rot),
      y = sin(angle + rot),
      freq_init = 0.3,
      noise = gen_perlin,
      fractal = fbm,
      octaves = 2
    ) %>% normalise(to = c(w_min, w_max)),
    id = id
  )
}


# Funkcje tworzące więcej elementów

tree_data <- function(ntrees = 10, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  palette_tree <- c(
    "#98FB98",
    "#66CDAA",
    "#3CB371",
    "#2E8B57",
    "#228B22",
    "#4B8A3D",
    "#006400",
    "#2F4F4F",
    "#36454F",
    "#1B4D3E"
  )
  
  tree_settings <- tibble(
    id = 1:ntrees,
    n = 5000,
    w_min = -10,
    w_max = 10,
    rot = runif(ntrees, -pi, pi),
    x_shift = 0,
    y_shift = 0,
    r = 0
  )
  
  tree_settings %>%
    pmap_dfr(tree) %>%
    group_by(id) %>%
    mutate(
      shade = sample(palette_tree, 1),
      width = abs(width)
    )
  
}

baubles_data <- function(nlayers = 2, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  palette_baubles <- c(
    "#D7263D",
    "#FF5700",
    "#FBB13C",
    "#FFD700",
    "#6ECB63",
    "#0B7189",
    "#1D4E89",
    "#7031B9",
    "#e34285",
    "#FFFFFF"
  )
  
  nbaubles = 15
  x_0 <- c(
    4, 1.5, -1.7, -4,
    3, 1, -1.8, -2.5, 2.5, 0,
    1.8, -1, 1, -1.6,
    0.2
  )
  y_0 <- c(
    1, 0.7, 1.2, 2,
    3, 3.4, 4, 6, 5.3, 5.4,
    7.2, 8, 9, 9.5,
    11.2
  )
  
  baubles_settings <- tibble(
    id = 1:(nlayers*nbaubles),
    n = 5000,
    w_min = -3,
    w_max = 3,
    rot = runif(nlayers*nbaubles, -pi, pi),
    x_shift = rep(x_0, each = nlayers),
    y_shift = rep(y_0, each = nlayers),
    r = rep(runif(nbaubles, 1.2, 1.6), each = nlayers)
  )
  
  baubles_settings %>%
    pmap_dfr(bauble) %>%
    group_by(id) %>%
    mutate(
      shade = sample(palette_baubles, 1),
      width = abs(width)
    )
  
}

lights_data <- function(seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  nlights = 44
  x_1 <- seq(-1.7, 2.4, length.out = 9)
  y_1 <- 1/10 * (x_1 - 1)^2 + 10
  x_2 <- seq(-2.7, 3.8, length.out = 14)
  y_2 <- 1/12 * (x_2 - 1.5)^2 + 6.2
  x_3 <- seq(-3.8, 5, length.out = 21)
  y_3 <- 1/20 * (x_3 - 2)^2 + 2
  
  lights_settings <- tibble(
    id = 1:nlights,
    x = c(x_1, x_2, x_3),
    y = c(y_1, y_2, y_3),
    shade = "#FCFBCA",
    size = 3.5,
    alpha = 1
  )
  
}

star_data <- function(nstars = 3, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  palette_star <- c(
    "#FFFACD",
    "#FFEC8B",
    "#FFC107",
    "#FFA500",
    "#FFD700",
    "#fffa00"
  )
  
  star_settings <- tibble(
    id = 1:nstars,
    n = 5000,
    w_min = -6,
    w_max = 6,
    rot = runif(nstars, -pi, pi),
    x_shift = 0,
    y_shift = 0,
    r = 0
  )
  
  star_settings %>%
    pmap_dfr(star) %>%
    group_by(id) %>%
    mutate(
      shade = sample(palette_star, 1),
      width = abs(width)
    )
  
}


# Funkcje rysujące wykresy

generate_path <- function(dat) {
  ggplot(dat, aes(x, y, group = id, size = width, colour = shade)) +
    geom_path(show.legend = FALSE) +
    theme_void() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_colour_identity() +
    scale_size_identity() +
    coord_fixed(xlim = c(-12, 12), ylim = c(-4, 20))
}

generate_point <- function(dat) {
  ggplot(dat, aes(x, y, group = id, size = size, colour = shade, alpha = alpha)) +
    geom_point(show.legend = FALSE) +
    theme_void() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_colour_identity() +
    scale_size_identity() +
    scale_alpha_identity() +
    coord_fixed(xlim = c(-12, 12), ylim = c(-4, 20))
}


# Generowanie GIFa

generate_one_frame <- function(dat_trees, dat_baubles, dat_lights_static, dat_lights_nonstatic, dat_star) {
  
  pic_trees <- generate_path(dat_trees)
  pic_baubles <- generate_path(dat_baubles)
  pic_lights_static <- generate_point(dat_lights_static)
  pic_lights_nonstatic <- generate_point(dat_lights_nonstatic)
  pic_star <- generate_path(dat_star)
  
  pic_trees_grob <- ggplotGrob(pic_trees)
  pic_baubles_grob <- ggplotGrob(pic_baubles)
  pic_lights_static_grob <- ggplotGrob(pic_lights_static)
  pic_lights_nonstatic_grob <- ggplotGrob(pic_lights_nonstatic)
  pic_star_grob <- ggplotGrob(pic_star)
  
  combined_pic <- pic_trees +
    annotation_custom(grob = pic_baubles_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_custom(grob = pic_lights_nonstatic_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_custom(grob = pic_lights_static_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_custom(grob = pic_star_grob, xmin= -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  
  grid.newpage()
  grid.draw(ggplotGrob(combined_pic))
}

rotate_vector <- function(x, percent) {
  
  len <- length(x)
  ind <- ceiling(len * percent)
  if(ind == 0) return(x)
  if(ind == len) return(x)
  c(x[(ind+1):len], x[1:ind])
}

change_lights_size <- function(x, percent) {
  val <- percent * 7 * pi
  x <- 1.5 * sin(val) + 5
}

generate_all_frames <- function(dat_trees, dat_baubles, dat_lights_static, dat_lights_nonstatic, dat_star, nframes = 200) {
  
  for(frame in 1:nframes) {
    dat_trees_ <- dat_trees %>%
      group_by(id) %>%
      mutate(width = width %>% rotate_vector(frame / nframes))
    dat_baubles_ <- dat_baubles %>%
      group_by(id) %>%
      mutate(width = width %>% rotate_vector(frame / nframes))
    dat_lights_nonstatic_ <- dat_lights_nonstatic %>%
      group_by(id) %>%
      mutate(size = size %>% change_lights_size(frame/ nframes))
    dat_lights_static_ <- dat_lights_static %>%
      group_by(id)
    dat_star_ <- dat_star %>%
      group_by(id) %>%
      mutate(width = width %>% rotate_vector(frame / nframes))
    generate_one_frame(dat_trees_, dat_baubles_, dat_lights_static_, dat_lights_nonstatic_, dat_star_)
  }
}

animated_tree <- function(seed, ...) {
  trees <- tree_data(seed = seed, ...)
  baubles <- baubles_data(seed = seed, ...)
  lights_static <- lights_data(seed = seed, ...)
  lights_nonstatic <- lights_static %>%
    mutate(alpha = 0.7)
  stars <- star_data(seed = seed, ...)
  save_gif(
    expr = generate_all_frames(trees, baubles, lights_static, lights_nonstatic, stars),
    gif_file = paste0("animated-christmas-tree-", seed, ".gif"),
    height = 1000,
    width = 1000,
    delay = 0.1,
    progress = TRUE,
    bg = "#222222"
  )
  invisible(NULL)
}

# tic()
animated_tree(seed = 234)
# animated_tree(seed = 100)
# animated_tree(seed = 666)
# animated_tree(seed = 123)
# animated_tree(seed = 999)
# toc()