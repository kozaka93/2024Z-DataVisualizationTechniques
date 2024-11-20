library(dplyr)
library(tidyr)
library(ggplot2)
library(png)
library(showtext)
library(grid)

showtext_auto()
font_add_google(name = "Roboto", family = "Roboto")
font_add_google(name = "Lobster", family = "Lobster")
font_add_google(name = "Roboto Serif", family = "Roboto Serif")

premier_player_injuries <- read.csv("data/premier_player_injuries.csv")

t <- premier_player_injuries %>% 
  mutate(body.part = case_when(
    grepl("knee|meniscus|ligament|patelar", injury, ignore.case = TRUE) ~ "Knee",
    grepl("facial|head|eye", injury, ignore.case = TRUE) ~ "Head",
    grepl("hip", injury, ignore.case = TRUE) ~ "Hip",
    grepl("rib", injury, ignore.case = TRUE) ~ "Rib",
    grepl("foot|toe", injury, ignore.case = TRUE) ~ "Foot",
    grepl("hamstring|thighs", injury, ignore.case = TRUE) ~ "Thigh",
    grepl("ankle", injury, ignore.case = TRUE) ~ "Ankle",
    grepl("calf|achilles|shin", injury, ignore.case = TRUE) ~ "Calf",
    grepl("groin", injury, ignore.case = TRUE) ~ "Groin",
    grepl("back|lumbago", injury, ignore.case = TRUE) ~ "Back",
    grepl("hand", injury, ignore.case = TRUE) ~ "Hand",
    grepl("elbow", injury, ignore.case = TRUE) ~ "Elbow",
    grepl("arm", injury, ignore.case = TRUE) ~ "Arm",
    grepl("finger", injury, ignore.case = TRUE) ~ "Finger"
  )) %>% filter(!(is.na(body.part))) %>% 
  group_by(body.part) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

  body_image = readPNG("resources/cr2.png")
  body_image <- rasterGrob(body_image, width = unit(1, "npc"), height = unit(1, "npc"))

t <- t %>% 
  mutate(x = c(0.55, 0.35, 0.1, 0.2, 0.49, 0.29, 0.4, 0.4, 0.61, 0.13, 0.46, 0.37, 0.7, 0.3),  
         y = c(0.34, 0.33, 0.1, 0.2, 0.47, 0.14, 0.65, 0.51, 0.94, 0.73, 0.7, 0.85, 0.7, 0.84),  
  ) %>% 
  mutate(label = paste(body.part, as.character(count), sep = "\n")) %>% 
  mutate(radius = if_else(count > 100, 
                          sqrt(count)*0.0025, 
                          0.02 + count/10000)) %>% 
  mutate(label_y = if_else(radius > 0.04, 
                           y + radius - 0.036, 
                           y + radius + 0.02)) %>% 
  mutate(num_y = if_else(radius > 0.04, y - 0.02, y)) %>% 
  mutate(num_size = if_else(radius >  0.04, 4.5, 3)) %>% 
  mutate(fraction = count/sum(count), fraction = round(fraction*100, 1)) %>% 
  mutate(fraction = paste(fraction, "%", sep = ""))

create_circle <- function(x_center, y_center, radius, linewidth, n_points = 1000) {
  theta <- seq(0, 2 * pi, length.out = n_points)
  data.frame(
    x_rim = x_center + radius * cos(theta),
    y_rim = y_center + radius * sin(theta),
    linewidth = linewidth
  )
}

circle_data <- t %>%
  rowwise() %>%
  do(cbind(., create_circle(.$x, .$y, .$radius, sqrt(.$radius*50))))

t$num_size[c(4, 5)] <- c(4, 4)
t$num_size[8] <- 2.3
t$num_size[10:14] <- rep(2, 5)

t <- t %>% mutate(label_x = x)
t$label_y[c(3, 4, 6, 7, 9, 11, 13)] <- c(t$label_y[3] + 0.06, 
                        t$label_y[4] + 0.05,
                        t$label_y[6] - 0.06,
                        t$label_y[7] - 0.02,
                        t$label_y[9] + 0.03,
                        t$label_y[11] - 0.09,
                        t$label_y[13] - 0.03)
t$label_x[c(1, 2, 5, 6, 7, 8, 11, 13, 14)] <- c(
                        t$label_x[1] + 0.13,
                        t$label_x[2] - 0.12,
                        t$label_x[5] + 0.1,
                        t$label_x[6] + 0.07,
                        t$label_x[7] - 0.06,
                        t$label_x[8] + 0.015,
                        t$label_x[11] + 0.04,
                        t$label_x[13] + 0.08,
                        t$label_x[14] - 0.02)
t$num_y[1:5] <- t$y[1:5]

p <- ggplot(circle_data, aes(x = x_rim, y = y_rim, 
                        group = body.part 
                        ), fill = "#DBE2EF") +
  annotation_custom(body_image, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
  geom_polygon(alpha = 0.72, color = "#DBE2EF",
               aes(linewidth = linewidth)) +
  scale_linewidth_identity() +
  coord_equal() + xlim(0, 1) +
  scale_size_identity() +
#  geom_text(data = t, aes(x = label_x, y = label_y, label = body.part),
#            color = "#DBE2EF", size = 40, family = "Roboto Serif") +
  geom_text(data = t, aes(x = x, y = num_y, label = fraction, 
                          size = num_size * 10),
            color = "#DBE2EF", family = "Roboto") +
  theme_void() +
  theme(legend.position = "none") 
ggsave("body_plot.png", plot = p, width = 18, height = 18, units = "in", dpi = 300)
