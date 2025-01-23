OlekRaw <- read.csv("../data/sleepdataOlek.csv")
SebastianRaw <- read.csv2("../data/sleepdataSebastian.csv")
PiotrRaw <- read.csv2("../data/sleepdata.xls.csv")

library(dplyr)
library(lubridate)

parse_percentage <- function(str) {
  as.numeric(substr(str, 1, nchar(str)-1))/100
}

process_raw <- function(dt) {
  dt|>
    mutate(
      Sleep.Quality = parse_percentage(Sleep.Quality),
      Regularity = parse_percentage(Regularity),
      Did.snore = Did.snore == "true",
      # Went.to.bed = time_format(Went.to.bed),
      Went.to.bed = ymd_hms(Went.to.bed),
      Woke.up = ymd_hms(Woke.up),
      day = as_date(Woke.up),
      # Woke.up = ymd_hms(Woke.up),
      Coughing..per.hour. = as.numeric(Coughing..per.hour.),
      Movements.per.hour = as.numeric(Movements.per.hour),
      # Wake.up.window.start = if_else(Wake.up.window.start == "",true = NA, false = time_format(Wake.up.window.start)),
      # Wake.up.window.stop = if_else(Wake.up.window.stop == "", true = NA, false = time_format(Wake.up.window.stop)),
    ) |>
    filter(day %within% (ymd("2024-12-01") %--% now())) |>
    select(-c(
      City,
      Alertness.score,
      Alertness.reaction.time..seconds.,
      Alertness.accuracy
    ))
}

# Sebastian <- SebastianRaw |>
#   mutate(
#     Sleep.Quality = parse_percentage(Sleep.Quality),
#     Regularity = parse_percentage(Regularity),
#     Did.snore = Did.snore == "true",
#     Went.to.bed = as.POSIXct(Went.to.bed),
#     Woke.up = as.POSIXct(Woke.up),
#     day = as.Date(Woke.up)
#   ) |>
#   select(-c(
#     City,
#     Alertness.score,
#     Alertness.reaction.time..seconds.,
#     Alertness.accuracy
#   ))

Sebastian <- process_raw(SebastianRaw)
Piotr <- process_raw(PiotrRaw)
Olek <- process_raw(OlekRaw)

# View(Sebastian)

library(ggplot2)
library(tidyr)

# Można zmieniać imię osoby na do wykresu
Piotr |>
  select(Time.in.bed..seconds., Time.asleep..seconds., day) |>
  mutate(Time.not.sleeping = Time.in.bed..seconds. - Time.asleep..seconds.) |>
  pivot_longer(
    cols = c( Time.asleep..seconds.,
             Time.not.sleeping),
    names_to =  "stat",
    values_to = "val"
  ) |>
  arrange(desc(stat)) |>
  # View()
  ggplot(aes(x = day, y = val, colour = stat)) +
  # geom_line() +
  geom_area(aes(fill = stat), alpha = 0.2) +
  ylim(0, NA) +
  scale_color_manual(labels = c("Time asleep in seconds", "Time in bed in seconds"),
                     values = c("blue", "red")) +
  scale_fill_manual(labels = c("Time asleep in seconds", "Time in bed in seconds"),
                     values = c("blue", "red")) +
  labs(
    y = "time",
    title = "Piotr"
  )


Sebastian |>
  mutate(woke_up_inter = interval(day, Woke.up),
         went_to_bed_inter = interval(day, Went.to.bed),
         fell_asleep_time = Went.to.bed + dseconds(Asleep.after..seconds.)) |>
  mutate(fell_asleep_inter = interval(day, fell_asleep_time))|>
  # View()
  ggplot(aes(x = day, y = fell_asleep_inter)) +
  geom_crossbar(aes(ymin = went_to_bed_inter, ymax = fell_asleep_inter), fill = "#887711", colour = NA) +
  geom_crossbar(aes(ymax = woke_up_inter, ymin = fell_asleep_inter), fill = "#223388", colour = NA) +
  scale_y_continuous(labels = (\(x) format(make_datetime(sec = x), "%H:%M"))) +
  labs(
    y = "time of day"
  )

minutes <- Data |>
  mutate(minute_went_to_bed = unclass(interval(day, Went.to.bed)) %/% 60) |>
  mutate(minute_woke_up = unclass(interval(day, Woke.up)) %/% 60)

samples <- min(minutes$minute_went_to_bed):max(minutes$minute_woke_up)

library(ggridges)

minutes |>
  cross_join(tibble(val = samples)) |>
  filter(minute_went_to_bed <= val & val <= minute_woke_up) |>
  # mutate(sleeper = factor(sleeper)) |>
  # group_by(val) |>
  # summarise(days_asleep = sum(was_asleep)) |>
  # View()
  ggplot(aes(x = val, y = sleeper, fill = factor(sleeper))) +
  stat_density_ridges(alpha = 0.6) +
  scale_x_continuous(labels = (\(x) format(make_datetime(min = x), "%H:%M"))) +
  labs(
    x = "time of day",
    y = NULL,
    fill = "sleeper"
  ) +
  theme_ridges() +
  theme(
    axis.text.y = element_blank()
  )
  # geom_density(alpha = 0.3) +
  # xlim(min(minutes$minute_went_to_bed), max(minutes$minute_woke_up))
  # xlim(-100, 1000)

  
library(patchwork)
library(ggalt)
(
(
Sebastian |>
  mutate(woke_up_inter = interval(day, Woke.up),
         went_to_bed_inter = interval(day, Went.to.bed),
         fell_asleep_time = Went.to.bed + dseconds(Asleep.after..seconds.)) |>
  mutate(fell_asleep_inter = interval(day, fell_asleep_time))|>
  # View()
  ggplot(aes(x = day, y = fell_asleep_inter)) +
  geom_crossbar(aes(ymin = went_to_bed_inter, ymax = fell_asleep_inter), fill = "#887711", colour = NA) +
  geom_crossbar(aes(ymax = woke_up_inter, ymin = fell_asleep_inter), fill = "#223388", colour = NA) +
  scale_y_time(labels = (\(x) format(make_datetime(sec = x), "%H:%M")),
               breaks = (\(x) {
                 y <- make_datetime(sec = floor(x[1]):1:(x[2]+1));
                 y <- y[second(y)==0 & minute(y)==0]})) +
  labs(
    y = "time of day"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
) + (
Sebastian |>
  ggplot(aes(x = day, y = Sleep.Quality)) +
  # geom_line() +
  geom_xspline() +
  ylim(min(Data$Sleep.Quality), NA) +
  scale_y_continuous(
    labels = (\(x) paste(100*x, "%"))
      ) +
  labs(
    y = "sleep quality"
  ) +
  scale_x_date(date_breaks = "3 days",
               date_labels = "%b %e")
) + plot_layout(
  guides = "collect",
  nrow = 2,
  ncol = 1,
  heights = c(0.7, 0.3)
))
