library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)


daneMW <- read.csv("daneMW.csv")
daneWB <- read.csv("daneWB.csv")

daneMW$student <- "Maciek"
daneWB$student <- "Wojtek"
dane <- rbind(daneMW, daneWB)


spanie <- dane %>% 
  filter(activity.name == 'Spanie') %>% 
  select(time.started, time.ended, record.tags, duration.minutes, student) %>% 
  mutate(date1 = as.Date(substr(time.started, 1, 10)),
         date2 = as.Date(substr(time.ended, 1, 10)),
         time1 = as.POSIXct(substr(time.started, 12, 19), format = "%H:%M:%S", tz = "UTC"),
         time2 = as.POSIXct(substr(time.ended, 12, 19), format = "%H:%M:%S", tz = "UTC")) %>% 
  select(-time.started, -time.ended) %>% 
  mutate(
    przed_pol = ifelse(date1 < date2, as.numeric(difftime(as.POSIXct("23:59:59", format="%H:%M:%S", tz="UTC"), time1, units = "secs")), 0),
    po_pol = ifelse(date1 < date2, as.numeric(difftime(time2, as.POSIXct("00:00:00", format="%H:%M:%S", tz="UTC"), units = "secs")),
                    ifelse(date1 == date2, as.numeric(difftime(time2, time1, units = "secs")), 0))
  ) %>% 
  mutate(time = przed_pol + lag(po_pol, default = 0),
         time = (przed_pol + lag(po_pol, default = 0)) / 60) 



nauka <- dane %>% 
  filter(activity.name == 'Nauka') %>% 
  mutate(
    time.started = as.POSIXct(time.started, format = "%Y-%m-%d %H:%M:%S"), 
    day.of.week = weekdays(time.started)
  ) %>%
  filter(!is.na(day.of.week), !is.na(record.tags), !is.na(student)) %>% 
  filter(record.tags != 'Hiszpański' & record.tags != 'Drugi język') %>% 
  mutate(
    day.of.week.num = as.numeric(factor(day.of.week, levels = c(
      "poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"
    )))) %>%
  mutate(start.hour = as.numeric(format(time.started, "%H"))) %>% 
  mutate(
    day.of.week = factor(day.of.week, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))
  ) %>% 
  mutate(date1 = as.Date(substr(time.started, 1, 10)),
         date2 = as.Date(substr(time.ended, 1, 10)))


projekty <- nauka %>% 
  filter(comment == 'Projekt') 

kolosy <- nauka %>% 
  filter(comment == 'Kolos') 


nauka_z_wyspaniem <- dane %>%
  mutate(
    stan_temp = case_when(
      grepl("Niewyspany", record.tags) ~ "Niewyspany",
      grepl("Wyspany", record.tags) ~ "Wyspany",
      TRUE ~ NA_character_
    )
  ) %>%
  tidyr::fill(stan_temp, .direction = "down") %>%
  mutate(stan = stan_temp) %>%
  select(-stan_temp) %>% 
  filter(activity.name == 'Nauka') %>% 
  mutate(date1 = as.Date(substr(time.started, 1, 10)),
         date2 = as.Date(substr(time.ended, 1, 10))) %>% 
  mutate(
    time.started = as.POSIXct(time.started, format = "%Y-%m-%d %H:%M:%S"), 
    day.of.week = weekdays(time.started)
  ) %>% 
  select(date1, date2, day.of.week, stan, student, duration.minutes) %>% 
  group_by(date1, date2, day.of.week, stan, student) %>%
  mutate(mean.time = mean(duration.minutes)) %>%
  ungroup() %>% 
  select(-duration.minutes) %>% 
  unique() %>% 
  mutate(mean.time = signif(mean.time, digits = 3)) %>% 
  mutate(
    day.of.week = factor(day.of.week, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))
  ) 


mini <- dane %>% 
  filter(record.tags == 'Studia') %>% 
  mutate(
    time.started = as.POSIXct(time.started, format = "%Y-%m-%d %H:%M:%S"), 
    day.of.week = weekdays(time.started),
    date1 = as.Date(substr(time.started, 1, 10)),
    date2 = as.Date(substr(time.ended, 1, 10))
  )

wolne <- dane %>% 
  mutate(
    time.started = as.POSIXct(time.started, format = "%Y-%m-%d %H:%M:%S"), 
    day.of.week = weekdays(time.started),
    date1 = as.Date(substr(time.started, 1, 10)),
    date2 = as.Date(substr(time.ended, 1, 10))
  )
