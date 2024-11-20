library(dplyr)
library(tidyr)
library(ggplot2)
library(png)
library(grid)
df <- read.csv("../data/Final-player.csv")
View(df)
t <- df %>% select(injuries)
inj <- df %>% mutate(injury = lapply(injuries, 
                                function(x) strsplit(x[[1]], ",")[[1]][2])) %>% 
  select(injury)

. <- inj %>% group_by(injury) %>% summarize(count = n()) %>% 
  arrange(desc(count))
View(.)

t <- inj %>% 
  mutate(body.part = case_when(
    grepl("knee|meniscus|ligament|patelar", injury, ignore.case = TRUE) ~ "knee",
    grepl("facial|head|eye", injury, ignore.case = TRUE) ~ "face",
    grepl("hip", injury, ignore.case = TRUE) ~ "hip",
    grepl("rib", injury, ignore.case = TRUE) ~ "rib",
    grepl("foot|toe", injury, ignore.case = TRUE) ~ "foot",
    grepl("hamstring|thighs", injury, ignore.case = TRUE) ~ "thigh",
    grepl("ankle", injury, ignore.case = TRUE) ~ "ankle",
    grepl("calf|achilles|shin", injury, ignore.case = TRUE) ~ "calf",
    grepl("groin", injury, ignore.case = TRUE) ~ "groin",
    grepl("back", injury, ignore.case = TRUE) ~ "back",
    grepl("hand", injury, ignore.case = TRUE) ~ "hand",
    grepl("elbow", injury, ignore.case = TRUE) ~ "elbow",
    grepl("arm", injury, ignore.case = TRUE) ~ "arm",
    grepl("finger", injury, ignore.case = TRUE) ~ "finger",
    grepl("lumbago", injury, ignore.case = TRUE) ~ "low back"
  )) %>% filter(!(is.na(body.part))) %>% 
  group_by(body.part) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))



View(t)

