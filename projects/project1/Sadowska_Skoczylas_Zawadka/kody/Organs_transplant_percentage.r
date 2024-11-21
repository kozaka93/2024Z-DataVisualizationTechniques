install.packages('readxl')
library("readxl")
library(dplyr)
library(tidyr)

# procent wykonanych przeszczepów
Organs <- read_excel("Organ_Donation_and_Transplantation_Data.xlsx", sheet = "Overview - National")
Organ_table <- Organs %>% replace(Organs == ".", "0") %>%
  mutate("Number of donor organ transplant recipients" = 
                    as.numeric(`Number of deceased donor organ transplant recipients`) + 
                    as.numeric(`Number of living donor organ transplant recipients`)) %>%
  separate_rows(Organ, sep = "-") %>%
  group_by(Organ) %>% summarise("Number of transplants" = sum(`Number of donor organ transplant recipients`)) %>%
  filter(Organ != "All") %>%
  mutate("Percent" = round(`Number of transplants` / sum(`Number of transplants`) * 100, 2)) %>%
  arrange(-Percent)
