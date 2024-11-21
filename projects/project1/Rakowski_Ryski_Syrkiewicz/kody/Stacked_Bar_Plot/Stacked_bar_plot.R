library(dplyr)
library(tidyr)
library(ggplot2)


#Creating data frame
df <- read.csv("Bar_Plot.csv", sep=";")
df2 <- df %>% filter(!grepl("Male", Data) & !grepl("Female", Data) & !endsWith(Data, "WITHOUT Synthetic Opioids other than Methadone") & !endsWith(Data, "AND Synthetic Opioids other than Methadone"));
df3 <- df2[-c(3,6, 7, 8),-c(26)]
df3$Data <- c("Total Overdose Deaths", "Any Opioid", "Fentanyl", "Heroin", "Cocaine", "Cocaine + Any Opioid", "Cocaine w/o Opioids", "Meth & SAP", "Meth & SAP + Any Opioid", "Meth & SAP w/o Opioids", "Benzodiazepines", "Benzodiazepines + Any Opioid", "Benzodiazepines w/o Opioids", "Antidepressants", "Antidepressants + Any Opioid", "Antidepressants w/o Opioids")
df4 <- df3[-c(5,8,11,14), ]
df5 <- t(df4)
colnames(df5)<- df5[1,]
df6 <- as.data.frame(df5[-1, , drop = FALSE])

df7 <- df6 %>% mutate(across(everything(), ~ as.numeric(gsub(",", ".", .)))) %>%
  mutate(`Any Opioid` = round(`Any Opioid` - `Fentanyl` - `Heroin`, digits=2)) %>% mutate(`Any Opioid` = ifelse(`Any Opioid` < 0, 0, `Any Opioid`)) %>%
  rename(`Other Opioids` = `Any Opioid`)
years <- rownames(df7)
colnames <- colnames(df7)
df8 <- expand.grid(year = years, drug = colnames)
df9 <- df8 %>%
  mutate(
    value = mapply(function(y, d) df7[y, d], as.character(year), as.character(drug))
  )

#Choosing color palette
palette <- c(  "#E39EC1", "#D4A8D4", "#C47AC0", "#77567A", "#464955","#2F323A", "#C47AC0")
#Final mutates
df10 <- df9 %>% filter(drug != "Total Overdose Deaths") %>% mutate(year  = as.character(year)) %>% mutate(year = substr(year, nchar(year) - 3, nchar(year)))
df11 <- df10 %>% filter(as.numeric(year)> 2010)
df12 <- df11 %>%
  mutate(drug = case_when(
    drug %in% c("Other Opioids", "Antidepressants") ~ "Other",
    drug %in% c("Cocaine + Any Opioid", "Cocaine w/o Opioids") ~ "Cocaine",
    drug %in% c("Meth & SAP w/o Opioids", "Meth & SAP + Any Opioid") ~ "Meth",
    drug %in% c("Benzodiazepines w/o Opioids", "Benzodiazepines + Any Opioid") ~ "Benzodiazepines",
    drug %in% c("Antidepressants + Any Opioid", "Antidepressants w/o Opioids") ~ "Other",
    TRUE ~ drug
  ))

#Creating our plot
ggplot(df12, aes(x = year, y = value, fill = drug)) +
  geom_col() +
  labs(
    title = "Rate of National Drug Overdose Deaths",
    subtitle = "Rates are Age-Adjusted per 100,000 population",
    y = "Number of people",
    x = "Years",
    fill = "Drugs"
  ) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12, face = "bold")
      )

