data <- read.csv("C:/Users/igorw/Desktop/studia/sem3/twd1/Table2_5.csv")
hosp <- read.csv("C:/Users/igorw/Desktop/studia/sem3/twd1/hospitaladm.csv")
teen <- read.csv("C:/Users/igorw/Desktop/studia/sem3/twd1/TeenDepr.csv")

data %>%
  left_join(hosp, x.by = "Country", y.by = "Location") %>%
  filter(c("Country", "Divorce_rate", "FactValueNumeric"))

merged_df <- data %>%
  inner_join(hosp, by = c("Country" = "Location")) %>%
  select(Country, Divorce_rate, FactValueNumeric)

#zrob wykres zaleznosci miedzy Divorce_rate a FactValueNumeric z merged_df
ggplot(merged_df, aes(x = Divorce_rate, y = FactValueNumeric)) +
  geom_point() 


library(tidyr)

merged_data <- merge(teen, data, by = "Country") %>%
  mutate(Divorce_category = cut(Divorce_rate,
                                breaks = c(-Inf, 2., 3.0, 3.5, Inf),
                                labels = c("low", "medium", "high", "very high"))) %>%
  group_by(Divorce_category) %>%
  summarise(across(c(depression, anxiety, OCD, insomnia), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = c(depression, anxiety, OCD, insomnia),
               names_to = "Condition",
               values_to = "Mean_Percentage") %>%
  ggplot(aes(x = Divorce_category, y = Mean_Percentage, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("white", "lightblue", "#3399FF", "darkblue")) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )
ggsave("exported_chart.png", plot = last_plot(), bg = "transparent", width = 8, height = 5)


merged_data
