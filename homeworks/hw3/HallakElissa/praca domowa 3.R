library(dplyr)
library(ggplot2)
library(tidyr)
dane_pkb <- data.frame(
  Rok = c(
    "2007 Q1", "2007 Q2", "2007 Q3", "2007 Q4",
    "2008 Q1", "2008 Q2", "2008 Q3", "2008 Q4",
    "2009 Q1", "2009 Q2", "2009 Q3", "2009 Q4",
    "2010 Q1", "2010 Q2", "2010 Q3", "2010 Q4",
    "2011 Q1", "2011 Q2", "2011 Q3", "2011 Q4",
    "2012 Q1", "2012 Q2", "2012 Q3", "2012 Q4",
    "2013 Q1", "2013 Q2", "2013 Q3", "2013 Q4",
    "2014 Q1", "2014 Q2", "2014 Q3", "2014 Q4",
    "2015 Q1", "2015 Q2", "2015 Q3", "2015 Q4",
    "2016 Q1", "2016 Q2", "2016 Q3", "2016 Q4",
    "2017 Q1", "2017 Q2", "2017 Q3", "2017 Q4",
    "2018 Q1", "2018 Q2", "2018 Q3", "2018 Q4",
    "2019 Q1", "2019 Q2", "2019 Q3", "2019 Q4",
    "2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4",
    "2021 Q1", "2021 Q2", "2021 Q3", "2021 Q4",
    "2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4",
    "2023 Q1", "2023 Q2", "2023 Q3", "2023 Q4",
    "2024 Q1", "2024 Q2", "2024 Q3"
  ),
  PKB = c(
    0.9, 2.3, 2.2, 2.5,
    -2.3, 2.1, -2.1, -8.5, #!!!!!!!!!
    -4.4, -0.6, 1.5, 4.5,
    1.5, 3.7, 3.0, 2.0,
    -1.0, 2.9, -0.1, 4.7,
    3.2, 1.7, 0.5, 0.5,
    3.6, 0.5, 3.2, 3.2,
    -1.2, 5.5, 5.0, 2.3,
    3.2, 3.0, 1.3, 0.1,
    2.0, 1.9, 2.2, 2.0,
    2.3, 2.2, 3.2, 3.5,
    2.5, 3.5, 2.9, 1.1,
    3.1, 2.0, 2.1, 2.1,
    -5.0, -31.4, 33.4, 4.3,
    6.4, 6.7, 2.3, 7.0,
    -2.0, -0.6, 2.7, 2.6,
    2.2, 2.1, 4.9, 3.4,  
    1.4, 3.0, 2.8  
  )
)
dane_pkb
str(dane_pkb)

ggplot(dane_pkb, aes(x = Rok, y = PKB, fill = PKB > 0)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("TRUE" = "#77AD78", "FALSE" = "#C00707")) +
  labs(title = "Dynamika PKB Stanów Zjednoczonych w ujęciu kwartalnym",
       x = "Rok",
       y = "PKB (%)") +
  theme_minimal() +
  scale_x_discrete(
    breaks = c("2007 Q2", "2008 Q2", "2009 Q2", "2010 Q2", "2011 Q2","2012 Q2", "2013 Q2", "2014 Q2", "2015 Q2", "2016 Q2","2017 Q2", "2018 Q2", "2019 Q2", "2020 Q2", "2021 Q2","2022 Q2", "2023 Q2", "2024 Q2"),
    labels = c("2007", "2008", "2009", "2010", "2011","2012", "2013", "2014", "2015", "2016","2017", "2018", "2019", "2020", "2021","2022", "2023", "2024")) +            
  guides(fill = "none") +
  geom_text(aes(label = PKB, 
                vjust = ifelse(PKB > 0, -0.3, 1.3),  # Pozycjonowanie tekstu nad lub pod słupkiem
                fontface = "bold"), size = 2)+
  
  
  geom_vline(xintercept = seq(4, length(dane_pkb$Rok), by = 4) + 0.5, linetype = "dashed", color = "gray", size = 0.5)+
  
  
  
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 65, face = "bold")
  )+
  scale_y_continuous(breaks = seq(min(round(dane_pkb$PKB),0), max(dane_pkb$PKB), by = 4))
  
  
  
  
  
  
  
  
#Początkowy wykres nie ma podpisanych osi, są wartości, które wychodzą poza skalę.
#Lata, które znajdują się na osi Y nie są widoczne oraz ogólna kolorystyka wykresu utrudnia jego czytanie

