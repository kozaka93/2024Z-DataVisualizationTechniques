library(dplyr)
library(ggplot2)
install.packages("showtext")
library(showtext)
urodzeni_df <- read.csv("C:/Users/Admin/Downloads/Urodzenia żywe w Polsce 2007-2023.xlsx - Urodzenia żywe 2007-2023.csv")
# Zajmę się tym jak zmieniała się ilość pozostawionych noworodków 
# w Polsce w stosunku do liczby urodzeń od roku 2007
wektor_ur <- urodzeni_df %>% slice(17) %>%
  select(-Województwo) %>% unlist() %>% unname()
porzuceni <- c(720,	775,	726,	798,	765,	918,	849,	772,	762,	709,	551,	472,	527,	506,	643,	571,	627)
tible <- data.frame(daty = substr(colnames(urodzeni_df)[2:ncol(urodzeni_df)],2,nchar(colnames(urodzeni_df)[2:ncol(urodzeni_df)])),
                    wskaznik = porzuceni/(wektor_ur/100000))
p <- ggplot(tible, aes(x = daty, y = wskaznik, group = 1)) +
  geom_line(size = 1, color = "#303174") + scale_y_continuous(limits = c(0,240)) +
  theme_minimal() + theme(plot.background = element_rect(fill = "white", color = NA),
                          text = element_text(family = "Tenor Sans"), size = 16,
                          plot.title = element_text(size = 20), 
                          axis.title.x = element_text(size = 14), 
                          axis.title.y = element_text(size = 14),  
                          axis.text.x = element_text(size = 12), 
                          axis.text.y = element_text(size = 12)) +
  labs(title = "Noworodki pozostawione w szpitalu w ostatnich latach",
                         subtitle = "Dane z końca każdego roku", 
                         x = "Rok", y ="Liczba porzuconych noworodków na 100tys. urodzeń" ) +
  geom_segment(aes(x = 9.4, y = 100, xend = 9.4, yend = 195),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), color = "#315ca8") +
  geom_label(
    aes(x = 9.4, y = 100, 
        label = "Start 500+"),
    color = "black", fill = "#b5e0f3", size = 5, label.padding = unit(0.5, "lines")
  )
ggsave("plot_output.png", plot = p, width = 8, height = 6, dpi = 150)
# Widzimy, że problem był największy w okolicy 2012. Od 2016 mniej dzieci
# pozostawało w szpitalach, więc możliwe, że programy socjalne mają wpływ na problem.
# Od 2018 roku problem się zaczął nasilać i obecnie jest mocno zauważalnhy w porównaniu do poprzednich lat.
