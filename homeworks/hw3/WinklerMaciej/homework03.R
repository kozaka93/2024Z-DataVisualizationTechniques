---
title: "Praca domowa 3"
author: "Maciej Winkler"
output:
  html_document:
    toc: true
    toc_float: true
    code_folding: hide
    theme: united
  pdf_document:
    toc: true
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(tidyr)
```

## źródła

źródło wykresu: <https://x.com/pio_tru/status/1849130099527881019>
data publikacji: 23.10.2024

## Wykres przed zmianą

![](images\wykres.jpg)

## Komentarz przed zmianą

Zamieszczony wyżej wykres posiada kilka rzucających się w oczy błędów, które łatwo będzie można poprawić.
Po pierwsze, kolumny ukazane na wykresie, które reprezentują podział czasu antenowego w TVP, są ustawione w odwrotnej
kolejności względem czasu (oś zaczyna się od 3 kwartału roku 2024 a kończy na 1 kwartale roku 2023). Może to prowadzić
do błędnego zrozumienia przez odbiorcę zmian w tych podziałach czasu antenowego na przestrzeni lat. Ponadto na wykresie 
można dostrzec braki w danych dla kwartału 2 roku 2024 oraz 4 roku 2023. Wykład oczywiście byłby także bardziej sprawiedliwy
gdyby wprowadzić podział na poszczególne partie zamiast na koalicję rządzącą i opozycje, niemniej jednak zostańmy przy oryginalnym podziale.

```{r zadanie 1, message=FALSE}
data <- data.frame(
  Kwartał = c("Q1 2023", "Q2 2023", "Q3 2023", "Q4 2023", "Q1 2024", "Q2 2024", "Q3 2024"),
  Koalicja_rządząca = c(77, 80, 79, 65, 85, 83, 86),
  Opozycja = c(23, 20, 21, 35, 15, 17, 14)
)

data$Kwartał <- factor(data$Kwartał, levels = c("Q1 2023", "Q2 2023", "Q3 2023", 
                                                "Q4 2023", "Q1 2024", "Q2 2024", "Q3 2024"))

data_long <- data %>%
  pivot_longer(cols = c(Koalicja_rządząca, Opozycja), 
               names_to = "Grupa", values_to = "Czas")

ggplot(data_long, aes(x = Kwartał, y = Czas, fill = Grupa)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(Czas, "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("Koalicja_rządząca" = "steelblue", "Opozycja" = "orange")) +
  labs(
    title = "Podział czasu antenowego w TVP kwartałami",
    subtitle = "[Źródło: raporty prof. Kowalskiego z KRRiT]",
    x = "Kwartał",
    y = "Czas antenowy (%)",
    fill = "Grupa"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

```

## Wykres po zmianie

![](images/wykres2.jpg)

## Komentarz po zmianie

Kolumny z poprzedniego wykresu zostały ułożone w prawidłowej kolejności, a brakujące kolumny zostały uzupełniony. Dzięki tym
zmianom wykres stał się bardziej zrozumiały i czytelny dla odbiorcy.