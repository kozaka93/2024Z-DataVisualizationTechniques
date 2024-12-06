library(leaflet)
library(geojsonio)
library(dplyr)

options(scipen = 12)

poland_data <- geojsonio::geojson_read("https://raw.githubusercontent.com/andilabs/polska-wojewodztwa-geojson/refs/heads/master/polska-wojewodztwa.geojson", what = "sp")

noworodki <- read.csv("Noworodki.csv",
                      sep =";", header = TRUE) %>% 
  tidyr::pivot_longer(cols = 2:18, values_to = "number", names_to = "year") %>% 
  group_by(Województwo) %>% summarise(pozostawione = sum(number))


zywe <- read.csv("Żywe.csv",
                 sep =";", header = TRUE) %>% 
  tidyr::pivot_longer(cols = 2:18, values_to = "number", names_to = "year") %>% 
  group_by(Województwo) %>% summarise(zywe_ur = sum(number))

data <- left_join(zywe, noworodki) %>%
  mutate(jeden_na_ile = round(zywe_ur / pozostawione)) %>% 
  rename(name = Województwo)
data <- data[-1,]

data$name <- c("Dolnośląskie", "Kujawsko-Pomorskie", "Lubelskie", "Lubuskie", 
               "Mazowieckie", "Małopolskie", "Opolskie", "Podkarpackie",
               "Podlaskie","Pomorskie","Warmińsko-Mazurskie","Wielkopolskie",
               "Zachodniopomorskie","Łódzkie","Śląskie","Świętokrzyskie")

poland_data@data$name <- c("Łódzkie", "Świętokrzyskie", "Wielkopolskie", 
                           "Kujawsko-Pomorskie", "Małopolskie", "Dolnośląskie",
                           "Lubelskie", "Lubuskie", "Mazowieckie", "Opolskie",
                           "Podlaskie", "Pomorskie", "Śląskie", "Podkarpackie",
                           "Warmińsko-Mazurskie", "Zachodniopomorskie")


poland_data@data <- left_join(poland_data@data, data, by = "name")


colors <- c("#b5e0f3", "#315ca8", "#303174", "#884292", "#8c2a64", "#e62248")


pal <- colorBin(colors, domain = poland_data@data$pozostawione, bins = 4)


labels <- sprintf(
  "<strong>%s</strong><br/>Liczba opuszczonych noworodków: <strong>%s</strong><br/>
    Liczba żywych urodzeń: %s<br/>Czyli <i>1 noworodek na %s </i> został opuszczony <br> przez rodziców w tym województwie",
  poland_data@data$name, 
  format(poland_data@data$pozostawione, big.mark = " ", scientific = FALSE),
  format(poland_data@data$zywe_ur, big.mark = " ", scientific = FALSE),
  format(poland_data@data$jeden_na_ile, big.mark = " ", scientific = FALSE)
) %>% lapply(htmltools::HTML)


leaflet(poland_data) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(pozostawione),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, 
    values = ~pozostawione, 
    opacity = 0.7, 
    title = paste("Liczba noworodków <br/> opuszczonych w latach 2007-2023"),
    position = "bottomright"
  )