library(plotly)
library(dplyr)
library(readxl)
library(stringi)
library(tidyr)
library(hrbrthemes)
library(htmlwidgets)

births <- read_xlsx('Urodzenia_zywe_w_Polsce_2007-2023.xlsx', 1)
infant_leaved <- read_xlsx('Noworodki_pozostawione_w_szpitalu_2007-2023.xlsx', 1)

births_processed <- births |>
  pivot_longer(c('2007.0', '2008.0', '2009.0', '2010.0', '2011.0', '2012.0', '2013.0', '2014.0', '2015.0', '2016.0', '2017.0',
                 '2018.0', '2019.0','2020.0','2021.0', '2022.0', '2023.0'), names_to = 'year', values_to = 'nr_births') |>
  mutate(year = stri_sub(year,1,4), Województwo = ifelse(Województwo == 'POLSKA', 'Polska', Województwo))

infant_leaved_processed <- infant_leaved |>
  slice(-c(1:7, 25)) |>
  rename('2007' = '...2', '2008' = '...3', '2009' = '...4', '2010' = '...5', '2011' = '...6', '2012' = '...7', '2013' = '...8',
         '2014' = '...9', '2015' = '...10', '2016' = '...11', '2017' = '...12', '2018' = '...13', '2019' = '...14', '2020' = '...15',
         '2021' = '...16', '2022' = '...17', '2023' = '...18', 
         "Województwo" = "Noworodki pozostawione w szpitalu nie ze względów zdrowotnych w latach 2007-2023") |>
  pivot_longer(c('2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019',
                 '2020','2021', '2022', '2023'), names_to = 'year', values_to = 'nr_leaved')


  
data <- inner_join(births_processed, infant_leaved_processed, by = c('Województwo', 'year')) |>
  filter(Województwo != 'Polska') |>
  mutate(nr_leaved = as.numeric(nr_leaved), nr_births = as.numeric(nr_births), ratio = nr_leaved/nr_births *1000,
         makroregion = case_when(Województwo %in% c('małopolskie', 'śląskie') ~ 'południowy',
                                 Województwo %in% c('wielkopolskie', 'zachodniopomorskie', 'lubuskie') ~ 'północno-zachodni',
                                 Województwo %in% c('dolnośląskie', 'opolskie') ~ 'południowo-zachodni',
                                 Województwo %in% c('kujawsko-pomorskie', 'warmińsko-mazurskie', 'pomorskie') ~'północny',
                                 Województwo %in% c('łódzkie', 'świętokrzyskie') ~ 'centralny',
                                 Województwo %in% c('lubelskie', 'podkarpackie', 'podlaskie') ~ 'wschodni',
                                 Województwo %in% c('mazowieckie') ~ 'województwo mazowieckie'))

p <- data |>
  mutate(text = paste("Makroregion: ", makroregion, "\nWojewództwo: ", Województwo,
                      "\nLiczba pozostawionych noworodków\nna 1000 urodzeń: ", round(ratio,1),
                      "\nLiczba żywych urodzeń: ", nr_births, "\nLiczba pozostawionych noworodków: ", nr_leaved,sep="")) |>
 
ggplot(aes(x= nr_leaved, y=nr_births, size = ratio, fill = makroregion, text=text)) +
  geom_point(alpha=0.7, shape = 21, color = 'black', aes(frame = year)) +
  scale_size(range = c(3, 15), name="") +
  scale_fill_manual(
    values = 
      c(
    '#7fc97f',
    '#beaed4',
    '#FF7F00',
    '#ffff99',
    '#386cb0',
    '#f0027f',
    '#E31A1C'
      # '#e41a1c',
      # '#377eb8',
      # '#4daf4a',
      # '#984ea3',
      # '#ff7f00',
      # '#ffff33',
      # '#a65628'
      )) +
  # scale_x_sqrt() +
  # scale_y_log10() +
  theme_ipsum()
  #coord_flip()

final <- ggplotly(p, tooltip="text") |> 
  layout(title = list(
    text = paste0('Zależność liczby żywych urodzeń od liczby pozostawionych noworodków w latach 2007-2023',
                  '<br>',
                  '<sup>',
                  'Wielkość kółek odpowiada liczbie pozostawionych noworodków na 1000 urodzeń','</sup>')),
         #titlefont = list(size = 15),
         # annotations = list(
         #   list(x = 1.4 , y = 0.0, 
         #   text = "Wielkość kółek odpowiada\nliczbie pozostawionych\nnoworodków na
         #        1000 urodzeń               ",
         #        showarrow = F, xref='paper', yref='paper', automargin = F)),
         font=list(size = 15),
    xaxis = list(title = "Liczba pozostawionych noworodków", titlefont = list(size = 20),
                 tickfont = list(size = 15)),
    yaxis = list(title = "Liczba żywych urodzeń", titlefont = list(size = 20), 
                 tickfont = list(size = 15)) ) |>
  animation_opts(2000, easing = "elastic", redraw = FALSE) |>
  animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="red")))

final

saveWidget(final, file = "chart.html")




