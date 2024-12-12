opuszczone<-read.csv("C:\\Users\\User\\Desktop\\Programowanie\\Techniki_Wizualizacji_Danych\\hw4\\KacprzakKarol\\porzucone_noworodki.csv",sep=";")
żywe<-read.csv2("C:\\Users\\User\\Desktop\\Programowanie\\Techniki_Wizualizacji_Danych\\hw4\\KacprzakKarol\\Urodzenia żywe w Polsce 2007-2023.csv")

library(ggplot2)
library(dplyr)
library(sf)
library(tidyr)
library(stringi)


geojson_file <- "gadm41_POL_1.json"

polska <- st_read(geojson_file) %>% 
  mutate(Województwo=tolower(NAME_1))

opuszczone_prom <- opuszczone[,-1] / żywe[,-1]*1000
opuszczone_prom[,ncol(opuszczone_prom)+1]=data.frame(opuszczone$województwo)
opuszczone_prom<-opuszczone_prom %>%
  mutate(Województwo=opuszczone.województwo) %>% 
  select(-opuszczone.województwo)

   
  
opuszczone_prom<-opuszczone_prom  %>% 
  pivot_longer(
    cols = -Województwo,        
    names_to = "rok",           
    values_to = "wartosc"       
) %>% 
  mutate(rok= as.numeric(stri_replace_first_regex(rok, "^X", "")))


polska<-polska %>% 
  inner_join(opuszczone_prom)




library(plotly)

# Tworzenie animacji
fig <- plot_ly(data = polska) %>%
  add_sf(
    stroke = I("black"),
    showlegend = FALSE,
    colors = c("#b5e0f3", "#8c2a64","#e62248"),
    split = ~NAME_1,         
    color = ~wartosc,       
    frame = ~rok           
  ) %>%
  layout(
    title = "Udział dzieci pozostawionych przez rodziców po narodzinach
     w całkowitej liczbie narodznych w latach 2007-2023",
    colorbar = list(title = "Wartość [‰]"),
    geo = list(
      fitbounds = "locations",
      visible = FALSE
    )
  )

fig





