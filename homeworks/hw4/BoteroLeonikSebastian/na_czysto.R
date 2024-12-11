library(tidyr)
library(dplyr)
library(eurostat)
library(sf)
library(ggplot2)

opuszczone <- read.csv2("DANE/Noworodki opuszczone przez rodziców/Noworodki pozostawione w szpitalu 2007-2023.csv")
ogolnie <- read.csv2("DANE/Noworodki opuszczone przez rodziców/Urodzenia żywe w Polsce 2007-2023.csv")

colnames(opuszczone) <- c("Województwo", 2007:2023)
colnames(ogolnie)[-1] <- 2007:2023

ogolnie[[17,1]] <- "Polska"

opuszczone_dl <- pivot_longer(opuszczone, -1, names_to = "rok", values_to = "opuszczone")
ogolnie_dl <- pivot_longer(ogolnie, -1, names_to = "rok", values_to = "ogolnie")

op_razem <- opuszczone |>
  mutate(
    `2007-2009` = `2007` + `2008` + `2009`,
    `2014-2016` = `2014` + `2015` + `2016`,
    `2021-2023` = `2021` + `2022` + `2023`
    ) |>
  select(Województwo, `2007-2009`, `2014-2016`, `2021-2023`) |>
  pivot_longer(-Województwo, names_to = "lata", values_to = "opuszczone")

og_razem <- ogolnie |>
  mutate(
    `2007-2009` = `2007` + `2008` + `2009`,
    `2014-2016` = `2014` + `2015` + `2016`,
    `2021-2023` = `2021` + `2022` + `2023`
    ) |>
  select(Województwo, `2007-2009`, `2014-2016`, `2021-2023`) |>
  pivot_longer(-Województwo, names_to = "lata", values_to = "ogolnie") 

wsp_razem <- op_razem |>
  inner_join(og_razem) |>
  mutate(wspolczynnik = opuszczone/ogolnie * 1000)

sf <- get_eurostat_geospatial(resolution = "01",
                        nuts_level = 2,
                        year = 2016,
                        country = "Poland")

sf$NAME_LATN[sf$NAME_LATN == "Mazowiecki regionalny"] <- "Mazowieckie"

sf$geometry[sf$NAME_LATN == "Mazowieckie"] <-  st_union(
  sf[sf$NAME_LATN %in% c("Mazowieckie", "Warszawski stołeczny"),]
)

dane <- wsp_razem |>
  left_join(
    sf |>
  mutate(NAME_LATN = tolower(NAME_LATN)),
    by = join_by(Województwo == NAME_LATN)
  ) |>
  filter(Województwo != "Polska")

# library(extrafont)
# ttf_import("./Fonty i kolory Fundacji Gajusz/Fonty czyli czcionki/",
#                      recursive = TRUE)
# 
# loadfonts()
# showtext.auto()
# library(sysfonts)
  
# font_paths("./Fonty i kolory Fundacji Gajusz/Fonty czyli czcionki/Raleway/static/")
# font_add("Raleway",
#          regular = "./Fonty i kolory Fundacji Gajusz/Fonty czyli czcionki/Raleway/static/Raleway-Regular.ttf",
#          bold = "./Fonty i kolory Fundacji Gajusz/Fonty czyli czcionki/Raleway/static/Raleway-Bold.ttf",
#          bolditalic = "./Fonty i kolory Fundacji Gajusz/Fonty czyli czcionki/Raleway/static/Raleway-BoldItalic.ttf",
#          italic = "./Fonty i kolory Fundacji Gajusz/Fonty czyli czcionki/Raleway/static/Raleway-Italic.ttf" )
# 
# font_add("Tenor Sans",
#          regular = "./Fonty i kolory Fundacji Gajusz/Fonty czyli czcionki/Tenor_Sans/TenorSans-Regular.ttf")

# font_add("Raleway",
#          regular = "Raleway-Regular.ttf",
#          bold = "Raleway-Bold.ttf",
#          bolditalic = "Raleway-BoldItalic.ttf",
#          italic = "Raleway-Italic.ttf" )

# font_add_google("Raleway")

mapa <- dane |>
  ggplot(aes(fill = wspolczynnik, geometry = geometry)) +
  geom_sf(colour = "black", linewidth = 0.5, show.legend = FALSE) +
  facet_wrap(~lata) +
  # scale_fill_distiller(
  #   direction = 1,
  #   palette = "YlOrRd"
  # )
  scale_fill_gradient2(
    # low =  "#f7bfd7",
    low = "white",
    high ="#e62248"
    # high =  "#8c2a64"
    # midpoint = 2.5
  ) +
  theme_void() +
  geom_sf_label(aes(label = formatC(wspolczynnik, format="f", big.mark=",", digits=1),
                   ),
                fill = scales::muted("#e62248", l = 100, c = 40),
                colour =  "#8c2a64",
                nudge_x = 
                  c(
                    0, 0, 0, 0, 0, 0, -0.4, 0, 0, 0.2, 0, 0, 0, 0, -0.1, 0
                  ),
                nudge_y = 
                  c(
                    0, 0, 0, 0, 0, 0, 0, 0, 0, -0.2, 0, 0, 0, 0, -0.1, 0
                  ),
               size = 5,
               show.legend = FALSE
  ) +
  labs(
    title = "Współczynnik dzieci opuszczonych przez rodziców",
    subtitle = "(opuszczone/1000 narodzeń)"
  ) +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    strip.text = element_text(size = 14, face = "italic"),
    plot.title.position = "panel"
    
    # ,text = element_text(family = "Raleway")
  )

ggsave(
  "mapa.png",
  mapa,
  height = 2160,
  width = 3840,
  units = "px",
  bg = "transparent"
)
