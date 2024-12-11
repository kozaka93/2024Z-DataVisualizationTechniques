opuszczone <- read.csv2("DANE/Noworodki opuszczone przez rodziców/Noworodki pozostawione w szpitalu 2007-2023.csv")
ogolnie <- read.csv2("DANE/Noworodki opuszczone przez rodziców/Urodzenia żywe w Polsce 2007-2023.csv")

colnames(opuszczone) <- c("Województwo", 2007:2023)
colnames(ogolnie)[-1] <- 2007:2023

ogolnie[[17,1]] <- "Polska"

library(tidyr)
library(dplyr)

opuszczone_dl <- pivot_longer(opuszczone, -1, names_to = "rok", values_to = "opuszczone")
ogolnie_dl <- pivot_longer(ogolnie, -1, names_to = "rok", values_to = "ogolnie")
wspolczynniki_dl <- opuszczone_dl |>
  inner_join(
    ogolnie_dl
  ) |>
  mutate(wspolczynnik = 1000*opuszczone/ogolnie)

wspolczynniki <- wspolczynniki_dl |>
  pivot_wider(id_cols = 1, names_from = "rok", values_from = wspolczynnik) |>
  inner_join(
    wspolczynniki_dl |>
      group_by(Województwo) |>
      summarise(srednia = mean(wspolczynnik))
  )
  # pivot_wider(id_cols = 1, names_from = "rok", values_from = wspolczynnik)

library(ggplot2)

wspolczynniki |>
  ggplot(aes(x = Województwo, y = srednia)) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 90)
  )

poland <- map_data("world", "poland")

poland |>
  ggplot(aes(x = long, y = lat, group = group)) +
  coord_fixed(100) +
  geom_polygon(aes(order), colour = "black")
  
library(eurostat)
sf <- get_eurostat_geospatial(resolution = "01",
                        nuts_level = 2,
                        year = 2016,
                        country = "Poland")

sf$NAME_LATN[sf$NAME_LATN == "Mazowiecki regionalny"] <- "Mazowieckie"
library(sf)

sf$geometry[sf$NAME_LATN == "Mazowieckie"] <-  st_union(
  sf[sf$NAME_LATN %in% c("Mazowieckie", "Warszawski stołeczny"),]
)


sf |>
  mutate(NAME_LATN = tolower(NAME_LATN)) |>
  right_join(wspolczynniki, by = join_by(NAME_LATN == Województwo)) |>
  # View()
  ggplot(aes(fill = srednia)) +
  geom_sf() +
  scale_fill_distiller(
    direction = 1,
    palette = "Reds"
  )




# Nie widoczne jest działanie programu 500+
wspolczynniki_dl |>
  # filter(rok > 2016) |>
  # mutate(delta = c(diff(wspolczynnik), 0)) |>
  ggplot(aes(x = rok, y = wspolczynnik)) +
  geom_col() +
  facet_wrap(~Województwo) +
  theme(
    axis.text.x = element_text(angle = 90)
  )
  # geom_vline(xintercept = 9.5)

x <- wspolczynniki_dl |>
  filter(Województwo %in% c("zachodniopomorskie", "podkarpackie")) |>
  group_by(Województwo) |>
  summarise(mean = mean(wspolczynnik))

x<-x[[2]]

x[2]/x[1]

wspolczynniki_dl |>
  filter(Województwo %in% c("zachodniopomorskie", "podkarpackie")) |>
  pivot_wider(id_cols = rok,names_from = Województwo, values_from = wspolczynnik) |>
  mutate(ratio = zachodniopomorskie/podkarpackie) |>
  View()

ogolnie |>
  pivot_longer(cols = -Województwo, names_to = "rok", values_to = "ogólnie") |>
  filter(Województwo %in% c("zachodniopomorskie", "podkarpackie")) |>
  group_by(Województwo) |>
  summarise(sum = sum(ogólnie)) |>
  inner_join(
    opuszczone |>
      pivot_longer(cols = -Województwo, names_to = "rok", values_to = "opuszczone") |>
      filter(Województwo %in% c("zachodniopomorskie", "podkarpackie")) |>
      group_by(Województwo) |>
      summarise(sum = sum(opuszczone))
    , by = join_by(Województwo == Województwo)
  ) |>
  mutate(wsp = sum.x/sum.y) -> x

x$wsp[1]/x$wsp[2]


opuszczone |>
  pivot_longer(cols = -Województwo, names_to = "rok", values_to = "opuszczone") |>
  filter(Województwo %in% c("zachodniopomorskie", "podkarpackie")) |>
  group_by(Województwo) |>
  summarise(sum = sum(opuszczone))


opuszczone_dl |>
  group_by(Województwo) |>
  summarise(sOp = sum(opuszczone)) |>
  inner_join(
    ogolnie_dl |>
      group_by(Województwo) |>
      summarise(sOg = sum(ogolnie))
  ) |>
  mutate(wspolczynnik = sOp/sOg*1000)

opuszczone_dl |>
  group_by(Województwo, rok)

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

wsp_razem |>
  left_join(
    sf |>
  mutate(NAME_LATN = tolower(NAME_LATN)),
    by = join_by(Województwo == NAME_LATN)
  ) |>
  View()

dane <- sf |>
  mutate(NAME_LATN = tolower(NAME_LATN)) |>
  right_join(
    opuszczone_dl |>
      group_by(Województwo) |>
      summarise(sOp = sum(opuszczone)) |>
      inner_join(
        ogolnie_dl |>
          group_by(Województwo) |>
          summarise(sOg = sum(ogolnie))|>
          filter(Województwo != "Polska")
      ) |>
      mutate(wspolczynnik = sOp/sOg*1000)
    , by = join_by(NAME_LATN == Województwo))


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

dane <- wsp_razem |>
  left_join(
    sf |>
  mutate(NAME_LATN = tolower(NAME_LATN)),
    by = join_by(Województwo == NAME_LATN)
  ) |>
  filter(Województwo != "Polska")

dane |>
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
    
    # plot.margin = unit(c(0.1, 0, 0, 0), units = "npc")
    # text = element_text(family = "Raleway")
  )

  # geom_sf_text(aes(label = round(wspolczynnik, 1),
  #                  colour = wspolczynnik
  #                  ),
  #                  # colour =  "#b5e0f3",
  #              # colour = "white",
  #              nudge_x = c(
  #                0, -0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.4, 0
  #              ),
  #              size = 7,
  #              show.legend = FALSE
  # ) +
  # scale_colour_gradient2(
  #   low =  "#bfd7f7",
  #   mid = "#a6b5f5",
  #   high =  "#2a648c",
  #   midpoint = 2.5,
  #   transform = "reverse"
  # )
  
  #e62248
  # scale_colour_gradient2(
  #   low =  "#f7bfd7",
  #   mid ="white" ,
  #   high =  "#8c2a64",
  #   midpoint = 2,
  #   transform = "reverse"
  # )
  
# #e4007e
#   scale_fill_stepsn(
#     colours = c(
# "#FFD4E2",
# "#FF9BA9",
# "#ED6373",
# "#B2273D",
# "#8c2a64"
# # "#7B0000"
#     )
#   )
# "#8c2a64"
# 
# # "#fbe0da",
# # "#f9c0bb",
# # "#f39b98",
# # "#ee7677",
# # "#e9535c"
