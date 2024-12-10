# Jak sie prezentuje  problem (pozostawienia noworodkow w Szitalu)
# posczegolnych wojewodztwach w poszczegolnych latach

# Opis wykorzystanego aspektu danych

# Wykorzystano dane o urodzeniach żywych w Polsce na przestrzeni lat oraz dane
# o noworodkach pozostawionych w szpitalu nie z powodów zdrowotnych
# (dane z podzialem na lata oraz wojewodztwa), 
# została pokazana skala problemu porzucenia noworodków w poszczególnych 
# województwach. Wykorzystano podział danych na województwa, co pozwoliło 
# na dokładną analizę regionalną.

setwd("/Users/admin/TWD")
# install.packages("ggplot2")
# install.packages("sf")           
# install.packages("rnaturalearthdata")
library(tools)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(shiny)
library(tidyr)

df_pozostawione <- read.csv("noworodkipozostawione.csv")
df_zywe<- read.csv("urodzeniazywe.csv")

df_pozostawione$Region <- toTitleCase(tolower(df_pozostawione$Region))
df_zywe$Województwo <- toTitleCase(tolower(df_zywe$Województwo))

long_df1 <- df_zywe %>%
  pivot_longer(
    cols = !Województwo,         
    names_to = "year",          
    values_to = "amount"         
  )
long_df2 <- df_pozostawione %>%
  pivot_longer(
    cols = !Region,         
    names_to = "year",          
    values_to = "amount"         
  )


long_df1$year <- substring(long_df1$year, 2)
long_df2$year <- substring(long_df2$year, 2)

df_merged <- long_df1 |>
  inner_join(long_df2, by = c("Województwo" = "Region", "year")) |>
  arrange(year) |>
  mutate(leftBy1K = (amount.y / amount.x) * 10000)

## shiny app
map <- geodata::gadm("Poland", level = 1, path = tempdir())
map_df <- as.data.frame(map)
df_map <- st_as_sf(map)

ui <- fluidPage(
  titlePanel("Regionalna Analiza Liczby Pozostawinych Noworodkow"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_year",
        label = "Wybierz rok:",
        choices = unique(df_merged$year),
        selected = 2023
      )
    ),
    mainPanel(
      plotOutput("data_plot"),
      tableOutput("data_table")
    )
  )
)


server <- function(input, output) {
  
  df_map1_temp <- reactive({
    df_merged %>% filter(year == input$selected_year) |>
      select(c("Województwo", "leftBy1K"))
  })
  
  
  
  df_map1 <- reactive({
    df_map %>%
      left_join(df_map1_temp(), by = c("NAME_1" = "Województwo"))
  }) 
  
  output$data_plot <- renderPlot({
    ggplot(df_map1()) +
      geom_sf(aes(fill = leftBy1K)) +
      scale_fill_gradientn(colors = c("#b5e0f3", "#315ca8", "#303174"), limits = c(0, max(df_merged$leftBy1K)),  name="") +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
      ) +
      labs(
        title = paste("Liczba pozostawionych noworodków w spitalu w ", input$selected_year, " roku"),
        subtitle = "na 10tys. żywych urodzeń"
      )
    
    
  })
  
}

shinyApp(ui = ui, server = server)

####
# Opis wizualizacji

# Mozna zauwazyc, ze jest tendencja do wiekszego procentu pozostawionych noworodkow
# w wojewodztwach polozonych na zachod, niz w tych na wschodzie Polski
