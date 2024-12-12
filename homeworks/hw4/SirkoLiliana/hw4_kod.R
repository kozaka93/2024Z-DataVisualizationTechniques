library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(gridExtra)

##dane
df_urodzenia <- read.csv('/Users/lila/Desktop/hw 4 twd/urodzenia.csv')
df_pozostawione <- read.csv('/Users/lila/Desktop/hw 4 twd/pozostawione_noworodki.csv')

df_pozostawione <- df_pozostawione %>% head(17)

long_pozostawione <- subset(df_pozostawione, select = -Polska) %>% pivot_longer(cols = dolnoslaskie:zachodniopomorskie, 
                                                                                names_to = "region",
                                                                                values_to = "pozostawione_noworodki")

long_urodzenia <- df_urodzenia %>% pivot_longer(cols = dolnoslaskie:zachodniopomorskie,
                                                names_to = "region",
                                                values_to = "urodzenia")

df_long <- long_pozostawione %>% inner_join(long_urodzenia, by=c('rok', 'region'))


#kolory fundacji:
custom_gradient <- colorRampPalette(c("#303174", "#315ca8", "#884292", "#8c2a64", "#e62248", "#e4007e", "#ea4f7f"))(16)


data <- df_long


#Shiny:

ui <- fluidPage(
  titlePanel("Ilość pozostawionych noworodków i żywych urodzeń na przestrzeni lat, z podziałem na województwa"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_regions", "Wybierz wojewódz", choices = unique(df_long$region))
    ),
    mainPanel(
      plotOutput("highlightPlot", height = "700px", width = "900px")
    )
  )
)


server <- function(input, output) {
  output$highlightPlot <- renderPlot({
    data$region_id <- as.numeric(as.factor(data$region))
    
    #Wykres: pozostawione
    plot_abandoned <- ggplot(data, aes(x = rok, y = pozostawione_noworodki, group = region)) +
      geom_line(aes(color = region_id,
                    size = ifelse(region %in% input$selected_regions, 1.5, 0.5),
                    alpha = ifelse(region %in% input$selected_regions, 1, 0.3))) +
      scale_color_gradientn(colors = custom_gradient) +
      scale_size_continuous(range = c(0.5, 1.5)) +
      scale_x_continuous(breaks = seq(2007, 2023, by = 1)) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Pozostawione noworodki", x = "Rok", y = "") +
      theme(axis.title.x = element_text(hjust = 0.35))
    
    # Wykres: urodzenia
    plot_live_births <- ggplot(data, aes(x = rok, y = urodzenia, group = region)) +
      geom_line(aes(color = region_id,
                    size = ifelse(region %in% input$selected_regions, 1.5, 0.5),
                    alpha = ifelse(region %in% input$selected_regions, 1, 0.3))) +
      scale_color_gradientn(colors = custom_gradient) +
      scale_size_continuous(range = c(0.5, 1.5)) +
      scale_x_continuous(breaks = seq(2007, 2023, by = 1)) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Urodzenia", x = "Rok", y = "")+
      theme(axis.title.x = element_text(hjust = 0.35))
    
    grid.arrange(plot_abandoned, plot_live_births, nrow = 2, heights = c(1, 1))
  })
}

shinyApp(ui = ui, server = server)