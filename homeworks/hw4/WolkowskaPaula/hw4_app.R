#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# Data preparation
urodzenia <- read.csv("C:/Users/glajt/OneDrive - TU Eindhoven/Documents/Semestr_3/TWD/homeworks/hw4/urodzenia.csv")
noworodki <- read.csv("C:/Users/glajt/OneDrive - TU Eindhoven/Documents/Semestr_3/TWD/homeworks/hw4/noworodki_pozostawione.csv")

urodzenia_df <- urodzenia %>% 
  pivot_longer(cols = -Województwo, names_to = "Year", values_to = "Births") %>% 
  mutate(Year=substring(Year, 2,5),
         Year=as.numeric(Year),
         Births=as.numeric(Births))

noworodki_df <- noworodki %>% 
  pivot_longer(cols = -Województwo, names_to = "Year", values_to = "LeftInHospital") %>% 
  mutate(Year=substring(Year, 2,5),
         Year=as.numeric(Year),
         LeftInHospital=as.numeric(LeftInHospital))

combined_df <- left_join(urodzenia_df, noworodki_df, by = c("Województwo", "Year")) %>%
  mutate(PercentageLeft = (LeftInHospital / Births) * 100)


ui <- fluidPage(
  titlePanel("Noworodki pozostawione w szpitalach"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", 
                  "Wybierz Województwo:", 
                  choices = setdiff(unique(combined_df$Województwo),"POLSKA"),
                  selected = unique(combined_df$Województwo)[1],
                  width="110%"
                  ),
      br(),
      p("Wybierz województwo, aby zobaczyć dane dotyczące procentu noworodków pozostawionych w szpitalach.")
      
    ),
    mainPanel(
      plotOutput("trendPlot", height="500px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$trendPlot <- renderPlot({
      
      filtered_data <- combined_df %>% filter(Województwo==input$region)
      country_data <- combined_df %>% filter(Województwo=="POLSKA")
        
      max_p = max(max(country_data$PercentageLeft),max(filtered_data$PercentageLeft))
          
      ggplot()+
        geom_line(data=country_data, aes(x=Year, y=PercentageLeft), size=1, color="#315ca8", linetype="dashed")+
        geom_line(data=filtered_data, aes(x=Year, y=PercentageLeft), size=1, color="#ea4f7f")+
        geom_point(data=filtered_data, aes(x=Year, y=PercentageLeft), size=3, color="#ea4f7f")+
        scale_x_continuous(breaks = seq(2007,2023,1))+
        scale_y_continuous(expand=c(0,0), limits=c(0,1.5*max_p)) +
        labs(
          title = paste0("Procent noworodków pozostawionych w szpitalach w ", input$region, " (2007-2023)"),
          subtitle = "Dane dla całego kraju pokazano szarą linią przerywaną",
          x = "Rok",
          y = "Procent pozostawionych noworodków"
        )+
        theme_minimal()+
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        )+
        geom_vline(xintercept = 2016, color = "#303174", linetype = "dashed", size = 1)+
        geom_label(data=country_data, label="Start 500+", x=2016, y=max_p*1.35, fill = "#303174", color="white", size=4)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
