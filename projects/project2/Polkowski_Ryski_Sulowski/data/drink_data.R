#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)
drink_data <- read_excel("data/TWD 2.xlsx", sheet = "Drinks",
                      col_types = c("text", "numeric", "text", 
                                      "text", "date", "skip"))


# View(drink_data)
drink_categories <- read_excel("data/TWD 2.xlsx", sheet = "Drinks",
                      col_types = c("skip", "skip", "skip",
                                      "skip", "skip", "text"), n_max = 6)
# View(drink_categories)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Drink Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("people",
                        "Select of which people should data be displayed for:",
                        c(
                          "Person 1" = "Adam",
                          "Person 2" = "Czarek",
                          "Person 3" = "Michał"
                        ),
                        selected = c("Adam","Czarek","Michał")),
            dateRangeInput("daterange", "Date range:",
                           start  = min(drink_data$Date),
                           end    = max(drink_data$Date),
                           min    = min(drink_data$Date),
                           max    = max(drink_data$Date),
                           format = "yy/mm/dd",
                           separator = " - "),
            checkboxGroupInput("categories",
                               "Select which categories of drinks should data be displayed for:",
                               choiceNames = drink_categories$`Categories:`[!is.na(drink_categories$`Categories:`)],
                               choiceValues = drink_categories$`Categories:`[!is.na(drink_categories$`Categories:`)],
            )
            
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("value"),
          plotOutput("drinks_Barplot"),
          plotOutput("water_Barplot")
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$value <- renderText({ input$daterange  })

  output$drinks_Barplot <- renderPlot(
     drink_data %>% 
      filter(as.Date(Date) >= input$daterange[1], as.Date(Date) <= input$daterange[2]) %>% 
      filter(Name %in% input$people) %>%
      ggplot(aes(x = factor(Date), y = Amount, fill = Name)) + geom_col() + 
      scale_y_continuous(expand = expansion(mult = c(0, 0))) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      ) 
  )
  
  output$water_Barplot <- renderPlot(
    drink_data %>% filter(Category %in% input$categories) %>% 
      ggplot(aes(x = Name, y = Amount, fill = Name)) + geom_col() +
      scale_y_continuous(expand = expansion(mult = c(0, 0))) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      )
  )
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
