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
library(scales)
steps_data <- read_excel("TWD 2.xlsx", sheet = "Steps",
                         col_types = c("text", "numeric", "date"))




# View(steps_data)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Steps Data"),
  
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
                     start  = min(steps_data$Day),
                     end    = max(steps_data$Day),
                     min    = min(steps_data$Day),
                     max    = max(steps_data$Day),
                     format = "yy/mm/dd",
                     separator = " - ")
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("steps_Lineplot"),
      plotOutput("steps_Barplot"),
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$steps_Lineplot <- renderPlot(
    steps_data %>% 
      filter(as.Date(Day) >= input$daterange[1], as.Date(Day) <= input$daterange[2]) %>% 
      filter(Name %in% input$people) %>%
      ggplot(aes(x = Day, y = Steps, color = Name)) + geom_line(size = 1.5) + 
      scale_y_continuous(expand = expansion(mult = c(0, 0))) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        
      ) +
      labs(title = "Amount of steps taken each day",
           x = "Date",
           y = "Amount of steps taken")
  )
  
  output$steps_Barplot <- renderPlot(
    steps_data %>% 
      filter(as.Date(Day) >= input$daterange[1], as.Date(Day) <= input$daterange[2]) %>% 
      filter(Name %in% input$people) %>%
      group_by(Name) %>% summarise(st = sum(Steps)) %>% 
      ggplot(aes(x = Name, y = st, fill = Name)) + geom_col() + 
      scale_y_continuous(expand = expansion(mult = c(0, 0)), labels = label_comma()) +
      labs(title = paste("Sum of steps taken between ", input$daterange[1]," and ", input$daterange[2]),
           x = "Person",
           y = "Sum of steps taken")
  )
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
