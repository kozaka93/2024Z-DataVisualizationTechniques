#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

sen <- card(
  full_screen = TRUE,
  card_header("My plot"),
  plotOutput("p")
)

kroki <- card(
  full_screen = TRUE,
  card_header("My plot"),
  plotOutput("p")
)

nawodnienie <- card(
  full_screen = TRUE,
  card_header("My plot"),
  plotOutput("p")
)

czasNaDworze <- card(
  full_screen = TRUE,
  card_header("My plot"),
  plotOutput("p")
)

czasPrzedEkranem <- card(
  full_screen = TRUE,
  card_header("My plot"),
  plotOutput("p")
)



ui <- navbarPage(
  "Styl życia studenta MiNi",
  tabPanel("Sen 🛏️",sen),
  tabPanel("Kroki 👣️", kroki),
  tabPanel("Nawodnienie 🥛", nawodnienie),
  tabPanel("Czas na dworze ⛰️", czasNaDworze),
  tabPanel("Czas przed ekranem 🖥️", czasPrzedEkranem)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
