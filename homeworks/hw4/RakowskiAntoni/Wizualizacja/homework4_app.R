# set to directory containing data.csv
# setwd("path to directory")

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

data <- read.csv("data.csv")

choice_labels <- list(
  "Ilość porzuconych dzieci w szpitalu na 100k urodzonych" = "y1",
  "Ilość porzuconych dzieci w szpitalu" = "y2",
  "Ilość urodzonych żywych dzieci" = "y3"
)

ui <- fluidPage(

  titlePanel("Analiza urodzeń i noworodków pozostwionych w szpitalu (z przyczyn niezdrowotnych) dla fundacji Gajusz"),

  sidebarLayout(

    sidebarPanel(

      sliderInput("year", "Wybierz lata", min = 2007,
                  max = 2023, value = c(2007, 2023), sep = ""),

      checkboxInput("ispoland",
                    label = "Pokaż dla całej Polski", value = FALSE),

      conditionalPanel(
        condition = "input.ispoland == false",
        selectInput("region", "Wybierz region(y)",
                    choices = data[, 1], multiple = TRUE,
                    selected = c("Mazowieckie", "Małopolskie"))
      ),

      conditionalPanel(
        condition = "input.tabs == 'Wykres punktowy'",
        checkboxGroupInput("scatterOptions", "Wybierz opcje",
                           choices = c("Zaznacz wprowadzenie 500+",
                                       "Skala logarytmiczna", "Krzywa trendu"))
      ),

      conditionalPanel(
        condition = "input.tabs == 'Wykres słupkowy'",
        checkboxGroupInput("barOptions", "Wybierz opcje",
                           choices = c("Zaznacz wprowadzenie 500+",
                                       "Skala logarytmiczna"))
      ),

      conditionalPanel(
        condition = "input.tabs != 'Dane'",
        radioButtons("plotValues", "Wybierz wartośći na osi OY",
                     choices = choice_labels)
      )
    ),

    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Wykres punktowy", plotOutput("scatterPlot")),
        tabPanel("Wykres słupkowy", plotOutput("barPlot")),
        tabPanel("Dane", dataTableOutput("dataTable")),

        conditionalPanel(
          condition = "input.tabs == 'Dane'",
          textOutput("source")
        )
      )
    )
  )
)

server <- function(input, output) {

  region <- reactive({
    if (input$ispoland == TRUE) {
      c("Polska")
    } else {
      input$region
    }
  })

  years <- reactive(input$year)

  data_to_plot <- reactive({
    data %>%
      filter(voivodeship %in% region()) %>%
      filter(year >= min(years()), year <= max(years()))
  })

  values <- reactive({
    if (input$plotValues == "y1") {
      data_to_plot()$abandonned_per_100k
    } else if (input$plotValues == "y2") {
      data_to_plot()$abandonned
    } else {
      data_to_plot()$births
    }
  })

  output$scatterPlot <- renderPlot({
    scatter_plot <- ggplot(data_to_plot(), aes(x = year, y = values(),
                                               color = voivodeship)) +
      geom_point() +
      labs(x = "Rok",
           y = names(choice_labels)[which(choice_labels == input$plotValues)],
           color = "Region") +
      scale_x_continuous(breaks = min(years()):max(years())) +
      theme_minimal()

    if ("Krzywa trendu" %in% input$scatterOptions & years()[2] - years()[1] + 1 > 2) { #nolint
      scatter_plot <- scatter_plot +
        geom_smooth(method = "gam", aes(group = voivodeship, #nolint
                                        fill = voivodeship),
                    show.legend = FALSE,
                    formula = y ~ s(x, k = years()[2] - years()[1] + 1))
    }

    if ("Krzywa trendu" %in% input$scatterOptions & years()[2] - years()[1] + 1 <= 2) { #nolint
      scatter_plot <- scatter_plot +
        geom_smooth(method = "lm", aes(group = voivodeship))
    }

    if ("Skala logarytmiczna" %in% input$scatterOptions) {
      scatter_plot <- scatter_plot +
        scale_y_log10()
    }

    if ("Zaznacz wprowadzenie 500+" %in% input$scatterOptions & max(years()) >= 2016) { #nolint
      scatter_plot <- scatter_plot +
        geom_vline(xintercept = 2016, linetype = "dashed", color = "black")
    }

    scatter_plot
  })

  output$barPlot <- renderPlot({
    bar_plot <- ggplot(data_to_plot(), aes(x = factor(year), y = values(),
                                           fill = voivodeship)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Rok",
           y = names(choice_labels)[which(choice_labels == input$plotValues)],
           fill = "Region") +
      theme_minimal()

    if ("Zaznacz wprowadzenie 500+" %in% input$barOptions) {
      bar_plot <- bar_plot +
        geom_vline(xintercept = "2016", linetype = "dashed", color = "black")
    }

    if ("Skala logarytmiczna" %in% input$barOptions) {
      bar_plot <- bar_plot +
        scale_y_log10()
    }

    bar_plot
  })

  output$dataTable <- DT::renderDT(data_to_plot(),
                                   options = list(
                                     pageLength = 5,
                                     lengthMenu = c(5, 10)
                                   ))

  output$source <- renderText("źródłó: https://drive.google.com/drive/folders/159LnpRjeSvcrSkAGN7DPxrI_3j7ouNiJ") #nolint
}

shinyApp(ui = ui, server = server)
