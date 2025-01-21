appsUI <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "container-fluid",
    
    tags$style(HTML("
      body {
        background-color: #F2F7F7; 
      }
      .control-label {
        font-weight: bold;
      }
    ")),
    
    box(
      title = "",
      solidHeader = TRUE,       
      status = "warning",              
      width = 12,                       
      style = "background-color: #158cba	; border: 2px solid #158cba; border-radius: 10px; padding: 10px ;color: white;", 
      fluidRow(column(
        3,
        selectInput(
          ns("appChoice"),
          "Choose an application",
          choices = unique(apps_df$application),
          selected = "Duolingo"
        )
      ),
      column(
        3,
        dateRangeInput(
          ns("dateRange"),
          label = "Choose date range",
          start = min(apps_df$date),
          end = max(apps_df$date),
          min = min(apps_df$date),
          max = max(apps_df$date),
          format = "yyyy-mm-dd",
          separator = " to "
        )
      ))),
    fluidRow(style = "height: 40px;"),
    fluidRow(column(3,
                    box(
                      title = tags$span(
                        "How often is the app used?", 
                        style = "background-color: #ff7f0e; color: white; padding: 10px; border-radius: 10px; width: 100%; display: inline-block; box-sizing: border-box;"
                      ),
                      solidHeader = TRUE, 
                      width = 12,
                      style = "background-color: #f9f9f9; border: 2px solid #ff7f0e; border-radius: 10px; padding: 10px;",  # Pomarańczowa obramówka
                      tags$div(
                        style = "font-size: 16px; color: black;",
                        uiOutput(ns("text1"))
                      )
                    )),
             column(9,
                    box(
                      title = "", 
                      solidHeader = TRUE,                     
                      status = "primary", 
                      width = 12,
                      style = "background-color: #f9f9f9; border: 2px solid #ff7f0e; border-radius: 10px; padding: 0px; display: inline-block; vertical-align: top;", 
                      tags$div(
                        style = "display: flex; justify-content: center; align-items: center; height: 100%;",
                        plotlyOutput(ns("appsPlotLine"), height = "100%", width = "1100px")%>%
                          shinycssloaders::withSpinner(color = "#158cba", type=6)
                      )
                    )
             )),
    fluidRow(style = "height: 80px;"),
    fluidRow(column(3,
                    box(
                      title = tags$span(
                        "How much time using the apps?", 
                        style = "background-color: #dc143c; color: white; padding: 10px; border-radius: 10px; width: 100%; display: inline-block; box-sizing: border-box;"
                      ),
                      solidHeader = TRUE, 
                      width = 12,
                      style = "background-color: #f9f9f9; border: 2px solid #dc143c; border-radius: 10px; padding: 10px;",
                      tags$div(
                        style = "font-size: 16px; color: black;",
                        uiOutput(ns("text2"))
                      )
                    )),
             column(9,
                    box(
                      title = "", 
                      solidHeader = TRUE,                     
                      status = "primary", 
                      width = 12,
                      style = "background-color: #f9f9f9; border: 2px solid #dc143c; border-radius: 10px; padding: 0px; display: inline-block; vertical-align: top;", 
                      tags$div(
                        style = "display: flex; justify-content: center; align-items: center; height: 100%;",
                        plotlyOutput(ns("appsBarPlot"), height = "100%", width = "1100px")
                      )
                    ))),
    
    fluidRow(style = "height: 80px;")
    
  )
}

appsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$text1<-renderText("This chart shows how much time each of us spent on selected application each day. It is possible to select a date range. Thanks to this, we are able to compare data over a chosen period, e.g. during the Christmas break.")
    
    output$text2<-renderText("Here we have a summary of the entire data collection period. The bar chart shows which user spent the most time and which the least time using a specific application.")
    
    output$appsPlotLine <- renderPlotly({
      plot_data <- apps_df %>%
        filter(application == input$appChoice,
               date >= input$dateRange[1],
               date <= input$dateRange[2]) %>%
        mutate(
          tooltip_text = paste("Date:", date, "<br>Time spent:", time_spent, "minutes")
        )
      plot<-ggplot(plot_data,aes(x = date, y = time_spent, color = user, group = user,text = tooltip_text)) +
        geom_line() +
        geom_point() +
        labs(
          x = "Date",
          y = "Time [min]",
          color = "Users"
        ) +
        scale_color_manual(values = c(
          "Milosz" = '#158cba', 
          "Kasia" = "#ff7f0e",
          "Zuzia" = "#dc143c"  
        )) +
        theme_minimal()
      
      ggplotly(plot,tooltip = "text") %>%
        layout(paper_bgcolor = "rgba(0, 0, 0, 0)",
               plot_bgcolor = "rgba(0, 0, 0, 0)") %>%
        config(displayModeBar = FALSE)
    })
    
    output$appsBarPlot<-renderPlotly({
      filtered_data <- apps_df %>%
        filter(date>=input$dateRange[1] & date<=input$dateRange[2]) %>%
        group_by(application,user) %>%
        summarise(total_minutes = sum(time_spent, na.rm=TRUE),.groups = 'drop')
      
      user_colors = c("Milosz" = '#158cba', 
                      "Kasia" = "#ff7f0e",
                      "Zuzia" = "#dc143c" )
      plot_ly(filtered_data,
              y = ~application,
              x = ~total_minutes,
              color = ~user,
              colors = user_colors,
              type = 'bar',
              hoverinfo = 'text',
              text = ~paste(user, ':', total_minutes, 'min'),
              textposition = 'none') %>%
        layout(
          barmode = 'stack',
          yaxis = list(title = 'Applications'),
          xaxis = list(title = 'Time [min]'),
          legend = list(title = list(text = "Users")),
          paper_bgcolor = "rgba(0, 0, 0, 0)",
          plot_bgcolor = "rgba(0, 0, 0, 0)"
        ) %>% config(displayModeBar=FALSE)
    })
  })
}