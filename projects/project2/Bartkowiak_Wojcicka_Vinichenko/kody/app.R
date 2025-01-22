library(dplyr)
library(tidyr)
library(readxl)
library(plotly)
library(shiny)
library(ggplot2)
library(grid)
library(jpeg)
library(shinycssloaders)

options(shiny.launch.browser = function(url) {
  browseURL(url)
})


Daria <- read_excel("/Users/daria1942/Desktop/Wydatki_TWD.xlsx", sheet = "Daria")
Oleksii <- read_excel("/Users/daria1942/Desktop/Wydatki_TWD.xlsx", sheet = "Oleksii")
Oliwia <- read_excel("/Users/daria1942/Desktop/Wydatki_TWD.xlsx", sheet = "Oliwia")
Daria <- Daria %>%
  mutate(Expenses = rowSums(select(., -c(Data, `Dzień tygodnia`, Wpływy, Kieszonkowe, Praca, `Zwrot środków`, Stypendium)), na.rm = TRUE))

Oleksii <- Oleksii %>%
  mutate(Expenses = rowSums(select(., -c(Data, `Dzień tygodnia`, Wpływy, Kieszonkowe, Praca, `Zwrot środków`, Stypendium)), na.rm = TRUE))

Oliwia <- Oliwia %>%
  mutate(Expenses = rowSums(select(., -c(Data, `Dzień tygodnia`, Wpływy, Kieszonkowe, Praca, `Zwrot środków`, Stypendium)), na.rm = TRUE))

# Lista osób i odpowiadających im danych
data_list <- list(
  "Daria" = Daria,
  "Oliwia" = Oliwia,
  "Oleksii" = Oleksii
)

person_colors <- c("Daria" = "#C3A2F5", "Oliwia" = "#F4B5F5", "Oleksii" = "#BBF5CB")

{
Daria__ <- read_excel("/Users/daria1942/Desktop/Wydatki_TWD.xlsx", sheet = "Daria")
Oleksii__ <- read_excel("/Users/daria1942/Desktop/Wydatki_TWD.xlsx", sheet = "Oleksii")
Oliwia__ <- read_excel("/Users/daria1942/Desktop/Wydatki_TWD.xlsx", sheet = "Oliwia")

Daria__[, c(-1, -2)] <- lapply(Daria__[, c(-1, -2)], as.numeric)
Daria__[is.na(Daria__)] <- 0
Daria_sum <- data.frame(colSums(Daria__[, c(-1, -2)]))
colnames(Daria_sum) <- "sum"
Daria_sum$category <- rownames(Daria_sum)

Oleksii__[, c(-1, -2)] <- lapply(Oleksii__[, c(-1, -2)], as.numeric)
Oleksii__[is.na(Oleksii__)] <- 0
Oleksii_sum <- data.frame(colSums(Oleksii__[, c(-1, -2)]))
colnames(Oleksii_sum) <- "sum"
Oleksii_sum$category <- rownames(Oleksii_sum)

Oliwia__[, c(-1, -2)] <- lapply(Oliwia__[, c(-1, -2)], as.numeric)
Oliwia__[is.na(Oliwia__)] <- 0
Oliwia_sum <- data.frame(colSums(Oliwia__[, c(-1, -2)]))
colnames(Oliwia_sum) <- "sum"
Oliwia_sum$category <- rownames(Oliwia_sum)
}
# Define UI
ui <- navbarPage(
  title = div(
    style = "font-family: 'Arial'; font-size: 24px; text-align: center; font-weight: bold;",
    "WYDATKI STUDENTÓW MINI"
  ),
  
  theme = shinythemes::shinytheme("flatly"),
  
  # Dodanie niestandardowego stylu CSS do zmiany koloru tła
  tags$style(HTML("
    body {
      background-color: #A9CCF5; /* Zmieniony kolor tła */
    }
  ")),
  
  tabPanel("Strona Główna",  # Pierwsza zakładka
          fluidPage(
            fluidRow(
            column(12,
              tags$div(
                style = "position: absolute; top: 150px; left: 50%; transform: translateX(-50%);", 
                
                imageOutput("description_image")
             ),
             fluidRow(
               column(4, 
                      tags$div(
                        style = "position: absolute; top: -5px; left: 410px;",
                        imageOutput("Daria_awatar")
                      )),
               column(4, 
                      tags$div(
                        style = "position: absolute; top: -5px; left: 140px;",
                        imageOutput("Oleksii_awatar")
                      )),
               column(4, 
                      tags$div(
                        style = "position: absolute; top: -5px; left: -130px;",
                        imageOutput("Oliwia_awatar")
                      ))
             )
            )
           )
          )
  ),
  
  tabPanel("Data",  # Druga zakładka
           fluidPage(
             fluidRow(
               column(3,
                      wellPanel(
                        checkboxGroupInput("persons", "Wybierz osoby:", 
                                           choices = names(data_list), 
                                           selected = "Daria"),
                        sliderInput(
                          "date_range", 
                          "Wybierz zakres dat:", 
                          min = as.Date(min(sapply(data_list, function(x) min(as.Date(x$Data))))),
                          max = as.Date(max(sapply(data_list, function(x) max(as.Date(x$Data))))),
                          value = c(
                            as.Date(min(sapply(data_list, function(x) min(as.Date(x$Data))))),
                            as.Date(max(sapply(data_list, function(x) max(as.Date(x$Data)))))
                          ),
                          timeFormat = "%d-%m-%Y",
                          step = 1,
                          animate = TRUE
                        )
                      )
               ),
               column(9,
                      withSpinner(uiOutput("plots"))
               )
             ),
             fluidRow(
               column(3,
                      wellPanel(
                        selectInput("bar_person", "Wybierz osobę:", 
                                    choices = names(data_list))
                      )
               ),
               column(9,
                      withSpinner(plotOutput("barPlot"))
               )
             ),
             fluidRow(
               column(3,
                      wellPanel(
                        checkboxGroupInput("bar_person_2", "Wybierz osoby:", 
                                           choices = names(data_list), 
                                           selected = names(data_list)),
                        selectInput("day", "Wybierz dzień tygodnia:", 
                                    choices = c("poniedziałek", "wtorek", "środa", "czwartek", 
                                                "piątek", "sobota", "niedziela"))
                      )
               ),
               column(9,
                      withSpinner(plotOutput("barPlot2"))
               )
             )
             
           )
  ),
  
  tabPanel("Kategorie",  # Trzecia zakładka
           fluidRow(
             column(3,
                    selectInput(
                      inputId = "osoba",
                      label = "Wybierz osobę",
                      choices = list("Daria" = "Daria", "Oleksii" = "Oleksii", "Oliwia" = "Oliwia"),
                      selected = "Daria"
                    )
             )
           ),
           fluidRow(column(12,
                           withSpinner(plotlyOutput(outputId = "sankey", height = "400px"))
           )),
           fluidRow(
             style = "margin-top: 80px;",
             column(3, 
                    selectInput(
                      inputId = "sum_df",
                      label = "Wybierz osobę",
                      choices = list("Daria" = "Daria", "Oleksii" = "Oleksii", "Oliwia" = "Oliwia"),
                      selected = "Daria"
                    )),
             column(9, 
                    withSpinner(plotlyOutput(outputId = "sum_bar_plot")))
           )
  ),
  
  tabPanel("Wpływy",  # Czwarta zakładka
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("personWplywy", "Wybierz osobę:", choices = c("Daria", "Oleksii", "Oliwia"))
               ),
               mainPanel(
                 shinycssloaders::withSpinner(plotOutput("Plot")),    # Pierwszy wykres - słupkowy
                 withSpinner(plotlyOutput("piePlot")) # Drugi wykres - kołowy
               )
             )
           )
  )
)

# Define server logic
server <- function(input, output, session) {
  session$onSessionEnded(function(){
    stopApp()
  })
  selected_data <- reactive({
    switch(input$personWplywy,  # Używamy personWplywy, aby odwołać się do kontrolki w zakładce "Wpływy"
           "Daria" = Daria,
           "Oleksii" = Oleksii,
           "Oliwia" = Oliwia)
  })
  
  # Wykres słupkowy
  output$Plot <- renderPlot({
    data <- selected_data()
    data$Data <- as.Date(data$Data)

    # Dodaj kolumnę z miesiącem i rokiem
    data$Month <- format(data$Data, "%m")
    data$Month <- case_when(
      data$Month == "01" ~ "styczeń",
      data$Month == "12" ~ "grudzień"
    )

    # Zdefiniuj kolumny dla wydatków i wpływów
    expenditure_columns <- c(
      "Jedzenie na mieście", "Zakupy spożywcze", "Przekąski", "Alkohol",
      "Ciuchy", "Kosmetyki", "Prezenty", "Paliwo", "Komunikacja miejska", "Rozrywka", "Inne"
    )
    income_columns <- c("Wpływy")

    data <- data %>%
      mutate(
        TotalExpenditures = rowSums(select(., all_of(expenditure_columns)), na.rm = TRUE),
        TotalIncomes = rowSums(select(., all_of(income_columns)), na.rm = TRUE)
      )

    # Oblicz miesięczne sumy wydatków i wpływów
    monthly_data <- data %>%
      group_by(Month) %>%
      summarise(
        TotalExpenditures = sum(TotalExpenditures, na.rm = TRUE),
        TotalIncomes = sum(TotalIncomes, na.rm = TRUE)
      ) %>%
      mutate(Difference = TotalIncomes - TotalExpenditures)

    person_colors <- list(
      "Daria" = c("TRUE" = "#C3A2F5", "FALSE" = "#A267F4"),
      "Oleksii" = c("TRUE" = "#BBF5CB", "FALSE" = "#5CDB7C"),
      "Oliwia" = c("TRUE" = "#F4B5F5", "FALSE" = "#F56AD1")
    )

    # Pobierz odpowiednie kolory na podstawie wyboru
    selected_colors <- person_colors[[input$personWplywy]]

    # Tworzenie wykresu
    ggplot(monthly_data, aes(x = Month, y = Difference, fill = Difference >= 0)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_manual(values = selected_colors, guide = "none") +  # Ustawienie kolorów
      geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
      labs(
        title = "Różnica między wpływami a wydatkami",
        x = "Miesiąc",
        y = "Zł"
      ) +
      theme_minimal() +
      theme(
        # Wyśrodkowanie i pogrubienie tytułu
        plot.title = element_text(size = 20, hjust = 0.5),

        # Pogrubienie i zwiększenie podpisów osi
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),

        # Rotacja i dostosowanie tekstu osi X oraz zwiększenie rozmiaru
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Zmienione 'size'

        # Kolor tła
        panel.background = element_rect(fill = "#A9CCF5", color = NA),
        plot.background = element_rect(fill = "#A9CCF5", color = NA)
      )
  })
  
  # Wykres kołowy
  output$piePlot <- renderPlotly({
    data <- selected_data()
    data$Data <- as.Date(data$Data)
    data$Month <- format(data$Data, "%m")
    
    wplywy <- data[, c("Kieszonkowe", "Praca", "Zwrot środków", "Stypendium")]
    suma_kategorii <- wplywy %>%
      summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))
    
    kategorie <- colnames(suma_kategorii)
    kwoty <- as.numeric(suma_kategorii[1, ])
    
    kategorie <- kategorie[kwoty > 0]
    kwoty <- kwoty[kwoty > 0]
    
    if(length(kategorie) == 0 || length(kwoty) == 0) {
      return(NULL)  # Jeśli nie ma danych, nie rysuj wykresu
    }
    
    kwoty_labels <- paste(kwoty, "zł")
    
    kolory <- case_when(
      input$personWplywy == "Oleksii" ~ c("#23803D", "#BBF5CB", "#8EF4A3"),
      input$personWplywy == "Daria" ~ c("#C3A2F5", "#A267F4", "#752DBD"),
      input$personWplywy == "Oliwia" ~ c("#F4B5F5", "#BD38BB", "#F56AD1")
    )
    
    plot_ly(
      labels = kategorie,
      values = kwoty,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value",
      text = kwoty_labels,
      showlegend = FALSE,
      marker = list(colors = kolory[1:length(kategorie)])
    ) %>%
      layout(
        plot_bgcolor = '#A9CCF5',
        paper_bgcolor = '#A9CCF5',
        title = list(text = 'Rozkład wszystkich wpływów', font = list(size = 20, family = "Arial", weight = "bold")),
        margin = list(t = 50, b = 20, l = 40, r = 20)
      )
  })
  
  output$plots <- renderUI({
    plot_outputs <- lapply(input$persons, function(person) {
      plotname <- paste0("plot_", person)
      plotOutput(plotname)
    })
    withSpinner(do.call(tagList, plot_outputs))
  })
  
  observe({
    lapply(input$persons, function(person) {
      output[[paste0("plot_", person)]] <- renderPlot({
        data <- data_list[[person]] %>%
          mutate(Data = as.Date(Data)) %>%
          filter(Data >= input$date_range[1] & Data <= input$date_range[2]) %>%
          mutate(Holiday = Data >= as.Date("2024-12-21") & Data <= as.Date("2025-01-02"))
        
        ggplot(data) +
          geom_line(aes(x = Data, y = Expenses, group = 1, color = Holiday), size = 1) +
          scale_color_manual(
            values = c(`TRUE` = "#32cd32", `FALSE` = "black"),
            labels = c(`TRUE` = "Okres świąt", `FALSE` = "Uczelnia")
          ) +
          labs(
            title = paste("Wykres wydatków -", person),
            x = "Data",
            y = "zł",
            color = " "
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank(),
            panel.background = element_rect(fill = "#A9CCF5", color = NA),
            plot.background = element_rect(fill = "#A9CCF5", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black")
          ) +
          scale_x_date(
            breaks = unique(as.Date(data$Data)),
            labels = format(unique(as.Date(data$Data)), "%d-%m-%Y")
          ) +
          scale_y_continuous(labels = scales::label_number(accuracy = 1))
      })
    })
  })
  
  # Pierwszy wykres słupkowy
  output$barPlot <- renderPlot({
    data <- data_list[[input$bar_person]]
    
    data_summary <- data %>%
      group_by(`Dzień tygodnia`) %>%
      summarize(AvgExpenses = mean(Expenses, na.rm = TRUE)) %>%
      mutate(`Dzień tygodnia` = factor(
        `Dzień tygodnia`, 
        levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")
      ))
    
    fill_color <- switch(input$bar_person,
                         "Daria" = "#C3A2F5",
                         "Oliwia" = "#F4B5F5",
                         "Oleksii" = "#BBF5CB")
    
    ggplot(data_summary, aes(x = `Dzień tygodnia`, y = AvgExpenses)) +
      geom_bar(stat = "identity", fill = fill_color, width = 0.7) +
      labs(
        title = paste("Średnie wydatki według dnia tygodnia -", input$bar_person),
        x = NULL,
        y = "zł"
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#A9CCF5", color = NA),  # Niebieskie tło panelu
        plot.background = element_rect(fill = "#A9CCF5", color = NA),   # Niebieskie tło wykresu
        panel.grid.major = element_line(color = "white"),               # Biała siatka główna
        panel.grid.minor = element_line(color = "white"), # Drobna biała siatka
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Drugi wykres słupkowy
  output$barPlot2 <- renderPlot({
    # Pobranie danych dla wybranych osób
    selected_data <- lapply(input$bar_person_2, function(person) {
      data <- data_list[[person]] %>%
        mutate(Person = person)  # Dodaj kolumnę z nazwą osoby
    }) %>% bind_rows()
    
    # Wykluczenie dat od 21 grudnia do 2 stycznia
    selected_data <- selected_data %>%
      mutate(Data = as.Date(Data)) %>%
      filter(!(Data >= as.Date("2024-12-21") & Data <= as.Date("2025-01-02")))
    
    # Przygotowanie danych do wykresu
    category_data <- selected_data %>%
      select(-c(Data, Wpływy, Expenses, Kieszonkowe, Praca, `Zwrot środków`, Stypendium)) %>%       # Usuń zbędne kolumny
      pivot_longer(cols = -c(`Dzień tygodnia`, Person),  # Transformacja danych na długi format
                   names_to = "Category", 
                   values_to = "Amount") %>%
      group_by(Person, `Dzień tygodnia`, Category) %>%  # Grupowanie po osobie, dniu tygodnia i kategorii
      summarize(
        TotalAmount = sum(Amount, na.rm = TRUE),   # Suma wydatków dla każdej kategorii i dnia
        DayCount = n(),                           # Liczba wystąpień dnia tygodnia
        .groups = "drop"
      ) %>%
      mutate(AvgAmount = TotalAmount / DayCount) %>%  # Średnia = suma / liczba dni
      filter(`Dzień tygodnia` == input$day) %>%
      mutate(Category = factor(Category, levels = colnames(select(selected_data, -c(Data, `Dzień tygodnia`, Wpływy,Kieszonkowe, Praca, `Zwrot środków`, Stypendium, Expenses, Person)))))  # Zachowanie kolejności kategorii z danych
    
    # Przypisanie kolorów do osób
    person_colors <- c("Daria" = "#C3A2F5", "Oliwia" = "#F4B5F5", "Oleksii" = "#BBF5CB")
    
    # Tworzenie wykresu
    ggplot(category_data, aes(x = Category, y = AvgAmount, fill = Person)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      scale_fill_manual(values = person_colors) +
      labs(
        title = paste("Średnie wydatki według kategorii"),
        x =  " ",
        y = "zł",
        fill = " "
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        panel.background = element_rect(fill = "#A9CCF5", color = NA),  # Niebieskie tło panelu
        plot.background = element_rect(fill = "#A9CCF5", color = NA),   # Niebieskie tło wykresu
        panel.grid.major = element_line(color = "white"),               # Biała siatka główna
        panel.grid.major.y = element_line(color = "white") # Drobna biała siatka
      ) +
      scale_y_continuous(labels = scales::label_number(accuracy = 1))
  })
  
  selected_df <- reactive({
    switch(input$osoba,
           "Daria" = Daria_sum,
           "Oleksii" = Oleksii_sum,
           "Oliwia" = Oliwia_sum)
  })
  
  output$sankey <- renderPlotly({
    
    df <- selected_df()
    
    nodes <- data.frame(name = c(rownames(df), "Oszczędności", "Wydano", "Inne wydatki", "Jedzenie", "Pozostało"))
    
    links <- data.frame(source = c(12:15, 11, 11, 16, 17, 17, rep(18, 7), rep(19, 4)), 
                        target = c(rep(11, 4), 20, 17, 17, 18, 19, 4:10, 0:3), 
                        value = c(df[c(13, 14, 15, 16), 1], 
                                  df[12, 1] - sum(df[1:11, 1]), min(df[12, 1], sum(df[1:11, 1])), sum(df[1:11, 1]) - df[12, 1],
                                  sum(df[5:11, 1]), sum(df[1:4, 1]),
                                  df[5:11, 1], df[1:4, 1]))
    
    fig <- plot_ly(
      type = "sankey",
      domain = list(
        x =  c(0,1),
        y =  c(0,1)
      ),
      orientation = "h",
      valueformat = ".0f",
      valuesuffix = "zł",
      
      node = list(
        label = nodes$name,
        pad = 15,
        thickness = 15,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = links$source,
        target = links$target,
        value =  links$value
      )
    ) 
    fig <- fig %>% layout(
      title = "",
      font = list(
        size = 12
      ),
      xaxis = list(showgrid = F, zeroline = F),
      yaxis = list(showgrid = F, zeroline = F),
      plot_bgcolor='rgba(0,0,0,0)',
      paper_bgcolor='rgba(0,0,0,0)'
    )
    
    fig
  })
  
  selected_sum_df <- reactive({
    switch(input$sum_df,
           "Daria" = Daria_sum,
           "Oleksii" = Oleksii_sum,
           "Oliwia" = Oliwia_sum)
  })
  
  output$sum_bar_plot <- renderPlotly({
    data <- selected_sum_df()
    
    plot_ly(data = data[1:11, ], 
            x = ~category, 
            y = ~sum, 
            type = 'bar', 
            marker = list(color = person_colors[[input$sum_df]], line = list(color = 'black', width = 1))) %>%
      layout(
        title = paste("Rozkład wydatków według kategorii - ", input$sum_df),
        xaxis = list(title = '', tickangle = 45, tickfont = list(size = 14)),
        yaxis = list(title = 'zł', range = c(0, 1500), titlefont = list(size = 14)),
        barmode = 'group',
        plot_bgcolor = 'rgba(255, 255, 255, 0)', 
        paper_bgcolor = 'rgba(255, 255, 255, 0)', 
        showlegend = FALSE
      )
  })
  
  output$description_image <- renderImage({
    list(src = "/Users/daria1942/Desktop/Bartkowiak_Wojcicka_Vinichenko/images/description.jpg",
         width = 640,
         height = 360)
  }, deleteFile = FALSE)
  
  output$Daria_awatar <- renderImage({
    list(src = "/Users/daria1942/Desktop/Bartkowiak_Wojcicka_Vinichenko/images/daria_awatar.png",
         width = 200,
         height = 200)
  }, deleteFile = FALSE)
  output$Oliwia_awatar <- renderImage({
    list(src = "/Users/daria1942/Desktop/Bartkowiak_Wojcicka_Vinichenko/images/oliwia_awatar.png",
         width = 200,
         height = 200)
  }, deleteFile = FALSE)
  output$Oleksii_awatar <- renderImage({
    list(src = "/Users/daria1942/Desktop/Bartkowiak_Wojcicka_Vinichenko/images/oleksii_awatar.png",
         width = 200,
         height = 200)
  }, deleteFile = FALSE)
  
}
# Run the application
shinyApp(ui = ui, server = server)



