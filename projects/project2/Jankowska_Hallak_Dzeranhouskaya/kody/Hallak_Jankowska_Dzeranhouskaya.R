###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###              PROJEKT 2              ###
###########################################

library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(shinycssloaders)
library(jsonlite)
library(stringr)
library(leaflet)
library(leaflet.extras)
########
#dodatkowe biblioteki dla logo i poczatkowej strony
library(bslib)
library(base64enc)

### basic data
#
df_Elissa <- fromJSON("Mapy_takeoutLJ/Moja_aktywność.json")
df_Liwia <- fromJSON("Mapy_takeout_Liwia/Moja_aktywność.json")
df_Dominika <- fromJSON("Mapy_takeout_Daminika/Moja_aktywność.json")


df_Elissa <- df_Elissa %>%
  mutate(id = row_number()) %>%
  select(id, title, titleUrl, time, locationInfos, description)

df_Liwia <- df_Liwia %>%
  mutate(id = row_number()) %>%
  select(id, title, titleUrl, time, locationInfos, description)

df_Dominika <- df_Dominika %>%
  mutate(id = row_number()) %>%
  select(id, title, titleUrl, time, locationInfos, description)


### data for leaflet with coords

Elissa_coords <- df_Elissa %>%
  # Wyrzucam trasy i wybieram tylko wiersze z linkiem
  filter(is.na(df_Elissa$description) &
           !is.na(df_Elissa$titleUrl)) %>%
  mutate(
    # Wyciągam szerokości i długości
    coords_raw = str_extract(titleUrl, "(?<=@)[^,]+,[^,]+"),
    latitude = as.numeric(str_extract(coords_raw, "^[^,]+")),
    longitude = as.numeric(str_extract(coords_raw, "(?<=,)[^,]+"))
  ) %>%
  select(!c(locationInfos, description))

Elissa_coords <- Elissa_coords %>%
  filter(!is.na(Elissa_coords$latitude) &
           !is.na(Elissa_coords$longitude))

Liwia_coords <- df_Liwia %>%
  # Wyrzucam trasy i wybieram tylko wiersze z linkiem
  filter(is.na(df_Liwia$description) &
           !is.na(df_Liwia$titleUrl)) %>%
  mutate(
    # Wyciągam szerokości i długości
    coords_raw = str_extract(titleUrl, "(?<=@)[^,]+,[^,]+"),
    latitude = as.numeric(str_extract(coords_raw, "^[^,]+")),
    longitude = as.numeric(str_extract(coords_raw, "(?<=,)[^,]+"))
  ) %>%
  select(!c(locationInfos, description))

Liwia_coords <- Liwia_coords %>%
  filter(!is.na(Liwia_coords$latitude) &
           !is.na(Liwia_coords$longitude))

Dominika_coords <- df_Dominika %>%
  # Wyrzucam trasy i wybieram tylko wiersze z linkiem
  filter(is.na(df_Dominika$description) &
           !is.na(df_Dominika$titleUrl)) %>%
  mutate(
    # Wyciągam szerokości i długości
    coords_raw = str_extract(titleUrl, "(?<=@)[^,]+,[^,]+"),
    latitude = as.numeric(str_extract(coords_raw, "^[^,]+")),
    longitude = as.numeric(str_extract(coords_raw, "(?<=,)[^,]+"))
  ) %>%
  select(!c(locationInfos, description))

Dominika_coords <- Dominika_coords %>%
  filter(!is.na(Dominika_coords$latitude) &
           !is.na(Dominika_coords$longitude))


Elissa_selected <- data.frame(
  title = c("x"),
  address = c("y"),
  latitude = c(0.0),
  longitude = c(0.0)
)
Liwia_selected <- data.frame(
  title = c("Dom", "Uczelnia", "Chłopak"),
  address = c("Książkowa 52P", "Koszykowa 75", "Makowa 6 Radzymin"),
  latitude = c(52.3274671, 52.2221473, 52.4162199),
  longitude = c(20.9489848, 21.0070047, 21.1516664)
)
Dominika_selected <- data.frame(
  title = c("Dom", "Sklep", "Uczelnia"),
  address = c("Waryńskiego 12", "Tamka 40", "Koszykowa 75"),
  latitude = c(52.2162757, 52.2367601, 52.2221473),
  longitude = c(21.016259, 21.0219041, 21.0070047)
)

### data for searches
Sys.setlocale("LC_TIME", "C")
Elissa_coords2 <- Elissa_coords %>%
  mutate(
    date = substr(time, 1, 10),
    hour = as.numeric(substr(time, 12, 13)),
    day_of_week = weekdays(as.Date(date))
  )

Liwia_coords2 <- Liwia_coords %>%
  mutate(
    date = substr(time, 1, 10),
    hour = as.numeric(substr(time, 12, 13)),
    day_of_week = weekdays(as.Date(date))
  )
Dominika_coords2 <- Dominika_coords %>%
  mutate(
    date = substr(time, 1, 10),
    hour = as.numeric(substr(time, 12, 13)),
    day_of_week = weekdays(as.Date(date))
  )
user_data <- list("Elissa" = Elissa_coords2,
                  "Liwia" = Liwia_coords2,
                  "Dominika" = Dominika_coords2)



### UI


######
#poczatkowa strona
#obrazek logo
logo_style <- paste0(
  "\n<style>\n",
  "  body {\n",
  "    background-color: #FFFFFF;\n",
  "    font-family: 'Arial', sans-serif;\n",
  "    font-size: 18px;\n",
  "    text-align: center;\n",
  "  }\n",
  "  .google-logo {\n",
  "    margin-top: 15vh;\n",
  "  }\n",
  "  .center-content {\n",
  "    display: flex;\n",
  "    flex-direction: column;\n",
  "    align-items: center;\n",
  "    justify-content: flex-start;\n",
  "    height: 100vh;\n",
  "  }\n",
  "</style>\n"
)

#wczytanie obrazka logo
logo_base64 <- base64enc::dataURI(file = "my_google_activity_logo/google_logo.png", mime = "image/png")

#przycisk "Back" na wszystkich stronach
back_button <- function() {
  actionButton("back_button", "← Back", style = "margin: 10px; position: absolute; top: 10px; left: 10px;")
}
#UI dla poczatkowej strony
ui_home <- fluidPage(
  tags$head(
    # Importowanie czcionki Roboto z Google Fonts
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap", rel = "stylesheet"),
    # Ustawianie Roboto jako domyślnej czcionki w aplikacji
    tags$style(
      HTML(
        "
      body {
        font-family: 'Roboto', sans-serif;
      }
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Roboto', sans-serif;
        font-weight: 500;
      }
      .shiny-text-output, .shiny-input-container {
        font-family: 'Roboto', sans-serif;
      }
      .btn {
        font-family: 'Roboto', sans-serif;
      }
    "
      )
    )
  ),
  HTML(logo_style),
  div(
    class = "center-content",
    img(
      src = logo_base64,
      alt = "Logo",
      class = "google-logo",
      width = "600"
    ),
    div(
      style = "margin-top: 30px;",
      selectInput(
        "selected_page",
        "Search:",
        choices = list(
          "Maps" = "maps",
          "Activity" = "activity",
          "Ranking" = "ranking",
          "About" = "about"
        ),
        selected = "maps"
      )
    ),
    actionButton("go_button", "Start", style = "margin-top: 20px;")
  )
)

ui1 <- fluidPage(
  back_button(),
  #przycisk Back
  tags$head(
    #czcionka jaka jest w google maps
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap", rel = "stylesheet"),
    # ustawiamy Roboto jako domyślną czcionkię w aplikacji
    tags$style(
      HTML(
        "
      body {
        font-family: 'Roboto', sans-serif;
      }
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Roboto', sans-serif;
        font-weight: 500;
      }
      .shiny-text-output, .shiny-input-container {
        font-family: 'Roboto', sans-serif;
      }
      .btn {
        font-family: 'Roboto', sans-serif;
      }
    "
      )
    )
  ),
  titlePanel(HTML(
    "<h1 style='text-align: center;'>Maps</h1>"
  )),
  #titlePanel("Maps"),
  
  h3("Where are we?", style = "text-align: center; margin-top: 50px;"),
  
  sidebarLayout(
    sidebarPanel(
      # Wybór osoby
      selectInput(
        "heatmap_user",
        "Choose user:",
        choices = list(
          "Elissa" = "Elissa",
          "Liwia" = "Liwia",
          "Dominika" = "Dominika"
        ),
        selected = "Elissa"
      ),
      
      # Wybór widoku
      selectInput(
        "heatmap_view",
        "Choose View:",
        choices = list(
          "Warsaw" = "Warszawa",
          "Poland" = "Polska",
          "Europe" = "Europa",
          "World" = "Świat"
        ),
        selected = "Warszawa"
      ),
      h4("Heatmap:", style = "margin-top: 20px;"),
      p(
        "This heatmap visualizes the areas most frequently searched by a given user. 
        Red indicates higher activity, while blue/purple - lower. 
        The map is customizable, allowing users to switch between views such as 
        Warsaw, Poland, Europe, or the World, providing detailed information or a 
        bigger perspective."
      ),
      
      
    ),
    
    
    mainPanel(leafletOutput("heatmap", height = 600), width = 6)
    
    
  ),
  
  # Dodajemy kolejny tytuł i wykres poniżej
  h3("Checkpoints", style = "text-align: center; margin-top: 50px;"),
  
  sidebarLayout(
    sidebarPanel(
      # Wybór osoby
      selectInput(
        "map_user",
        "Choose user:",
        choices = list(
          "Elissa" = "Elissa",
          "Liwia" = "Liwia",
          "Dominika" = "Dominika"
        ),
        selected = "Elissa"
      ),
      
      # Wybór widoku
      selectInput(
        "map_top",
        "Number of top locations:",
        choices = list(
          "3" = "3",
          "10" = "10",
          "20" = "20",
          "50" = "50"
        ),
        selected = "10"
      ),
      h4("Description:", style = "margin-top: 20px;"),
      p(
        "This map highlights the user's most searched destinations. 
        The number of top locations displayed can be adjusted, making it easy 
        to identify which specific places are most significant to us."
      )
      
    ),
    
    mainPanel(leafletOutput("map", height = 600), width = 6)
  ),
  
  tags$footer("Autor: studenci IAD, 2025", align = "center", style = "margin-top: 20px; color: gray;")
)

ui2 <- fluidPage(
  back_button(),
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap", rel = "stylesheet"),
    # ustawiamy Roboto jako domyślną czcionkię w aplikacji
    tags$style(
      HTML(
        "
      body {
        font-family: 'Roboto', sans-serif;
      }
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Roboto', sans-serif;
        font-weight: 500;
      }
      .shiny-text-output, .shiny-input-container {
        font-family: 'Roboto', sans-serif;
      }
      .btn {
        font-family: 'Roboto', sans-serif;
      }
    "
      )
    )
  ),
  titlePanel(HTML(
    "<h1 style='text-align: center;'>Activity</h1>"
  )),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_user",
        "Select a user:",
        choices = names(user_data),
        selected = names(user_data)[1]
      ),
      checkboxGroupInput(
        "selected_days",
        "Select days of the week:",
        choices = c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        ),
        selected = c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        )
      ),
      sliderInput(
        "hour_range",
        "Select hour range:",
        min = 0,
        max = 23,
        value = c(0, 23),
        step = 1
      ),
      p(
        "This heatmap shows when a user is most active on Google Maps. 
        Each square represents a specific hour on a particular day, with darker colors 
        meaning more total searches at that time. Click above to pick a user, 
        choose specific days of the week, and set a time range to see when searches happen most often."
      )
    ),
    mainPanel(
      withSpinner(
        plotOutput("heatmapPlot", width  = "500px", height = "700px"),
        type = 3,
        color = "#007BFF",
        size = 2,
        color.background = "transparent"
      )
    )
  ),
  tags$footer("Autor: studenci IAD, 2025", align = "center", style = "margin-top: 20px; color: gray;")
)

ui3 <- fluidPage(
  back_button(),
  #przycisk Back
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap", rel = "stylesheet"),
    # ustawiamy Roboto jako domyślną czcionkię w aplikacji
    tags$style(
      HTML(
        "
      body {
        font-family: 'Roboto', sans-serif;
      }
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Roboto', sans-serif;
        font-weight: 500;
      }
      .shiny-text-output, .shiny-input-container {
        font-family: 'Roboto', sans-serif;
      }
      .btn {
        font-family: 'Roboto', sans-serif;
      }
    "
      )
    )
  ),
  titlePanel(HTML(
    "<h1 style='text-align: center;'>Ranking</h1>"
  )),
  #przesuniety tytul na srodek
  #titlePanel("Top"),
  
  h3("Places", style = "text-align: center; margin-top: 50px;"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "user",
        "Select user:",
        choices = list(
          "Elissa" = "Elissa",
          "Liwia" = "Liwia",
          "Dominika" = "Dominika"
        ),
        selected = "Elissa"
      ),
      sliderInput(
        "years",
        "Choose the range of years you want to compare:",
        min = 2015,
        max = 2025,
        value = c(2015, 2025),
        step = 1,
        sep = ""
      ),
      p(
        "Here we can see a clear comparison of all of out top searches. The time 
          filter allows us to observe how our top destinations have changed over the years."
      ),
    ),
    
    mainPanel(
      plotOutput("wykres_3"),
      div(
        style = "margin-top: 20px;"
      )
    )
    
  ),
  
  # Dodajemy kolejny tytuł i wykres poniżej
  h3("Areas", style = "text-align: center; margin-top: 50px;"),
  
  sidebarLayout(
    sidebarPanel(
      p(
        "In Google Maps, we can not only search for specific places but also browse 
        through different areas of the map. Here, we can see the most viewed areas, 
        which have turned out to be the most interesting for each user."
      )
    ),
    
    mainPanel(
      plotOutput("wykres_3_2"),
      div(
        style = "margin-top: 20px;",
      )
    )
  ),
  
  tags$footer("Autor: studenci IAD, 2025", align = "center", style = "margin-top: 20px; color: gray;")
  
)

ui4 <- fluidPage(
  
  # Globalne style CSS
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap"),
    tags$style(HTML("
      body {
        margin: 20px; /* Marginesy na całej stronie */
        font-family: 'Montserrat', sans-serif; /* Ustawienie czcionki globalnie */
      }
      h1 {
        text-align: center;
        color: #2E86C1;
        font-size: 32px;
      }
    "))
  ),
  
  back_button(),
  
  # Wyśrodkowany tytuł strony
  titlePanel(HTML("<h1>About Us</h1>")),
  
  # Tekst na górze (wyśrodkowany)
  fluidRow(
    column(12, 
           div(
             style = "text-align: center; padding-top: 20px;",
             p("W naszym projekcie chciałyśmy pokazać, ile informacji o sobie pozostawiamy w internecie. 
               Przeprowadziłyśmy analizę, korzystając z funkcji Google, a dokładniej z Google Takeout. 
               Podczas analizy odkryłyśmy, że informacje pozostawione w aplikacji Google Maps są minimalne. 
               Dla każdej z nas zapisane zostało po kilka wyszukiwań. Postanowiłyśmy więc szukać dalej. 
               Okazało się, że zapisane zostały wpisy do samej wyszukiwarki, do których uzyskałyśmy dostęp poprzez MyActivity. 
               Dzięki tym zapisom, na podstawie samych wyszukiwań w mapach, udało nam się określić najważniejsze dla nas obszary. 
               Miejsca, które wyszukiwałyśmy najczęściej, odzwierciedlają przestrzenie związane z naszym życiem codziennym: 
               mieszkaniem, nauką i odpoczynkiem.")
           )
    )
  ),
  
  # Kontener z 3 kolumnami na środku
  fluidRow(
    column(4, 
           div(
             h3("Elissa"),
             tags$ul(
               tags$li("Mieszka na Woli."),
               tags$li("Studiuje na MiNI."),
               tags$li("Często odwiedza BUW i Bibliotekę Narodową.")
             )
           )
    ),
    column(4, 
           div(
             h3("Liwia"),
             tags$ul(
               tags$li("Mieszka na Białołęce."),
               tags$li("Studiuje na MiNI."),
               tags$li("Często odwiedza Wyszków, gdzie jeździ na działkę.")
             )
           )
    ),
    column(4, 
           div(
             h3("Dominika"),
             tags$ul(
               tags$li("Mieszka w Śródmieściu."),
               tags$li("Studiuje na MiNI."),
               tags$li("Często odwiedza bibliotekę w gmachu głównym PW.")
             )
           )
    )
  )
)

### Server #####################################################################


server <- function(input, output, session) {
  current_page <- reactiveVal("home")
  
  observeEvent(input$go_button, {
    current_page(input$selected_page)
  })
  
  observeEvent(input$back_button, {
    current_page("home")
  })
  
  output$ui <- renderUI({
    switch(
      current_page(),
      home = ui_home,
      maps = ui1,
      activity = ui2,
      ranking = ui3,
      about = ui4
    )
  })
  
  
  # Do wybierania przez menu
  datasets1 <- list(Elissa = Elissa_coords,
                    Liwia = Liwia_coords,
                    Dominika = Dominika_coords)
  datasets2 <- list(
    Warszawa = list(lng = 21.0118, lat = 52.2298, zoom = 9),
    Polska = list(lng = 19.1451, lat = 51.9194, zoom = 6),
    Europa = list(lng = 10, lat = 50, zoom = 4),
    Świat = list(lng = 0, lat = 0, zoom = 2)
  )
  datasets3 <- list(
    "3" = 3,
    "10" = 10,
    "20" = 10,
    "50" = 50
  )
  
  
  output$heatmap <- renderLeaflet({
    # Wybór ramki danych
    selected_data <- datasets1[[input$heatmap_user]]
    
    # Wybór lokalizacji i ustawienia widoku
    view_params <- datasets2[[input$heatmap_view]]
    
    # Mapa
    leaflet(data = selected_data) %>%
      addTiles() %>%
      addHeatmap(
        lng = ~ longitude,
        lat = ~ latitude,
        radius = 15,
        blur = 20,
        max = 0.1
      ) %>%
      setView(lng = view_params$lng,
              lat = view_params$lat,
              zoom = view_params$zoom)
    
  })
  
  
  output$map <- renderLeaflet({
    # Wybór ramki danych
    selected_data <- datasets1[[input$map_user]]
    
    # Wybór liczby top miejsc
    n_top <- datasets3[[input$map_top]]
    
    # Wybór n pierwszych punktów z ramki danych
    selected_data_gr <- selected_data %>%
      # Przycinam do trzech miejsc po przecinku
      mutate(latitude = as.numeric(str_sub(latitude, 1, 6)),
             longitude = as.numeric(str_sub(longitude, 1, 6))) %>%
      group_by(latitude, longitude) %>%
      summarise(n = n()) %>%
      arrange(desc(n))
    
    selected_data_top <- head(selected_data_gr, n = n_top)
    
    # Mapa z markerami
    leaflet(data = selected_data_top) %>%
      addTiles() %>%
      addMarkers(lng = ~ longitude, lat = ~ latitude) %>%
      # Widok na Warszawę
      setView(lng = 21.0118,
              lat = 52.2298,
              zoom = 9)
    
  })
  
  
  #wykres 2 heatmap
  heatmap_data <- reactive({
    user_data[[input$selected_user]] %>%
      filter(
        day_of_week %in% input$selected_days,
        hour >= input$hour_range[1],
        hour <= input$hour_range[2]
      ) %>%
      mutate(day_of_week = factor(
        day_of_week,
        levels = c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        )
      )) %>%
      group_by(day_of_week, hour) %>%
      summarise(count = n(), .groups = "drop")
  })
  
  output$heatmapPlot <- renderPlot({
    ggplot(heatmap_data(), aes(x = day_of_week, y = hour, fill = count)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white",
                          high = "#69b3a2",
                          name = "Frequency") +
      labs(#title = paste("Distribution of Queries by Hour for", input$selected_user),
        x = NULL, y = "Hour") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          size = 14,
          face = "bold"
        ),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.y  = element_text(size = 12, face = "bold"),
        
        # Legenda
        legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 12, face = "bold")
      )
  })
  
  
  #########
  # wykres z czestotliwoscia
  bluegreen = "#69b3a2"
  
  output$wykres_3 <- renderPlot ({
    if (input$user == "Elissa") {
      df <- df_Elissa
    } else if (input$user == "Liwia") {
      df <- df_Liwia
    } else {
      df <- df_Dominika
    }
    df$search <- gsub("Trasa do: ", "", df$title)
    df$search <- gsub("Szukano: ", "", df$search)
    df$search <- sub(",.*", "", df$search)
    df$time <- as.POSIXct(df$time, format = "%Y-%m-%dT%H:%M:%S")
    df$year <- as.numeric(format(df$time, "%Y"))
    
    #licze ile jest obserwacji dla danego hasla/adresu z danego zakresu lat i wybieram top 15
    fav_search <- df %>%
      filter(year >= input$years[1] & year <= input$years[2]) %>%
      filter(!(search == "Odkryte w Mapach Google")) %>%
      filter(!(search == "Używane: Mapy")) %>%
      filter(!(search == "Oglądano obszar w Mapach Google")) %>%
      filter(!(search == "1 powiadomienie")) %>%
      filter(!str_detect(title, "^Viewed area around")) %>%
      filter(!str_detect(title, "^Explored on Google Maps")) %>%
      filter(!str_detect(title, "^Used Maps")) %>%
      filter(!str_detect(title, "^Oglądano obszar wokół")) %>%
      filter(!str_detect(title, "^http")) %>%
      group_by(search) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      head(15)
    fav_search$search <- factor(fav_search$search, levels = fav_search$search)
    y_min <- min(fav_search$count)
    
    ggplot(data = fav_search, aes(x = search, y = as.numeric(count))) +
      geom_col(fill = bluegreen) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      #scale_y_continuous(limits = c(as.numeric(min(fav_search$count)), as.numeric(max(fav_search$count)))) +
      labs(x = NULL, y = "Count")
    
  })
  
  
  #wykres tego ktory obszar ogladamy najczesciej
  gold = "#DAB558"
  gray = "#B4AB96"
  brown = "#C59859"
  
  output$wykres_3_2 <- renderPlot({
    if (input$user == "Elissa") {
      df <- df_Elissa
      df$search <- gsub("Trasa do: ", "", df$title)
      df$search <- gsub("Szukano: ", "", df$search)
      df$search <- sub(",.*", "", df$search)
      df$time <- as.POSIXct(df$time, format = "%Y-%m-%dT%H:%M:%S")
      df$year <- as.numeric(format(df$time, "%Y"))
      
      top_df <- df %>%
        filter(str_detect(title, "^Oglądano obszar wokół")) %>%
        group_by(search) %>%
        summarize(count = n()) %>%
        arrange(count) %>%
        tail(3)
      top_df$search <- gsub("Oglądano obszar wokół: ", "", top_df$search)
      top_df <- top_df %>%
        mutate(rank = rank(-count))
      
      colors <- c(gold, gray, brown)
      
      ggplot(top_df, aes(
        x = reorder(search, -count),
        y = count,
        fill = factor(rank)
      )) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = colors,
                          labels = c("1st place", "2nd place", "3rd place")) +
        labs(title = " ",
             y = "Count",
             fill = "Place") +
        theme_minimal() +
        theme(
          plot.title = element_text(
            hjust = 0.5,
            size = 16,
            face = "bold"
          ),
          axis.text.x = element_text(
            angle = 30,
            hjust = 1,
            size = 12
          ),
          axis.text.y = element_text(size = 12),
          legend.position = "none"
        ) +
        geom_text(aes(label = count), vjust = -0.5, size = 5) +
        labs(x = NULL, y = "Count")
      
    } else if (input$user == "Liwia") {
      df <- df_Liwia
      df$search <- gsub("Trasa do: ", "", df$title)
      df$search <- gsub("Szukano: ", "", df$search)
      df$search <- sub(",.*", "", df$search)
      df$time <- as.POSIXct(df$time, format = "%Y-%m-%dT%H:%M:%S")
      df$year <- as.numeric(format(df$time, "%Y"))
      
      top_df <- df %>%
        filter(str_detect(title, "^Oglądano obszar wokół")) %>%
        group_by(search) %>%
        summarize(count = n()) %>%
        arrange(count) %>%
        tail(3)
      top_df$search <- gsub("Oglądano obszar wokół: ", "", top_df$search)
      top_df <- top_df %>%
        mutate(rank = rank(-count))
      
      colors <- c(gold, gray, brown)
      
      ggplot(top_df, aes(
        x = reorder(search, -count),
        y = count,
        fill = factor(rank)
      )) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = colors,
                          labels = c("1st place", "2nd place", "3rd place")) +
        labs(title = " ",
             y = "Count",
             fill = "Place") +
        theme_minimal() +
        theme(
          plot.title = element_text(
            hjust = 0.5,
            size = 16,
            face = "bold"
          ),
          axis.text.x = element_text(
            angle = 30,
            hjust = 1,
            size = 12
          ),
          axis.text.y = element_text(size = 12),
          legend.position = "none"
        ) +
        geom_text(aes(label = count), vjust = -0.5, size = 5) +
        labs(x = NULL, y = "Count")
      
    } else {
      df <- df_Dominika
      df$search <- gsub("Trasa do: ", "", df$title)
      df$search <- gsub("Szukano: ", "", df$search)
      df$search <- sub(",.*", "", df$search)
      df$time <- as.POSIXct(df$time, format = "%Y-%m-%dT%H:%M:%S")
      df$year <- as.numeric(format(df$time, "%Y"))
      
      top_df <- df %>%
        filter(str_detect(title, "^Viewed area around")) %>%
        group_by(search) %>%
        summarize(count = n()) %>%
        arrange(count) %>%
        tail(3)
      
      top_df$search <- gsub("Viewed area around:", "", top_df$search)
      
      top_df <- top_df %>%
        mutate(rank = rank(-count))
      
      colors <- c(gold, gray, brown)
      
      ggplot(top_df, aes(
        x = reorder(search, -count),
        y = count,
        fill = factor(rank)
      )) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = colors,
                          labels = c("1st place", "2nd place", "3rd place")) +  
        labs(title = " ",
             y = "Count",
             fill = element_blank()) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            hjust = 0.5,
            size = 16,
            face = "bold",
          ),
          axis.text.x = element_text(
            angle = 30,
            hjust = 1,
            size = 12
          ),
          axis.text.y = element_text(size = 12),
          legend.position = "none"
        ) +
        geom_text(aes(label = count), vjust = -0.5, size = 5) +
        labs(x = NULL, y = "Count")
      
      
    }
    
    
    
  })
  
}

ui <- fluidPage(uiOutput("ui"))

runApp(shinyApp(ui, server))

