library(readxl)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(grid)
library(png)
library(DT)

karo <- read_excel("TWD2.xlsx",sheet = "wendsey")
jular <- read_excel("TWD2.xlsx",sheet = "nickiMinaj ")
julat <- read_excel("TWD2.xlsx",sheet = "ZIPZOP")
weather <- read.csv("pogoda.csv", skip=2)



# Funkcje do tworzenia skarpetek, koszulki i spodni
# Skarpetki - jak w poprzednim kodzie
create_circle <- function(center_x = 0, center_y = 0, radius = 1, n_points = 100, group_name = "circle") {
  theta <- seq(0, 2 * pi, length.out = n_points)
  data.frame(
    x = center_x + radius * cos(theta),
    y = center_y + radius * sin(theta),
    group = group_name
  )
}

create_rectangle <- function(x_min, x_max, y_min, y_max, group_name) {
  data.frame(
    x = c(x_min, x_max, x_max, x_min, x_min),
    y = c(y_min, y_min, y_max, y_max, y_min),
    group = group_name
  )
}

rotate_points <- function(data, angle_degrees) {
  angle_radians <- angle_degrees * pi / 180
  rotation_matrix <- matrix(
    c(cos(angle_radians), -sin(angle_radians),
      sin(angle_radians), cos(angle_radians)),
    nrow = 2
  )
  rotated_coords <- as.data.frame(t(rotation_matrix %*% t(data[, c("x", "y")])))
  colnames(rotated_coords) <- c("x", "y")
  data.frame(rotated_coords, group = data$group)
}

mirror_points_with_shift <- function(data, shift_x = 3 ) {
  data.frame(
    x = -data$x + shift_x,
    y = data$y,
    group = paste0(data$group, "_mirror")
  )
}

# Generowanie skarpetek
create_socks <- function(shift_x = 6, sock1_x_offset = 1.2, sock2_x_offset = 7.1) {
  # Pierwsza skarpetka
  circle_data <- create_circle(center_x = 0.4 + sock1_x_offset, center_y = -6.5, radius = 0.7, group_name = "circle1")
  circle_data1 <- create_circle(center_x = 1.05 + sock1_x_offset, center_y = -6.5, radius = 0.7, group_name = "circle2")
  rect1_data <- create_rectangle(x_min = 0.4 + sock1_x_offset, x_max = 1.15 + sock1_x_offset, y_min = -7.2, y_max = -5.8, group_name = "rect1")
  rect2_data <- create_rectangle(x_min = 1 + sock1_x_offset, x_max = 2.3 + sock1_x_offset, y_min = -6.2, y_max = -5, group_name = "rect2")
  rect2_data <- rotate_points(rect2_data, angle_degrees = 5)
  
  sock1_data <- rbind(circle_data, circle_data1, rect1_data, rect2_data)
  
  # Lustrzana skarpetka
  sock2_data <- mirror_points_with_shift(sock1_data, shift_x = sock2_x_offset)
  
  rbind(sock1_data, sock2_data)
}

# Kurtka
jacket_data <- data.frame(
  x = c(3.4, 2.3, -1, -1, 2, 2, 3.4, 3.4, 5, 5, 8, 8, 4.7, 3.4, 3.4),
  y = c(4.8, 5.5, 5, 3, 3.5, 0, 4.8, 4.8, 0, 3.5, 3, 5, 5.5, 4.8, 4.8) + 0.2,
  group = "jacket"
)


# Koszulka1
tshirt_data <- data.frame(
  x = c(3, 2.3, 1, 2.3, 2, 2, 4, 4, 5, 5, 4, 8, 4.7, 4, 3),
  y = c(4.8, 5.5, 5, 3, 3.5, 0, 0, 0, 0, 3.5, 3, 5, 5.5, 4.8, 4.8) + 0.2,
  group = "tshirt"
)

#Koszulka2
tshirt_data1 <- data.frame(
  x = c(3, 2.3, 1, 1, 2, 2, 4, 4, 5, 5, 6, 6, 4.7, 4, 3),
  y = c(4.8, 5.5, 5, 3, 3.5, 0, 0, 0, 0, 3.5, 3, 5, 5.5, 4.8, 4.8) + 0.2,
  group = "tshirt"
) 

# Spodnie
pants_data <- data.frame(
  x = c(2, 2, 2, 2, 2, 5, 5.5, 4, 3.5, 3.5, 3, 1.5),
  y = c(0, 0, 0, 0, 0, 0, -5, -5, -1.5, -1.5, -5, -5),
  group = "pants"
)

# Połączenie wszystkich elementów
clothing_data <- rbind(
  create_socks(shift_x = 4),
  tshirt_data,
  pants_data,
  jacket_data
)
clothing_data1 <- rbind(
  create_socks(shift_x = 4),
  tshirt_data1,
  pants_data,
  jacket_data
)
draw_clothing_with_socks <- function(tshirt_color = "red", pants_color = "green", sock_colors = c("blue", "blue"), jacket_color = "yellow") {
  
  # Ustawienie domyślnego koloru dla skarpetek w przypadku NA
  sock_colors[is.na(sock_colors)] <- "transparent"
  
  sock_groups <- c("circle1", "circle2", "rect1", "rect2", "circle1_mirror", "circle2_mirror", "rect1_mirror", "rect2_mirror")
  
  # Filtrowanie danych, jeśli skarpetki mają być usunięte
  if (any(is.na(sock_colors))) {
    clothing_data <- subset(clothing_data, !group %in% sock_groups)
  }
  
  ggplot(data = clothing_data, aes(x = x, y = y, group = group)) +
    # Skarpetki
    geom_polygon(data = subset(clothing_data, group %in% sock_groups),
                 fill = sock_colors[1], show.legend = FALSE) +
    # Spodnie
    geom_polygon(data = subset(clothing_data, group == "pants"),
                 fill = pants_color) +
    # Koszulka
    geom_polygon(data = subset(clothing_data, group == "tshirt"),
                 fill = tshirt_color) +
    # Kurtka
    geom_polygon(data = subset(clothing_data, group == "jacket"),
                 fill = jacket_color) +
    coord_fixed() +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "#75c4cc", color = "#75c4cc"), # Tło panelu
      plot.background = element_rect(fill = "#75c4cc", color = "#75c4cc"),
      plot.margin = margin(0,132,0,132, unit = "pt")# Tło całego wykresu
    ) +
    # Skala kolorów dla skarpetek
    scale_fill_manual(values = c(
      "circle1" = sock_colors[1], "circle2" = sock_colors[1], "rect1" = sock_colors[1], "rect2" = sock_colors[1],
      "circle1_mirror" = sock_colors[2], "circle2_mirror" = sock_colors[2], "rect1_mirror" = sock_colors[2], "rect2_mirror" = sock_colors[2]
    ))
}

draw_clothing_with_socks1 <- function(tshirt_color = "red", pants_color = "green", sock_colors = c("blue", "blue")) {
  
  # sock_colors[is.na(sock_colors)] <- "transparent"
  # Poprawiamy dane i skalę kolorów
  sock_groups <- c("circle1", "circle2", "rect1", "rect2",
                   "circle1_mirror", "circle2_mirror", "rect1_mirror", "rect2_mirror")
  
  ggplot(data = clothing_data1, aes(x = x, y = y, group = group)) +
    # Skarpetki
    geom_polygon(data = subset(clothing_data1, group %in% sock_groups),
                 fill = sock_colors[1], show.legend = FALSE) +
    # Spodnie
    geom_polygon(data = subset(clothing_data1, group == "pants"),
                 fill = pants_color) +
    # Koszulka
    geom_polygon(data = subset(clothing_data1, group == "tshirt"),
                 fill = tshirt_color) +
    coord_fixed() +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "#75c4cc", color = "#75c4cc"), # Tło panelu
      plot.background = element_rect(fill = "#75c4cc", color = "#75c4cc"),
      plot.margin = margin(0,190,0,190, unit = "pt")# Tło całego wykresu
    ) +
    #Skala kolorów dla skarpetek
    scale_fill_manual(values = c(
      "circle1" = sock_colors[1], "circle2" = sock_colors[1], "rect1" = sock_colors[1], "rect2" = sock_colors[1],
      "circle1_mirror" = sock_colors[2], "circle2_mirror" = sock_colors[2], "rect1_mirror" = sock_colors[2], "rect2_mirror" = sock_colors[2]))
  
}


pogoda <- weather %>% 
  mutate(czy_padalo = if_else(rain_sum..mm. != 0.0, TRUE, FALSE),
         czy_slonce = if_else(sunshine_duration..s. != 0.00, TRUE, FALSE),
         czy_snieg = if_else(snowfall_sum..cm. != 0.00, TRUE, FALSE)) %>% 
  mutate(ikonka = case_when(czy_padalo == F & czy_slonce == TRUE ~ "slonce",
                            czy_padalo == FALSE & czy_slonce == FALSE & czy_snieg == FALSE ~ "zwykle_zachmurzenie",
                            czy_snieg == TRUE ~ "snieg",
                            czy_padalo == TRUE & czy_slonce == FALSE ~ "deszcz",
                            czy_padalo == T & czy_slonce == T ~ "deszcz_ze_sloncem"))
pogoda <- rename(pogoda, "temperatura_max" = "temperature_2m_max...C.")
pogoda <- rename(pogoda, "temperatura_min" = "temperature_2m_min...C.")
kolor_paleta <- c(
  "czarny" = "black",
  "biały" = "#F7F7F7",
  "szary" = "gray",
  "beżowy" = "#F5F5DC",
  "brązowy" = "#8B4513",
  "niebieski" = "#0088ff",
  "granatowy" = "#000080",
  "czerwony" = "#DC143C",
  "zielony" = "#3CB371",
  "żółty" = "#F0E68C",
  "różowy" = "#DDA0DD",
  "turkusowy" = '#40E0D0',
  "błękitny" = '#87CEEB',
  "pomarańczowy" = '#FFA000',
  "bordowy" = '#800000',
  "fioletowy" = '#52057B'
)
# Interfejs użytkownika z zakładkami
ui <- fluidPage(
  # tags$head(
  #   imageOutput("image")
  # ),
  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;700&display=swap"
  ),
  tags$style(HTML("
    .nav-tabs {
      background-color: #75c4cc; /* Kolor tła zakładek */
      border-radius: 10px;       /* Zaokrąglone rogi */
      margin-bottom: 10px;       /* Odstęp poniżej zakładek */
    }

    .nav-tabs > li > a {
      color: white;              /* Kolor tekstu zakładek */
      font-size: 14px;           /* Rozmiar czcionki */
      padding: 10px 20px;        /* Odstępy wewnętrzne w zakładkach */
      border-radius: 5px;        /* Zaokrąglone rogi */
      text-transform: uppercase; /* Duże litery */
    }

    .nav-tabs > li > a:hover {
      background-color: #5a9bd4; /* Kolor zakładki po najechaniu */
    }

    .nav-tabs > .active > a {
      background-color: #007782; /* Kolor aktywnej zakładki */
      border-radius: 10px;       /* Zaokrąglone rogi aktywnej zakładki */
      font-weight: bold;         /* Pogrubiona czcionka */
    }
    body {
      background-color: #75c4cc;
    }
    .tab-content {
      background-color: #75c4cc;
      padding: 15px;
    }
    .nav-tabs > li > a {
      background-color: #007782;
      color: white;
    }
    .nav-tabs > li.active > a {
      background-color: #54a3ac;
      color: white;
    }
        {
      background-color: #f0f8ff;  /* Błękitne tło */
      border: 2px solid #4682b4; /* Obramowanie */
      border-radius: 5px;       /* Zaokrąglone rogi */
      padding: 5px;             /* Wewnętrzne odstępy */
      color: #2f4f4f;           /* Kolor tekstu */
      font-size: 16px;          /* Rozmiar tekstu */
    }
    h3 {
      color: white;           /* Kolor nagłówków */
      font-family: 'Poppins', sans-serif;
      font-weight: 600;
    }
    .btn-primary {
      background-color: #4682b4; /* Kolor przycisku */
      color: white;              /* Kolor tekstu */
      border: none;              /* Bez obramowania */
      padding: 10px 20px;        /* Większe odstępy */
      font-size: 16px;           /* Większy tekst */
      border-radius: 10px;       /* Zaokrąglone rogi */
      transition: background-color 0.3s; /* Efekt przejścia */
    }
    .btn-primary:hover {
      background-color: #5a9bd4; /* Jaśniejszy kolor przy najechaniu */
    }
    #tabela_statystyk {
          height: 500px;  /* Zwiększamy wysokość tabeli */
    }
    #tabela_statystyk td, #tabela_statystyk th {
          padding: 15px;  /* Zwiększamy odstępy w komórkach */
    }
    .navbar, .dataTables_wrapper { 
      color: white; 
      background-color: black; 
    }
    table.dataTable thead {
      background-color: #75c4cc; /* Kolor tła całego nagłówka */
      border-bottom: none; /* Usunięcie linii pod nagłówkiem */
    }
    table.dataTable thead th {
      background-color: #5aa6b3; /* Kolor tła nagłówków kolumn */
      color: white; /* Kolor tekstu w nagłówkach */
      font-weight: bold; /* Pogrubienie tekstu w nagłówkach */
    }
    table.dataTable tbody td {
      background-color: #75c4cc; /* Kolor tła komórek tabeli */
      color: white; /* Kolor tekstu w komórkach */
    }
    table.dataTable {
      border-collapse: collapse; /* Usuń odstępy między krawędziami tabeli */
    }
    body {
        font-family: 'Poppins', sans-serif;
    }
    .navbar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 10px;
        background-color: #54acb4;
        border: 2px solid white;
        border-radius = 10px;
    }
    .navbar img {
        width: 80px;           /* Zmniejszenie szerokości logo */
        height: 80px;
        margin-top: -80px;      /* Przesunięcie logo nieco w górę */
        margin-left: 400px;    /* Przesunięcie logo w lewo */
    }
    .navbar .navbar-text {
        font-size: 30px;
        font-weight: bold;
        color: white;
        font-family: 'Poppins', sans-serif;
    }
    label {
      font-family: 'Poppins, sans-serif;
      font-size: 14px;
      color: white;
    }
    /* Styl dla selectInput */
    .selectize-input, .selectize-dropdown {
      background-color: white; /* Tło pól wyboru */
      c
    }
    .selectize-input {
      border: none; /* Usuń obramowanie */
      border-radius: 4px; /* Zaokrąglone rogi */
    }
    h1 {
    color: white;
    font-size: 50px;
    font-weight: bold;
    font-family: 'Poppins', sans-serif;
    text-align: center; 
    font-weight: 600;
    }
    .center-image {
        display: flex;
        justify-content: center;  /* Wyśrodkowanie poziome */
        align-items: center;      /* Wyśrodkowanie pionowe */
        height: 100vh;            /* Wysokość na całej stronie */
    }
    h2 {
    color: white;
    font-size: 35px;
    font-weight: bold;
    font-family: 'Poppins', sans-serif;
    text-align: center; 
    font-weight: 600;
    }
    
  ")),
  br(),
  column(12,
         style = "border: 3px solid white; padding: 20px; border-radius: 15px; background-color: #007782;",
        column(4,
               style = "margin: -30px;",
               plotOutput("logo")
               ),
        column(8, 
               br(),br(),
               h1("Aplikacja Modowa"),
               h2("Witamy w naszej aplikacji, w której dokładnie analizujemy trendy
                  w modzie. Nasze 3 modelki podzieliły się z nami szczegółami swoich strojów z 
                  poprzedniego miesiąca. ")
               )
             
             
      ),
  tabsetPanel(
    tabPanel(
      "Raport z dnia",
      fluidRow(
        column(3, 
               h3("Gdzie byłyśmy?"),
               verbatimTextOutput("day_info"),
               h3("Marki ubrań:"),
               verbatimTextOutput("clothing_info"),
               h3("Rodzaj ubrań:"),
               verbatimTextOutput("clothing_info1")
        ),
        column(5, 
               plotOutput("clothing_plot")
        ),
        column(4, 
               br(),
               br(),
               br(),
               br(),
               br(),
               dateInput("date_picker", "Wybierz datę:",
                         value = "2024-12-05",
                         format = "yyyy-mm-dd",
                         min = "2024-12-05",     
                         max = "2025-01-16"),
               br(),
               selectInput(
                 inputId = "selected_df4",
                 label = "Wybierz osobę:",
                 choices = c("Karolina", "Julia R", "Julia T"),
                 selected = "Karolina"
               )
        )
      )
    ),
    tabPanel(
      "Top 3 Marki",
     fluidRow(
       column(8,
              plotOutput("top_brands_podium", height = "500px", width = "100%")
       ), 
       column(4,
              br(),
              br(),
              h3("Które marki królują"),
              h3("w naszych szafach?"),
          selectInput(
            inputId = "selected_df",
            label = "Wybierz osobę:",
            choices = c("Karolina", "Julia R", "Julia T"),
            selected = "Karolina"
          ),
          br(),
          br(), 
          br(),
          br(),
          selectInput(
            inputId = "selected_column",
            label = "Wybierz kategorię:",
            choices = c("Wszystko", "KoszulkaMarka", "SpodnieMarka", "WierzchneMarka"),
            selected = "Wszystko"
          )
        )
      )
    ),
    tabPanel(
      "Pogoda, a ubiór",
      fluidRow(
        column(4,
          dateInput(
            "date_picker",
            "Wybierz datę:",
            value = "2024-12-05",
            min = "2024-12-05",
            max = "2025-01-16"
          ),
          h3("Pogoda"),
          verbatimTextOutput("temp_info"),
          plotOutput("weatherIcon") # Umieszczamy ikonę pogody pod kalendarzem
        ),
        div(
          h3("Codzienna paleta kolorów"),
          style = "text-align: center; margin-top: 20px; transform: translateX(-5%);" # Wyrównanie i przesunięcie w lewo
        ),
        
        fluidRow(
          column(2,
                 plotlyOutput("tablica_kolorow1")
          ),
          
          column(2,
                 plotlyOutput("tablica_kolorow2")
          ),
          column(2, 
                 plotlyOutput("tablica_kolorow3")
          )
        )
      )
    ),
    tabPanel(
      "Wykres Ocen",
      fluidRow(
        column(3,
               br(),br(),
          h3("Jak zmieniają się oceny strojów?"),
               br(),
               checkboxGroupInput(
            inputId = "selected_person",
            label = "Wybierz osoby:",
            choices = c("Karolina", "Julia R", "Julia T"),
            selected = c("Karolina")  # Domyślnie zaznaczona osoba
          ),
          dateRangeInput(
            inputId = "date_range",
            label = "Zakres dat:",
            start = "2024-12-05",  # Domyślna data początkowa
            end = "2025-01-16"        # Domyślna data końcowa (dzisiejszy dzień)
          )
        ),
        column(9,
          plotlyOutput("line_plot")
        )
      )
    ),
  tabPanel(
    "Wykres najczęsciej noszonych kolorów",
    fluidRow(
      column(2,
             br(), br(),br(),br(),br(),br(),br(),
        selectInput(
          "selected_person2",
          "Wybierz osobę:",
          c("Karolina", "Julia R", "Julia T"),
          selected = "Karolina"
        ),
        br(),br(),br(),br(),
        selectInput(
          "selected_clothing",
          "Wybierz część ubioru:",
          c("Wszystko","Koszulka","Spodnie","Skarpetki", "Wierzchne"),
          selected = "Wszystko"
        )
      ),
        column(10,
               plotlyOutput("bar_plotly")
        )
      )
),

  tabPanel(
    "Oceny, a status dnia i biżuteria",
    
    fluidRow(
      
      column(5,
             plotlyOutput("boxplot_plotly")
      ),
      column(2,
             br(),br(),br(),br(),
             h3("Od czego zależą nasze oceny?"),
        selectInput(
          "selected_person3",
          "Wybierz osobę:",
          c("Karolina","Julia R","Julia T"),
          selected = "Karolina"
        )
      ),
        column(5,
               plotlyOutput("violinplot")
      )
    )
    
  ),
tabPanel(
  "Statystyki",
  fluidRow(
    column(12,
           DTOutput("tabela_statystyk")  # Zmieniamy na DTOutput
    )
  )
)

)
)

# Serwer
server <- function(input, output) {
  # Reaktywne wybieranie ramki danych
  selected_data <- reactive({
    data <- switch(input$selected_df,
                   "Karolina" = karo,
                   "Julia R" = jular,
                   "Julia T" = julat)
    # Usuwanie braków
    data <- drop_na(data)
    return(data)
  })
  
  logo_info <- data.frame(
    Marka = c("Bershka", "Cropp", "Diverse", "Egurrola", "ENRAGE", "GAP", "H&M", "House", "Intimissimi", "NA-KD", 
              "Oysho", "Pull&Bear", "Stradivarius", "odzież używana", "ZARA"),
    LogoPath = c("www/Bershka.png", "www/Cropp.png", "www/Diverse.png", "www/Egurrola.png", "www/Enrage.png", "www/Gap.png", 
                 "www/H&M.png", "www/House.png", "www/Intimissimi.png", "www/Na_kd.png", 
                 "www/Oysho.png", "www/Pull&Bear.png", "www/Stradivarius.png", "www/Uzywana.png", 
                 "www/Zara.png")
  )

  
  output$top_brands_podium <- renderPlot({
    data <- selected_data()
    
    # Filtrowanie danych
    if (input$selected_column == "Wszystko") {
      data <- data %>%
        select(KoszulkaMarka, SpodnieMarka, WierzchneMarka) %>%
        pivot_longer(cols = everything(), names_to = "Kategoria", values_to = "Marka") %>%
        drop_na(Marka)
    } else {
      data <- data %>%
        select(all_of(input$selected_column)) %>%
        rename(Marka = all_of(input$selected_column)) %>%
        drop_na(Marka)
    }
    
    # Zliczanie i wybór top 3
    top_3 <- data %>%
      count(Marka, name = "Liczba wystąpień") %>%
      arrange(desc(`Liczba wystąpień`)) %>%
      head(3) %>%
      mutate(Pozycja = rank(-`Liczba wystąpień`, ties.method = "first"))
    
    # Dane do podium
    podium_data <- data.frame(
      Pozycja = c(2, 1, 3),
      Marka = top_3$Marka[match(c(2, 1, 3), top_3$Pozycja)],
      Xmin = c(0, 1, 2),  # Pozycje X
      Xmax = c(1, 2, 3),
      Ymin = c(0, 0, 0),  # Wszystkie zaczynają od tej samej wysokości
      Ymax = c(0.65, 1, 0.5),  # Wysokości stopni podium
      Y_num = c(0.32, 0.47, 0.25)  # Wysokości liczb na stopniach
    )
    
    # Dopasowanie logo do top 3 marek
    podium_data$LogoPath <- logo_info$LogoPath[match(podium_data$Marka, logo_info$Marka)]
    
    # Funkcja do wczytywania plików PNG z zachowaniem oryginalnych proporcji
    load_logo <- function(logo_path) {
      img <- readPNG(logo_path)  # Wczytanie obrazu PNG
      img_dim <- dim(img)[1:2]  # Pobranie wymiarów obrazu (wysokość, szerokość)
      return(list(image = rasterGrob(img), width = img_dim[2], height = img_dim[1]))  # Przechowywanie wymiarów
    }
    
    # Tworzenie podium
    ggplot() +
      # Stopnie podium - jednolity brązowy kolor
      geom_rect(data = podium_data, aes(xmin = Xmin, xmax = Xmax, ymin = Ymin, ymax = Ymax), fill = "#007782") +
      # Numery pozycji na stopniach - różne rozmiary
      geom_text(data = podium_data, aes(x = (Xmin + Xmax) / 2, label = Pozycja), color = "white", size = c(70, 110, 40), y = podium_data$Y_num) +
      # Dodanie logo marek na podium, nad stopniami
      annotation_custom(
        grob = load_logo(podium_data$LogoPath[1])$image,
        xmin = 0, xmax = 1, ymin = podium_data$Ymax[1] + 0.1, ymax = podium_data$Ymax[1] + 0.4
      ) +
      annotation_custom(
        grob = load_logo(podium_data$LogoPath[2])$image,
        xmin = 1, xmax = 2, ymin = podium_data$Ymax[2] + 0.1, ymax = podium_data$Ymax[2] + 0.4
      ) +
      annotation_custom(
        grob = load_logo(podium_data$LogoPath[3])$image,
        xmin = 2, xmax = 3, ymin = podium_data$Ymax[3] + 0.1, ymax = podium_data$Ymax[3] + 0.4
      ) +
      # Tło i proporcje
      coord_cartesian(xlim = c(0, 3), ylim = c(0, 1.5)) +  # Zwiększenie wysokości wykresu
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#75c4cc", color = "#75c4cc"),  # Kolor tła
        panel.background = element_rect(fill = "#75c4cc", color = "#75c4cc"),  # Usunięcie białych ramek
        plot.margin = margin(10, 10, 30, 10) # Zwiększenie marginesów od dołu i góry
      ) +
      # Tytuł wykresu
      labs(title = NULL)
  })
  
  # Reaktywne dane do wykresu ocen z filtrem zakresu dat
  selected_person_data <- reactive({
    # Połączenie danych dla wybranych osób
    selected_data <- list()
    if ("Karolina" %in% input$selected_person) {
      karo$Osoba <- "Karolina"
      selected_data <- append(selected_data, list(karo))
    }
    if ("Julia R" %in% input$selected_person) {
      jular$Osoba <- "Julia R"
      selected_data <- append(selected_data, list(jular))
    }
    if ("Julia T" %in% input$selected_person) {
      julat$Osoba <- "Julia T"
      selected_data <- append(selected_data, list(julat))
    }
    
    # Łączenie danych w jedną ramkę danych
    combined_data <- do.call(rbind, selected_data)
    combined_data$Dzien <- as.Date(combined_data$Dzien)
    
    # Filtr danych na podstawie zakresu dat
    if (!is.null(input$date_range)) {
      combined_data <- combined_data %>%
        filter(Dzien >= input$date_range[1] & Dzien <= input$date_range[2])
    }
    
    return(combined_data)
  })
  
  # Wykres liniowy ocen w plotly
  output$line_plot <- renderPlotly({
    custom_colors <- c("Karolina" = "#0c2c4c", "Julia R" = "#FFA000", "Julia T" = "white")
     plot <- selected_person_data() %>%
      ggplot(aes(x = Dzien, y = Ocenka, color = Osoba, group = Osoba)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = NULL,
           x = NULL,
           y = NULL,
           color = "Osoba") +
       scale_color_manual(values = custom_colors) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(hjust = 1, color = "white", family = 'Poppins'),
        axis.text.x = element_text( hjust = 1, color = "white", family = 'Poppins',margin = margin(t = 10)),
        panel.background = element_rect(fill = "transparent", color = NA),  # Przezroczyste tło
        plot.background = element_rect(fill = "transparent", color = NA), # Przezroczyste tło wykresu
        panel.grid.major = element_blank(),  # Usunięcie dużych linii siatki
        panel.grid.minor = element_blank(),  # Usunięcie małych linii siatki
        panel.grid.major.y = element_line(color = "white"),  # Pozostawienie linii poziomych siatki
        panel.grid.minor.y = element_line(color = "white",size = 0.1),
        legend.title = element_text(color = "white",family = 'Poppins'),
        legend.text = element_text(color = "white",family = 'Poppins')
      )+
      scale_x_date(
        breaks = c(min(selected_person_data()$Dzien), max(selected_person_data()$Dzien)),  # Pokazuje tylko pierwszą i ostatnią datę
        labels = scales::date_format("%d %B")  # Formatuje daty w stylu: 5 grudnia
      )+
      scale_y_continuous(limits = c(0, 10),  # Zakres osi Y od 0 do 10
                          breaks = seq(0, 10, by = 1)) 
    
    ggplotly(plot)  # Konwersja ggplot na interaktywny plotly
  })
  
  
  #dane do wykresu barplot
  selected_data2 <- reactive({
    data <- switch(input$selected_person2,
                   "Karolina" = karo,
                   "Julia R" = jular,
                   "Julia T" = julat)
    # Usuwanie braków
    data <- drop_na(data)
    return(data)
    
    
  })

  
  output$bar_plotly <- renderPlotly({
    
    person <- selected_data2()
    
    if(input$selected_clothing == "Wszystko"){
      data <- person %>% 
        select(c("KoszulkaKolor","SpodnieKolor","SkarpetkiKolor","WierzchneKolor")) %>% 
        pivot_longer(cols = everything(), names_to = "Ubranie", values_to = "Kolor") %>% 
        filter(Kolor != 'NA') %>% 
        drop_na(Kolor)
    }else{
      
      column_name <- paste0(input$selected_clothing, "Kolor")
      data <- person %>%
        select(all_of(column_name)) %>%
        rename(Kolor = all_of(column_name)) %>%
        filter(Kolor != 'NA') %>% 
        drop_na(Kolor)
    }
    
    
    data_sorted <- data %>% 
      count(Kolor) %>% 
      arrange(n)
  
    plot_ly(data_sorted, 
            y = ~reorder(Kolor, n),  # Kolory na osi Y
            x = ~n,  # Liczba wystąpień na osi X
            type = 'bar', 
            color = ~Kolor,  # Kolory przypisane do Kolor
            colors = kolor_paleta,  # Użycie kolorów z color_map
            text = ~paste('Liczba:<br> ', n),  # Tekst pojawiający się po najechaniu
            hoverinfo = 'text',  # Tylko tekst pojawi się po najechaniu
            textposition = 'outside'  # Tekst pojawia się obok słupków
    ) %>% 
      layout(
        title = "Najczęściej noszone kolory oraz ich liczba wystąpień",
        xaxis = list(title = "",showgrid = FALSE,showticklabels = FALSE),  # Oś X teraz reprezentuje liczbę
        yaxis = list(
          title = " ",  # Usunięcie tytułu osi Y
          showticklabels = FALSE,  # Usunięcie wartości na osi Y
          showgrid = FALSE  # Usunięcie siatki na osi Y
        ),
        
        showlegend = FALSE,  # Ukrycie legendy
        plot_bgcolor = 'transparent',  # Przezroczyste tło wykresu
        paper_bgcolor = 'transparent',  # Przezroczyste tło obramowania wykresu
        titlefont = list(size = 18, family = 'Poppins', bold = TRUE,color = "white"),
        bargap = 0.2,  # Zwiększenie odstępu między słupkami
        width = 950,  # Zwiększenie szerokości wykresu
        height = 600
      )
    
    
  })
  
  #dane do wykresu boxplot
  selected_data3 <- reactive({
    data <- switch(input$selected_person3,
                   "Karolina" = karo,
                   "Julia R" = jular,
                   "Julia T" = julat)
    # Usuwanie braków
    data <- drop_na(data)
    return(data)
  })
  
  output$boxplot_plotly <- renderPlotly({
    
    dane <- selected_data3()
    
    plot_ly(
      dane,
      x = ~StatusDnia,
      y = ~as.factor(Ocenka),
      type = "box",
      marker = list(),       # Różowy kolor punktów (outliers)
      line = list()         # Różowy kolor linii box plota
    ) %>% 
      layout(
        title = list(
          text = "Ocena dnia w zależności od statusu dnia",
          font = list(
            color = "white"               # Biały kolor tytułu
          )
        ),
        xaxis = list(
          title = "Status dnia",
          titlefont = list(color = "white"), # Biały kolor tytułu osi X
          tickfont = list(color = "white")  # Biały kolor etykiet osi X
        ),
        yaxis = list(
          title = "Ocena",
          titlefont = list(color = "white"), # Biały kolor tytułu osi Y
          tickfont = list(color = "white")  # Biały kolor etykiet osi Y
        ),
        plot_bgcolor = 'rgba(0, 0, 0, 0)',  # Przezroczyste tło wykresu
        paper_bgcolor = 'rgba(0, 0, 0, 0)'  # Przezroczyste tło papieru
      )
  })
  

  # Funkcja obliczająca statystyki
  oblicz_statystyki <- function(data) {
    # Procenty wzorków dla poszczególnych elementów garderoby
    procent_wzorki_koszulka <- round(mean(data$KoszulkaWzorki == "tak", na.rm = TRUE) * 100, 1)
    procent_wzorki_spodnie <- round(mean(data$SpodnieWzorki == "tak", na.rm = TRUE) * 100, 1)
    procent_wzorki_skarpetki <- round(mean(data$SkarpetkiWzorki == "tak", na.rm = TRUE) * 100, 1)
    procent_wzorki_wierzchnie <- round(mean(data$WierzchnieWzorki == "tak", na.rm = TRUE) * 100, 1)
    
    # Sumaryczne wzorki - średnia procentów wzorków w różnych częściach garderoby
    sumaryczne_wzorki <- round(mean(c(procent_wzorki_koszulka, procent_wzorki_spodnie, procent_wzorki_skarpetki, procent_wzorki_wierzchnie)), 1)
    
    # Obliczanie statystyk
    c(
      Średnia_ocena = round(mean(data$Ocenka, na.rm = TRUE), 2),
      Biżuteria = paste0(round(mean(data$Biżuteria == "tak", na.rm = TRUE) * 100, 1), "%"),
      Top_status_dnia = names(sort(table(data$StatusDnia), decreasing = TRUE)[1]),
      Wzorki_góra_stroju = paste0(procent_wzorki_koszulka, "%"),
      Wzorki_dół_stroju = paste0(procent_wzorki_spodnie, "%"),
      Skarpetki_z_wzorkami = paste0(procent_wzorki_skarpetki, "%"),
      Wierzchnie_z_wzorkami = paste0(procent_wzorki_wierzchnie, "%"),
      Sumaryczne_wzorki = paste0(sumaryczne_wzorki, "%")
    )
  }
  
  # Obliczanie statystyk dla każdej osoby
  statystykiKaro <- oblicz_statystyki(karo)
  statystykiJuliaR <- oblicz_statystyki(jular)
  statystykiJuliaT <- oblicz_statystyki(julat)
  
  output$tabela_statystyk <- renderDT({
    # Tworzenie opisu (nagłówki wierszy)
    opis <- c(
      "Średnia ocena",
      "Biżuteria",
      "Top status dnia",
      "Góra stroju we wzorki",
      "Dół stroju we wzorki",
      "Skarpetki we wzorki",
      "Wierzchnie we wzorki",
      "Sumaryczne wzorki"
    )
    
    # Tworzenie tabeli wyników
    tabela <- data.frame(
      Karolina = statystykiKaro,
      `Julia R` = statystykiJuliaR,
      `Julia T` = statystykiJuliaT,
      check.names = FALSE
    )
    
    # Ustawienie opisów jako nazw wierszy (indeksów)
    rownames(tabela) <- opis
    
    # Zastosowanie formatu w DT
    datatable(tabela,
              options = list(
                pageLength = nrow(tabela),  # Wyświetlamy wszystkie wiersze
                lengthChange = FALSE,      # Wyłączamy możliwość zmiany liczby wierszy
                paging = FALSE,            # Wyłączamy paginację
                scrollX = TRUE,            # Umożliwiamy przewijanie w poziomie
                dom = 't',                 # Wyświetlamy tylko tabelę
                columnDefs = list(
                  list(targets = 1:3, className = 'dt-center')  # Wyrównanie tekstu w kolumnach
                )
              ),
              style = "default"  # Korzystamy z Bootstrap 4
    )
  })
  
  output$tablica_kolorow1 <- renderPlotly({
    
    selected_date <- as.POSIXct(input$date_picker)
    
    data <- karo %>% 
      filter(Dzien == selected_date) %>% 
      select(c('SpodnieKolor','KoszulkaKolor','SkarpetkiKolor','WierzchneKolor')) %>%
      unlist() %>% 
      as.character()
    
    if(is.na(data[4])){
      
      data <- head(data,-1)
      fig <- plot_ly(
        type = 'scatter',
        mode = 'markers',
        x = c(0.25, 0.81, 0.21), 
        y = c(0.25,0.25, 0.54), 
        marker = list(
          color = kolor_paleta[data],       
          size = 80,         
          symbol = 'square'
          #line = list(width = 2, color = 'black')  
        ),
        text = c("Spodnie", "Koszulka", "Skarpetki"), 
        hoverinfo = 'text'    
      ) %>%
        layout(
          title = list(
            text = "Karolina",  # Tytuł wykresu
            font = list(
              color = "white",  # Kolor tytułu
              size = 25,        # Rozmiar czcionki
              family = "Poppins, sans-serif", # Domyślna czcionka Plotly
              bold = TRUE       # Pogrubienie
            ),
            x = 0.5,  # Centrowanie tytułu
            xanchor = "center",
            y = 0.80  # Obniżenie tytułu; zmniejsz wartość, aby obniżyć
          ),
          xaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          showlegend = FALSE,
          plot_bgcolor = 'rgba(0, 0, 0, 0)',  # Przezroczyste tło wykresu
          paper_bgcolor = 'rgba(0, 0, 0, 0)',
          shapes = list(
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0.5, x1 = 1, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0.5, y1 = 1, line = list(width = 0))
          )
        )
    }
    else{
      fig <- plot_ly(
        type = 'scatter',
        mode = 'markers',
        x = c(0.25, 0.81, 0.25, 0.81), 
        y = c(0.25,0.25, 0.54, 0.54),
        marker = list(
          color = kolor_paleta[data],       
          size = 80,          
          symbol = 'square'
          #line = list(width = 2, color = 'black')  
        ),
        text = c("Spodnie", "Koszulka", "Skarpetki", "Wierzchne"), 
        hoverinfo = 'text'    
      ) %>%
        layout(
          title = list(
            text = "Karolina",  # Tytuł wykresu
            font = list(
              color = "white",  # Kolor tytułu
              size = 25,        # Rozmiar czcionki
              family = "Poppins, sans-serif", # Domyślna czcionka Plotly
              bold = TRUE    # Pogrubienie
          ),
          x = 0.5,  # Centrowanie tytułu
          xanchor = "center",
          y = 0.80  # Obniżenie tytułu; zmniejsz wartość, aby obniżyć
        ),
          xaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE),
          yaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE),
          showlegend = FALSE,
          plot_bgcolor = 'rgba(0, 0, 0, 0)',  # Przezroczyste tło wykresu
          paper_bgcolor = 'rgba(0, 0, 0, 0)',
          shapes = list(
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0.5, x1 = 1, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0.5, y1 = 1, line = list(width = 0)), 
            list(type = 'rect', x0 = 0.5, x1 = 1, y0 = 0.5, y1 = 1, line = list(width = 0)) 
            
          )
        )
      
    }
    fig
    
  })
  output$tablica_kolorow2 <- renderPlotly({
    
    selected_date <- as.POSIXct(input$date_picker)
    
    data <- jular %>% 
      filter(Dzien == selected_date) %>% 
      select(c('SpodnieKolor','KoszulkaKolor','SkarpetkiKolor','WierzchneKolor')) %>%
      unlist() %>% 
      as.character()
    
    if(is.na(data[4])){
      
      data <- head(data,-1)
      fig <- plot_ly(
        type = 'scatter',
        mode = 'markers',
        x = c(0.25, 0.81, 0.21),
        y = c(0.25,0.25, 0.54),
        marker = list(
          color = kolor_paleta[data],       
          size = 80,         
          symbol = 'square' 
          #line = list(width = 2, color = 'black')  
        ),
        text = c("Spodnie", "Koszulka", "Skarpetki"), 
        hoverinfo = 'text'    
      ) %>%
        layout(
          title = list(
            text = "Julia R",  # Tytuł wykresu
            font = list(
              color = "white",  # Kolor tytułu
              size = 25,        # Rozmiar czcionki
              family = "Poppins, sans-serif", # Domyślna czcionka Plotly
              bold = TRUE       # Pogrubienie
            ),
            x = 0.5,  # Centrowanie tytułu
            xanchor = "center",
            y = 0.80  # Obniżenie tytułu; zmniejsz wartość, aby obniżyć
          ),
          xaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          showlegend = FALSE,
          plot_bgcolor = 'rgba(0, 0, 0, 0)',  # Przezroczyste tło wykresu
          paper_bgcolor = 'rgba(0, 0, 0, 0)',
          shapes = list(
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0.5, x1 = 1, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0.5, y1 = 1, line = list(width = 0))
          )
        )
    }
    else{
      fig <- plot_ly(
        type = 'scatter',
        mode = 'markers',
        x = c(0.25, 0.81, 0.25, 0.81), 
        y = c(0.25,0.25, 0.54, 0.54),  
        marker = list(
          color = kolor_paleta[data],       
          size = 80,          
          symbol = 'square'
          #line = list(width = 2, color = 'black')  
        ),
        text = c("Spodnie", "Koszulka", "Skarpetki", "Wierzchne"), 
        hoverinfo = 'text'    
      ) %>%
        layout(
          title = list(
            text = "Julia R",  # Tytuł wykresu
            font = list(
              color = "white",  # Kolor tytułu
              size = 25,        # Rozmiar czcionki
              family = "Poppins, sans-serif", # Domyślna czcionka Plotly
              bold = TRUE       # Pogrubienie
            ),
            x = 0.5,  # Centrowanie tytułu
            xanchor = "center",
            y = 0.80  # Obniżenie tytułu; zmniejsz wartość, aby obniżyć
          ),
          xaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          showlegend = FALSE,
          plot_bgcolor = 'rgba(0, 0, 0, 0)',  # Przezroczyste tło wykresu
          paper_bgcolor = 'rgba(0, 0, 0, 0)',
          shapes = list(
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0.5, x1 = 1, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0.5, y1 = 1, line = list(width = 0)), 
            list(type = 'rect', x0 = 0.5, x1 = 1, y0 = 0.5, y1 = 1, line = list(width = 0)) 
            
          )
        )
      
    }
    fig
    
  })
  
  output$tablica_kolorow3 <- renderPlotly({
    
    selected_date <- as.POSIXct(input$date_picker)
    
    data <- julat %>% 
      filter(Dzien == selected_date) %>% 
      select(c('SpodnieKolor','KoszulkaKolor','SkarpetkiKolor','WierzchneKolor')) %>%
      unlist() %>% 
      as.character()
    
    if(is.na(data[4])){
      
      data <- head(data,-1)
      fig <- plot_ly(
        type = 'scatter',
        mode = 'markers',
        x = c(0.25, 0.81, 0.21),
        y = c(0.25,0.25, 0.54),
        marker = list(
          color = kolor_paleta[data],       
          size = 80,         
          symbol = 'square'
          #line = list(width = 2, color = 'black')  
        ),
        text = c("Spodnie", "Koszulka", "Skarpetki"), 
        hoverinfo = 'text'    
      ) %>%
        layout(
          title = list(
            text = "Julia T",  # Tytuł wykresu
            font = list(
              color = "white",  # Kolor tytułu
              size = 25,        # Rozmiar czcionki
              family = "Poppins, sans-serif", # Czcionka Poppins # Domyślna czcionka Plotly
              bold = TRUE       # Pogrubienie
            ),
            x = 0.5,  # Centrowanie tytułu
            xanchor = "center",
            y = 0.80  # Obniżenie tytułu; zmniejsz wartość, aby obniżyć
          ),
          xaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          showlegend = FALSE,
          plot_bgcolor = 'rgba(0, 0, 0, 0)',  # Przezroczyste tło wykresu
          paper_bgcolor = 'rgba(0, 0, 0, 0)',
          shapes = list(
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0.5, x1 = 1, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0.5, y1 = 1, line = list(width = 0))
          )
        )
    }
    else{
      fig <- plot_ly(
        type = 'scatter',
        mode = 'markers',
        x = c(0.25, 0.81, 0.25, 0.81),
        y = c(0.25,0.25, 0.54, 0.54),
        marker = list(
          color = kolor_paleta[data],       
          size = 80,          
          symbol = 'square' 
          #line = list(width = 2, color = 'black')  
        ),
        text = c("Spodnie", "Koszulka", "Skarpetki", "Wierzchne"), 
        hoverinfo = 'text'    
      ) %>%
        layout(
          title = list(
            text = "Julia T",  # Tytuł wykresu
            font = list(
              color = "white",  # Kolor tytułu
              size = 25,        # Rozmiar czcionki
              family = "Poppins, sans-serif", # Czcionka Poppins # Domyślna czcionka Plotly
              bold = TRUE       # Pogrubienie
            ),
            x = 0.5,  # Centrowanie tytułu
            xanchor = "center",
            y = 0.80  # Obniżenie tytułu; zmniejsz wartość, aby obniżyć
          ),
          xaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(range = c(-0.1, 1.1), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          showlegend = FALSE,
          plot_bgcolor = 'rgba(0, 0, 0, 0)',  # Przezroczyste tło wykresu
          paper_bgcolor = 'rgba(0, 0, 0, 0)',
          shapes = list(
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0.5, x1 = 1, y0 = 0, y1 = 0.5, line = list(width = 0)), 
            list(type = 'rect', x0 = 0, x1 = 0.5, y0 = 0.5, y1 = 1, line = list(width = 0)), 
            list(type = 'rect', x0 = 0.5, x1 = 1, y0 = 0.5, y1 = 1, line = list(width = 0)) 
          )
        )
    }
    fig
    
  })
  
  
  output$violinplot <- renderPlotly({
    
    dane <- selected_data3()
    
    plot_ly(
      dane, 
      x = ~Biżuteria, 
      y = ~as.factor(Ocenka), 
      type = "violin", 
      meanline = list(visible = TRUE),
      marker = list() # Kolor violin plota
    ) %>%
      layout(
        title = list(
          text = "Ocena w zależności od noszenia biżuterii",
          font = list(
            color = "white"  # Biały kolor tytułu
          )
        ),
        xaxis = list(
          title = "Noszenie biżuterii",
          titlefont = list(color = "white"), # Biały kolor tytułu osi X
          tickfont = list(color = "white")  # Biały kolor etykiet osi X
        ),
        yaxis = list(
          title = "Ocena",
          titlefont = list(color = "white"), # Biały kolor tytułu osi Y
          tickfont = list(color = "white")  # Biały kolor etykiet osi Y
        ),
        plot_bgcolor = 'rgba(0, 0, 0, 0)',  # Przezroczyste tło wykresu
        paper_bgcolor = 'rgba(0, 0, 0, 0)'  # Przezroczyste tło papieru
      )
  })
  output$weatherIcon1 <- renderPlot({
    icon_path <- "twd.png"
    img_data <- png::readPNG(icon_path)
    grid::grid.raster(img_data, width = 0.3, height = 0.3)
  }, bg = NA)
    
  output$weatherIcon <- renderPlot({
    
    
    # Wybór pogody dla konkretnego dnia (na przykład pierwszy dzień)
    selected_day <- input$date_picker  # Możesz to dostosować
    selected_weather <- pogoda %>% 
      filter(Dzien == selected_day)
    
    # Przypisywanie obrazków do warunków pogodowych
    icon_path <- switch(selected_weather$ikonka,
                        "slonce" = "www/sloneczko.png",
                        "zwykle_zachmurzenie" = "www/zwykle_zachmurzenie.png",
                        "deszcz" = "www/deszcz.png",
                        "snieg"= "www/snieg.png",
                        "deszcz_ze_sloncem" = "www/deszcz_ze_sloncem.png")
    
    # Wyświetlanie obrazka
    img_data <- png::readPNG(icon_path)
    
    # Wyświetlanie obrazu jako wykresu
    
    grid::grid.raster(img_data, height = 0.8, width = 0.8)

    
  }, bg = NA)
  
  output$temp_info <- renderText({
    
    selected_day <- input$date_picker  # Możesz to dostosować
    selected <- pogoda %>% 
      filter(Dzien == selected_day)
    
    paste(
      "Najniższa temperatura:", selected$temperatura_min, "°C",
      "\nNajwyższa temperatura:", selected$temperatura_max, "°C"
    )
    
    
    
  })
  
  
  
  selected_date <- reactive({
    as.POSIXct(input$date_picker)
  })
  selected_data4 <- reactive({
    data <- switch(input$selected_df4,
                   "Karolina" = karo,
                   "Julia R" = jular,
                   "Julia T" = julat)
    return(data)
  })
  output$day_info <- renderText({
    
    data <- selected_data4()
    
    data <- data %>%
      filter(Dzien == selected_date()) 
    paste(
      data$Dzien, ":",
      data$StatusDnia
    )
  })
  
  # Informacje o ubraniach po prawej stronie
  output$clothing_info <- renderText({
    data <- selected_data4()
    data <- data %>%
      filter(Dzien == selected_date()) 
    if (is.na(data$WierzchneMarka)) {
    paste(
      "Koszulka:", data$KoszulkaMarka,
      "\nWierzchne:", "",
      "\nSpodnie:", data$SpodnieMarka
    )
    }
    else {
        paste(
          "Koszulka:", data$KoszulkaMarka,
          "\nWierzchne:", data$WierzchneMarka,
          "\nSpodnie:", data$SpodnieMarka
        )
    }
  })
  output$clothing_info1 <- renderText({
    data <- selected_data4()
    data <- data %>%
      filter(Dzien == selected_date()) 
    if (is.na(data$WierzchneMarka)) {
      paste(
        "Koszulka:", data$KoszulkaRodzaj,
        "\nWierzchne:", "",
        "\nSpodnie:", data$SpodnieRodzaj
      )
    }
    else {
      paste(
        "Koszulka:", data$KoszulkaRodzaj,
        "\nWierzchne:", data$WierzchneRodzaj,
        "\nSpodnie:", data$SpodnieRodzaj
      )
    }
  })
  output$clothing_plot <- renderPlot({
    # Pobranie danych dla wybranej daty
    data <- selected_data4()
    data <- data %>%
      filter(Dzien == selected_date()) %>%
      select(c('KoszulkaKolor', 'SpodnieKolor', 'SkarpetkiKolor', 'WierzchneKolor')) %>%
      unlist() %>%
      as.character()
    

    if (is.na(data[4])) {
      cat("WierzchneKolor to NA - wywoływanie draw_clothing_with_socks1\n")
      draw_clothing_with_socks1(
        tshirt_color = kolor_paleta[data[1]],
        pants_color = kolor_paleta[data[2]],
        sock_colors = c(kolor_paleta[data[3]], kolor_paleta[data[3]])
      )
    } else {
      cat("WierzchneKolor:", data[4], "- wywoływanie draw_clothing_with_socks\n")
      draw_clothing_with_socks(
        tshirt_color = kolor_paleta[data[1]],
        pants_color = kolor_paleta[data[2]],
        sock_colors = c(kolor_paleta[data[3]], kolor_paleta[data[3]]),
        jacket_color = kolor_paleta[data[4]]
      )
    }
  
  })
  output$logo <- renderPlot({
    # Wybór pogody dla konkretnego dnia (na przykład pierwszy dzień)
    
    
    # Przypisywanie obrazków do warunków pogodowych
    icon_path <- "www/twd.png"
    
    # Wyświetlanie obrazka
    img_data <- png::readPNG(icon_path)
    
    # Wyświetlanie obrazu jako wykresu
    
    grid::grid.raster(img_data, height = 1, width = 1)
    
    
  }, bg = NA)

  
}

# Uruchom aplikację


shinyApp(ui = ui, server = server)
