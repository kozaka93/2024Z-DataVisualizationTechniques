library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(fmsb)
library(ggplot2)
library(lubridate)
library(tidyr)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)



# Wczytanie modułów
source("pages/home.R")
source("pages/apps.R")
source("pages/activities.R")
source("pages/myDay.R")

# Załadowanie grafiki
svg_image5 <- "images/WUT.svg"
svg_data5 <- base64enc::dataURI(file = svg_image5, mime = "image/svg+xml")


# Główny motyw aplikacji
my_theme <- bs_theme(
  version = 5,
  bootswatch = "lumen" 
)

ui <- fluidPage(
  title = "MY DAY",
  theme = my_theme,
  tags$style(HTML("
    .nav-tabs {
      background-color: var(--bs-primary);
      border-bottom: 2px solid var(--bs-primary);
    }
    .nav-tabs .nav-link.active {
      background-color: var(--bs-light);
      color: var(--bs-dark);
    }
    .nav-tabs .nav-link {
      color: var(--bs-light);
    }
    .nav-tabs .nav-link:hover {
      color: var(--bs-dark);
    }
  ")),
  tabsetPanel(
    tabPanel("Home", homeUI("home"), icon = icon("cat")),
    tabPanel("Apps", appsUI("apps"), icon = icon("mobile")),
    tabPanel("Activities", activitiesUI("activities"), icon = icon("person-running")),
    tabPanel("My Day", myDayUI("myDay"), icon = icon("calendar")),
  ),
  
  footer <- shiny::HTML(paste0("
  <footer class='text-center text-sm-start' style='width:100%; position: fixed; bottom: 0; background-color: #f8f9fa; padding: 0;'>
    <hr style='margin: 0;'>
    <div style='display: flex; justify-content: center; align-items: center; gap: 1px;'>
      <img src='", svg_data5, "' width='200' height='40' alt='Logo' style='margin: 0;' />
    </div>
  </footer>
"))
  
)

# Główny serwer aplikacji, inicjuje wszystkie moduły
server <- function(input, output, session) {
  homeServer("home")
  appsServer("apps")
  activitiesServer("activities")
  myDayServer("myDay")
}

shinyApp(ui = ui, server = server)

