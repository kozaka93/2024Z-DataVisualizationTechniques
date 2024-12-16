library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
path_to_data <- "./2023_PieczaZastepcza.xlsx"

dane_1 <- read_excel(
  path = path_to_data,
  sheet = "TABL.I.14",
  col_names = TRUE,
  range="A14:C29")

dane_2 <- read_excel(
  path = path_to_data,
  sheet = "TABL.I.14",
  col_names = TRUE,
  range="A34:C49")

dane_3 <- read_excel(
  path = path_to_data,
  sheet = "TABL.I.14",
  col_names = TRUE,
  range="A54:C69")

dane_4 <- read_excel(
  path = path_to_data,
  sheet = "TABL.I.14",
  col_names = TRUE,
  range="A74:C89")

dane_5 <- read_excel(
  path = path_to_data,
  sheet = "TABL.I.14",
  col_names = TRUE,
  range="A94:C109")

dane_6 <- read_excel(
  path = path_to_data,
  sheet = "TABL.I.14",
  col_names = TRUE,
  range="A114:C129")

dane_7 <- read_excel(
  path = path_to_data,
  sheet = "TABL.I.14",
  col_names = TRUE,
  range="A134:C149")


correct_data <- function(df){
  
  df <- df%>%select(-"...2")
  
  df <- rbind(df,c(names(df)[1],
                   substring(names(df)[2],2)))
  
  df <-df %>%rename_with(~"Liczba_dzieci",.cols=2)%>%
    rename_with(~"Wojewódzctwo",.cols=1)%>%
    mutate(Wojewódzctwo = as.character(Wojewódzctwo))
  
  
  
  df<-df%>%mutate(Liczba_dzieci = as.numeric(Liczba_dzieci))
  
  df <- df %>%
    mutate(Wojewódzctwo = recode(Wojewódzctwo,
                                 "Warmińsko-mazurskie" = "Warmińsko-Mazurskie",
                                 "Kujawsko-pomorskie" = "Kujawsko-Pomorskie"))
  
}

rodziny_zastępcze <- correct_data(dane_1)
rodziny_zastępcze_spokrewnione <- correct_data(dane_2)
rodziny_zastępcze_niezawodowe <- correct_data(dane_3)
rodziny_zastępcze_zawodowe <- correct_data(dane_4)
rodziny_zastępcze_zawodowe_spec <- correct_data(dane_5)
rodziny_zastępcze_pogotowie <- correct_data(dane_6)
rodzinne_domy <- correct_data(dane_7)

poland_ne <- ne_states(country = "Poland", returnclass = "sf")
poland_ne <- poland_ne%>%mutate(name = as.character(name))

ui <- fluidPage(
  h1("Liczba dzieci w różnych rodzinach \nzastępczych ze względu na województwo"),
  h4("Wykres przedstawia całkowitą liczbę dzięci pod opieką różnych rodzajów rodzin zastępczych w poszczególnych województwach w roku 2023"),
  h4("Komentarz:"),
  h4("Łatwo zauważyć, że w każdym rodzaju rodziny zastępczej poza rodziną pełniącą role pogotowia rodzinnego,najwięcej dzieci pod opieką rodziny zastępczej mieszka w województwie śląskim.W przypadku rodzin pełniących role pogotowia rodzinnego, najwięcej dzieci jest mieszkańcami wojewódzctwa pomorskiego i wielkopolskiego."),
  selectInput(
    "select", 
    "Wybierz rodzaj rodziny zastępczej",
    list("Rodziny zastępcze (łącznie)" = "1",
         "Rodziny zastępcze spokrewnione" = "2",
         "Rodziny zastępcze zawodowe" = "3",
         "Rodziny zastępcze zawodowe specjalistyczne" = "4",
         "Rodziny zastępcze zawodowe pełniące funkcję pogotowia rodzinnego" = "5",
         "Rodzinne domy dziecka" = "6"),
    multiple = FALSE
  ),
  textOutput("value"),
  tags$head(
    tags$style(HTML("
      .title{
        font-size: 60px
      }
      
      .main_plot {
        display: flex;
        justify-content: center;
        align-items: center;
        margin-top: 110px;
      }
    "))
  ),
  div(class = "main_plot",
      plotOutput("plot",width = "600px",height = "600px")
  )
)

server <- function(input, output) {
  observeEvent(input$select, {
    type <- input$select
    if(type == "1"){
      sheet <- rodziny_zastępcze
    }else if(type == "2"){
      sheet <- rodziny_zastępcze_spokrewnione
    }
    else if(type == "3"){
      sheet <- rodziny_zastępcze_zawodowe
    }
    else if(type == "4"){
      sheet <- rodziny_zastępcze_zawodowe_spec
    }
    else if(type == "5"){
      sheet <- rodziny_zastępcze_pogotowie
    }
    else if(type == "6"){
      sheet <- rodzinne_domy
    }
    output$plot <- renderPlot({
      poland_ne%>%
        left_join(sheet,by=c("name_vi"="Wojewódzctwo"))%>%
        ggplot()+
        geom_sf(aes(fill=`Liczba_dzieci`))+
        scale_fill_gradientn(
          colors = c("#fff7b9","#6e2000"),
          name = "Liczba dzieci pod \n opieką danego rodzaju \n rodziny zastępczej",
        )+
        theme_void()+
        theme(legend.key.size = unit(1,"cm"))
    })
    
  })
}
shinyApp(ui = ui, server = server)
