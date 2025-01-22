library(bslib)
library(shiny)

fluidPage(
  ### THEME ###
  theme = bs_theme(
    bg = "#1D1B56",
    fg = "#FFFFFF",
    primary = "#FF6770",
    base_font = font_google("Space Mono"),
    code_font = font_google("Space Mono")
  ),
  
  ### TABS ###
  titlePanel("THE MESSENGERS"),
  tabsetPanel(
    ### PANEL 1 ###
    tabPanel("Individual Stats",
             sidebarLayout(
               sidebarPanel(radioButtons(
                 "name",                           # Input ID
                 "Choose your character:",         # Label
                 c("Kamila" = "Kamila Więckowska",      # Display "Kamila", but use "kamila_value"
                   "Kuba" = "Kuba Rybak",          # Display "Kuba", but use "kuba_value"
                   "Liliana" = "Liliana Sirko")    # Display "Liliana", but use "liliana_value"
               )
               ),
               mainPanel(
                 
                 ### START OF MAIN PANEL ###
                 #plot 7
                 fluidRow(
                   column(8, plotOutput("plot7")),
                   column(4, 
                          radioButtons(
                            "filter_option", 
                            label = "Select Data to Display", 
                            choices = list(
                              "Holidays" = "TRUE",          # Display "Holidays", value is "holidays"
                              "Academic Year" = "FALSE", # Display "Academic Year", value is "academic_year"
                              "Total" = "total"                  # Display "Total", value is "total"
                            ), 
                            selected = "total"                   # Default selection
                          )
                   )
                 ),
                 
                 #plot 8
                 fluidRow(
                   column(8, plotOutput("plot8"))
                 #   column(4,
                 #          radioButtons(
                 #            inputId = "quarter",  # ID to use in the server logic
                 #            label = "Select Quarter:",  # Label above the radio buttons
                 #            choices = list(  # List of choices
                 #              "Q1 (Jan - Mar)" = '1',
                 #              "Q2 (Apr - Jun)" = '2',
                 #              "Q3 (Jul - Sep)" = '3',
                 #              "Q4 (Oct - Dec)" = '4'), selected = 1  # Default selected value)
                 # ) #end of radio button
               #) #end of col 1
             ), #end of fluid row
             
             #plot 9
             fluidRow(
               column(8, plotOutput("plot9"))
             ),
             
             #############
             #plot 10
             fluidRow(
               column(8, plotOutput("plot10"))
             )
             
             ############
             
           ), #end of main panel
             ) #end of sidebar layout
    ), #end of tab 1
    
    ### PANEL 2 ###
    tabPanel(
      "Ranking",
      fluidPage(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
          tags$script(src = "script.js")
        ),
        shinyjs::useShinyjs(),
        
        ### FIRST 3 CUBES ###
        ### !!! TODO: make separate function for cubes !!! ###
        fluidRow(column(
          4,
          div(class = "cube-container-container",
            div(
              class = "cube-container",
              id = "cube-container-1",
              div(
                class = "cube",
                id = "first_cube",
                onclick = "kliknijKwadrat(1)",
                div(class = "face", "Kuba"),
                div(class = "face", "Kuba"),
                div(class = "face", "Kamila"),
                div(class = "face", "Kamila"),
                div(class = "face", "Liliana"),
                div(class = "face", "Liliana")
              )
            )
          )
        ),
        column(
          4,
          div(class = "cube-container-container",
            div(
              class = "cube-container",
              id = "cube-container-2",
              div(
                class = "cube",
                id = "second_cube",
                onclick = "kliknijKwadrat(2)",
                div(class = "face", "Kuba"),
                div(class = "face", "Kuba"),
                div(class = "face", "Kamila"),
                div(class = "face", "Kamila"),
                div(class = "face", "Liliana"),
                div(class = "face", "Liliana")
              )
            )
          )
        ),
        column(
          4,
          div(class = "cube-container-container",
            div(
              class = "cube-container",
              id = "cube-container-3",
              div(
                class = "cube",
                id = "third_cube",
                onclick = "kliknijKwadrat(3)",
                div(class = "face", "Kuba"),
                div(class = "face", "Kuba"),
                div(class = "face", "Kamila"),
                div(class = "face", "Kamila"),
                div(class = "face", "Liliana"),
                div(class = "face", "Liliana")
              )
            )
          )
        )),
        
        
        fluidRow(
          column(4, 
                 div(class = "text_container",
                     textOutput("text1"))),
          column(4,
                 div(class = "text_container",
                     textOutput("text2"))),
          column(4, 
                 div(class = "text_container",
                     textOutput("text3")))
        ),
        
        fluidRow(
          column(4,
                 shinyjs::hidden(div(
                   id = "first_plot",
                   plotOutput("plot1")
                 ))),
          column(4,
                 shinyjs::hidden(div(
                   id = "second_plot",
                   plotOutput("plot2")
                 ))),
          column(4,
                 shinyjs::hidden(div(
                   id = "third_plot",
                   plotOutput("plot3")
                 )))
          
        ),
        ### NEXT 3 CUBES ###
        
        fluidRow(
          shinyjs::useShinyjs(),
          
          column(4,
                div(class = "cube-container-container",
                 div(
                   class = "cube-container",
                   id = "cube-container-4",
                   div(
                     class = "cube",
                     id = "fourth_cube",
                     onclick = "kliknijKwadrat(4)",
                     div(class = "face", "Kuba"),
                     div(class = "face", "Kuba"),
                     div(class = "face", "Kamila"),
                     div(class = "face", "Kamila"),
                     div(class = "face", "Liliana"),
                     div(class = "face", "Liliana")
                   )
                  )
                )
            ),
          column(4,
                 div(class = "cube-container-container",
                   div(
                     class = "cube-container",
                     id = "cube-container-5",
                     div(
                       class = "cube",
                       id = "fifth_cube",
                       onclick = "kliknijKwadrat(5)",
                       div(class = "face", "Kuba"),
                       div(class = "face", "Kuba"),
                       div(class = "face", "Kamila"),
                       div(class = "face", "Kamila"),
                       div(class = "face", "Liliana"),
                       div(class = "face", "Liliana")
                     )
                   )
                 )
                 ),
          column(4,
                 div(class = "cube-container-container",
                   div(
                     class = "cube-container",
                     id = "cube-container-6",
                     div(
                       class = "cube",
                       id = "sixth_cube",
                       onclick = "kliknijKwadrat(6)",
                       div(class = "face", "Kuba"),
                       div(class = "face", "Kuba"),
                       div(class = "face", "Kamila"),
                       div(class = "face", "Kamila"),
                       div(class = "face", "Liliana"),
                       div(class = "face", "Liliana")
                     )
                   )
                 )
                 )
          
        ),
        
        ### PLOTS DEFINED IN SERVER ###
        fluidRow(
          column(4, 
                 div(class = "text_container",
                     textOutput("text4"))),
          column(4,
                 div(class = "text_container",
                     textOutput("text5"))),
          column(4, 
                 div(class = "text_container",
                     textOutput("text6")))
        ),
        
        fluidRow(
          column(4,
                 shinyjs::hidden(div(
                   id = "fourth_plot",
                   plotOutput("plot4")
                 ))),
          column(4,
                 shinyjs::hidden(div(
                   id = "fifth_plot",
                   plotOutput("plot5")
                 ))),
          column(4,
                 shinyjs::hidden(div(
                   id = "sixth_plot",
                   plotOutput("plot6")
                 )))
        ), #end of fluidRow
      fluidRow(
        column(12,
               div(
                 id = "margines"
               ))
      )
        ) #end of main Panel
    ) #end of tabPanel 2
  ) #end of tabsetPanel
)  #end of fluid page