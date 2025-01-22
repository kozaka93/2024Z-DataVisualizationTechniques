library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
    tags$style(HTML("
	body { background-color: #343a40; }
	.navbar { background-color: #23272b; border-radius: 15px; }
	.nav-tabs {
  	border-radius: 15px;
  	border-bottom: none;
  	margin-bottom: 20px;
  	background-color: #23272b;
	}
	.nav-tabs > li > a {
  	color: #ffffff;
  	border-radius: 15px;
  	padding: 10px 20px;
  	transition: background-color 0.3s ease;
	}
	.nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
  	background-color: #007bff;
  	color: #ffffff;
  	border-radius: 15px;
	}
	.tab-content {
  	border-radius: 15px;
  	overflow: hidden;
  	background-color: #343a40;
	}
	.tab-pane {
  	border-radius: 15px;
  	padding: 20px;
	}
	.container-fluid {
  	padding: 20px;
  	max-width: 1200px;
  	border-radius: 15px;
	}
	.row {
  	margin-bottom: 20px;
	}
	.panel {
  	border-radius: 15px;
  	box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.3);
  	padding: 20px;
	}
	.form-group .input-group.date {
  	margin-bottom: 15px;
	}
	.form-group .input-group.date input {
  	padding: 10px;
	}
	"))
  ),
  
  navbarPage(
    title = "Jak (nie)zdać semestru",
    
    tabPanel("Sen",
             fluidRow(
               column(4,
                      div(class = "panel animate__animated animate__fadeIn",
                          checkboxGroupInput("sen_osoba", "Dane dotyczą:",
                                             choices = c("Maciek", "Wojtek")),
                          radioButtons("sen_wybor_wykres", "Wybierz dane:",
                                       choices = c("Godziny snu", "Czas wstawania i zaśnięcia"),
                                       selected = "Godziny snu"),
                          conditionalPanel(
                            condition = "input.sen_wybor_wykres == 'Czas wstawania i zaśnięcia'",
                            checkboxGroupInput("sen_zasniecie_wstanie", "Dane do wyświetlenia:",
                                               choices = c("Godzina wstawania", "Godzina zaśnięcia"))
                          ),
                          dateRangeInput("sen_data", "Wybierz datę:",
                                         start = Sys.Date() - 7, end = Sys.Date())
                      )
               ),
               column(8,
                      div(class = "panel animate__animated animate__fadeIn",
                          h3("Wykres snu"),
                          plotlyOutput("sen_wykres")
                      )
               )
             )
    ),
    
    tabPanel("Nauka",
             fluidRow(
               column(4,
                      div(class = "panel animate__animated animate__fadeIn",
                          checkboxGroupInput("nauka_osoba", "Dane dotyczą:",
                                             choices = c("Maciek", "Wojtek")),
                          radioButtons("nauka_wybor", "Wybierz dane do wyświetlenia:",
                                       choices = c(
                                         "Godzina rozpoczęcia nauki",
                                         "Czas spędzony na projekty w danym przedmiocie",
                                         "Czas nauki na kolokwia",
                                         "Średni czas nauki przy pełnym i niedobrym śnie"
                                       ),
                                       selected = "Godzina rozpoczęcia nauki"),
                          conditionalPanel(
                            condition = "input.nauka_wybor == 'Czas spędzony na projekty w danym przedmiocie' ||
                                          input.nauka_wybor == 'Czas nauki na kolokwia'",
                            selectInput("nauka_przedmiot", "Przedmiot:",
                                        choices = c("-", "MN", "Java",
                                                    "AiSD", "RP",
                                                    "TWD", "Fizyka"),
                                        selected = "-")
                          ),
                          dateRangeInput("nauka_data", "Wybierz datę:",
                                         start = Sys.Date() - 7, end = Sys.Date())
                      )
               ),
               column(8,
                      div(class = "panel animate__animated animate__fadeIn",
                          h3("Wykres nauki"),
                          plotlyOutput("nauka_wykres")
                      )
               )
             )
    ),
    
    tabPanel("Reszta Czasu",
             fluidRow(
               column(4,
                      div(class = "panel animate__animated animate__fadeIn",
                          checkboxGroupInput("reszta_osoba", "Dane dotyczą:",
                                             choices = c("Maciek", "Wojtek")),
                          radioButtons("reszta_wybierz", "Wybierz kategorię:",
                                       choices = c("Średni czas na wydziale", "Średni czas wolny"),
                                       selected = "Czas na wydziale"),
                          dateRangeInput("reszta_data", "Wybierz datę:",
                                         start = Sys.Date() - 7, end = Sys.Date())
                      )
               ),
               column(8,
                      div(class = "panel animate__animated animate__fadeIn",
                          h3("Wykres reszty czasu"),
                          plotlyOutput("reszta_wykres")
                      )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  output$sen_wykres <- renderPlotly({
    filtered_data <- spanie %>%
      filter(student %in% input$sen_osoba,
             date2 >= input$sen_data[1],
             date2 <= input$sen_data[2])
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    if (input$sen_wybor_wykres == "Godziny snu") {
      p <- ggplot(filtered_data, aes(x = date2, y = time, fill = student)) +
        geom_col(position = position_dodge(width = 1)) +
        labs(
          title = "Czas spania każdego dnia",
          x = "Dzień spania",
          y = "Minuty spania"
        ) +
        theme_minimal() +
        scale_fill_manual(values = c("Maciek" = "blue", "Wojtek" = "orange")) +
        scale_x_date(breaks = "3 days", labels = scales::date_format("%d-%m-%Y")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
          axis.text.y = element_text(color = "white"),
          axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white"),
          plot.title = element_text(color = "white", face = "bold"),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white"),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),  
          panel.grid.minor.x = element_blank()   
        )
      
      
      ggplotly(p) %>% 
        layout(plot_bgcolor = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
      
    } else if (input$sen_wybor_wykres == "Czas wstawania i zaśnięcia") {
      if (is.null(input$sen_zasniecie_wstanie) || length(input$sen_zasniecie_wstanie) == 0) {
        p <- NULL
      } else {
        plot_data <- filtered_data %>%
          pivot_longer(cols = c(time1, time2), names_to = "czas_typ", values_to = "czas") %>%
          filter(
            (czas_typ == "time2" & "Godzina wstawania" %in% input$sen_zasniecie_wstanie) |
              (czas_typ == "time1" & "Godzina zaśnięcia" %in% input$sen_zasniecie_wstanie)
          ) %>%
          mutate(
            legend_label = case_when(
              czas_typ == "time1" & student == "Maciek" ~ "M. zasypianie",
              czas_typ == "time2" & student == "Maciek" ~ "M. wstawanie",
              czas_typ == "time1" & student == "Wojtek" ~ "W. zasypianie",
              czas_typ == "time2" & student == "Wojtek" ~ "W. wstawanie"
            ),
            student_label = case_when(
              student == "Maciek" ~ "Maciek",
              student == "Wojtek" ~ "Wojtek"
            )
          )
        
        p <- ggplot(plot_data, aes(x = date2, y = czas, color = legend_label, shape = legend_label)) +
          geom_point(size = 3) +
          labs(
            title = "Czas wstawania i zaśnięcia",
            x = "Dzień", 
            y = "Godzina",
            color = "Legenda",
            shape = "Legenda"
          ) +
          theme_minimal() +
          scale_color_manual(values = c(
            "M. zasypianie" = "purple",
            "M. wstawanie" = "purple",
            "W. zasypianie" = "yellow",
            "W. wstawanie" = "yellow"
          )) +
          scale_shape_manual(values = c(
            "M. zasypianie" = 17,
            "M. wstawanie" = 16,
            "W. zasypianie" = 17,
            "W. wstawanie" = 16
          )) +
          scale_x_date(breaks = "3 days", labels = scales::date_format("%d-%m-%Y")) +
          scale_y_datetime(
            expand = expansion(mult = c(0, 0.1)),
            date_labels = "%H:%M",
            date_breaks = "6 hours"
          ) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
            axis.text.y = element_text(color = "white"),
            axis.title.x = element_text(
              color = "white",
              margin = margin(t = 15)
            ),
            axis.title.y = element_text(color = "white"),
            plot.title = element_text(color = "white", face = "bold"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white"),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_blank(),
            strip.text.y = element_blank()
          ) +
          facet_grid(student_label ~ .)
        
      }
      
      if (!is.null(p)) {
        ggplotly(p) %>% 
          layout(plot_bgcolor = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
      }
    }
  })
  
  output$nauka_wykres <- renderPlotly({
    if ("Godzina rozpoczęcia nauki" %in% input$nauka_wybor) {
      filtered_data <- nauka %>%
        filter(student %in% input$nauka_osoba,
               date2 >= input$nauka_data[1],
               date2 <= input$nauka_data[2])
      
      if (nrow(filtered_data) == 0) {
        return(NULL)
      }
      
      p <- ggplot(filtered_data, aes(x = day.of.week, y = start.hour, fill = student)) +
        geom_violin(
          trim = TRUE,
          alpha = 0.6,
          color = "black",
          drop = FALSE
        ) +
        scale_fill_manual(
          values = c("Maciek" = "blue", "Wojtek" = "orange")
        ) +
        scale_y_continuous(
          breaks = seq(0, 24, by = 6),
          labels = seq(0, 24, by = 6)
        ) +
        labs(
          title = "Godziny rozpoczęcia nauki w poszczególnych dniach",
          x = "Dzień tygodnia",
          y = "Godzina rozpoczęcia",
          fill = "Student"
        ) +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "transparent", color = NA),  
          plot.background = element_rect(fill = "transparent", color = NA),   
          axis.text = element_text(color = "white"),                         
          axis.title = element_text(color = "white"),                         
          plot.title = element_text(size = 14, face = "bold", color = "white"), 
          legend.text = element_text(color = "white"),                        
          legend.title = element_text(color = "white"),                      
          panel.grid.major = element_line(color = "white"),                   
          panel.grid.minor = element_line(color = "white"),                   
          axis.text.x = element_text(angle = 45, hjust = 1, color = "white"), 
          legend.position = "bottom",                                          
          strip.text = element_text(size = 0, color = "transparent")
        ) +
        facet_wrap(~student, ncol = 1)
      
      
      ggplotly(p) %>%
        layout(plot_bgcolor = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
    } 
    
    
    
    else if ("Czas spędzony na projekty w danym przedmiocie" %in% input$nauka_wybor) {
      filtered_data <- projekty %>%
        filter(student %in% input$nauka_osoba,
               date2 >= input$nauka_data[1],
               date2 <= input$nauka_data[2],
               record.tags %in% input$nauka_przedmiot)
      

      if (nrow(filtered_data) == 0) {
        return(NULL) 
      }
      
      filtered_data <- filtered_data %>% 
        select(record.tags, duration.minutes, student) %>% 
        group_by(record.tags, student) %>% 
        summarise(duration.minutes = sum(duration.minutes))
      
      p2 <- ggplot(filtered_data, aes(x = record.tags, y = duration.minutes, fill = student)) +
        geom_bar(stat = "identity", position = "dodge") +  
        labs(title = "Łączny czas poświęcony na projekty",
             x = "Przedmiot", 
             y = "Czas trwania (minuty)", 
             fill = "Student") +
        scale_fill_manual(values = c("Maciek" = "violet", "Wojtek" = "green")) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "transparent", color = NA),  
          plot.background = element_rect(fill = "transparent", color = NA),   
          axis.text = element_text(color = "white"),                         
          axis.title = element_text(color = "white"),                         
          plot.title = element_text(size = 14, face = "bold", color = "white"), 
          legend.text = element_text(color = "white"),                        
          legend.title = element_text(color = "white"),                      
          panel.grid.major = element_line(color = "white"),                   
          panel.grid.minor = element_line(color = "white"),                   
          axis.text.x = element_text(angle = 45, hjust = 1, color = "white"), 
          legend.position = "bottom"                                          
        )
      
      ggplotly(p2) %>%
        layout(plot_bgcolor = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
    }
    
    else if ("Czas nauki na kolokwia" %in% input$nauka_wybor) {
      filtered_data <- kolosy %>%
        filter(student %in% input$nauka_osoba,
               date2 >= input$nauka_data[1],
               date2 <= input$nauka_data[2],
               record.tags %in% input$nauka_przedmiot)
      
      
      if (nrow(filtered_data) == 0) {
        return(NULL) 
      }
      
      filtered_data <- filtered_data %>% 
        select(record.tags, duration.minutes, student) %>% 
        group_by(record.tags, student) %>% 
        summarise(duration.minutes = sum(duration.minutes))
      
      p2 <- ggplot(filtered_data, aes(x = record.tags, y = duration.minutes, fill = student)) +
        geom_bar(stat = "identity", position = "dodge") +  
        labs(title = "Łączny czas nauki na kolokwia",
             x = "Przedmiot", 
             y = "Czas trwania (minuty)", 
             fill = "Student") +
        scale_fill_manual(values = c("Maciek" = "purple", "Wojtek" = "yellow")) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "transparent", color = NA),  
          plot.background = element_rect(fill = "transparent", color = NA),   
          axis.text = element_text(color = "white"),                         
          axis.title = element_text(color = "white"),                         
          plot.title = element_text(size = 14, face = "bold", color = "white"), 
          legend.text = element_text(color = "white"),                        
          legend.title = element_text(color = "white"),                      
          panel.grid.major = element_line(color = "white"),                   
          panel.grid.minor = element_line(color = "white"),                   
          axis.text.x = element_text(angle = 45, hjust = 1, color = "white"), 
          legend.position = "bottom"                                          
        )
      
      ggplotly(p2) %>%
        layout(plot_bgcolor = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
    }
    
    else if("Średni czas nauki przy pełnym i niedobrym śnie" %in% input$nauka_wybor) {
      filtered_data <- nauka_z_wyspaniem %>%
        filter(student %in% input$nauka_osoba,
               date2 >= input$nauka_data[1],
               date2 <= input$nauka_data[2]) %>% 
        group_by(day.of.week, student, stan) %>%
        summarise(mean_time_avg = mean(mean.time), .groups = 'drop')
      
      if (nrow(filtered_data) == 0) {
        return(NULL) 
      }

      p3 <- ggplot(filtered_data, aes(x = day.of.week, y = mean_time_avg, fill = stan, group = stan)) +
        geom_col(position = position_dodge(width = 1)) +
        labs(
          title = "Średni czas nauki w zależności od stanu wyspania",
          x = "Dzień tygodnia",
          y = "Czas nauki (minuty)",
          color = "Stan"
        ) +
        scale_fill_manual(values = c("Niewyspany" = "blue", "Wyspany" = "red")) +
        scale_y_continuous(
          expand = c(0, 0)
        ) +
        facet_wrap(~student, scales = "free_y", ncol = 1) +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "transparent", color = NA),  
          plot.background = element_rect(fill = "transparent", color = NA),   
          axis.text = element_text(color = "white"),                         
          axis.title = element_text(color = "white"),                         
          plot.title = element_text(size = 14, face = "bold", color = "white"), 
          legend.text = element_text(color = "white"),                        
          legend.title = element_text(color = "white"),                      
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),  
          panel.grid.minor.x = element_blank(),                    
          axis.text.x = element_text(angle = 45, hjust = 1, color = "white"), 
          legend.position = "bottom",                                          
          strip.text = element_text(size = 0, color = "transparent")
        ) 
      
      
      ggplotly(p3) %>%
        layout(plot_bgcolor = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
    }
    
  })
  
  output$reszta_wykres <- renderPlotly({
    if ("Średni czas na wydziale" %in% input$reszta_wybierz) {
      filtered_data <- mini %>%
        filter(student %in% input$reszta_osoba,
               date2 >= input$reszta_data[1],
               date2 <= input$reszta_data[2])
      
      if (nrow(filtered_data) == 0) {
        return(NULL) 
      }
      
      filtered_data <- filtered_data %>% 
        group_by(student, day.of.week) %>% 
        summarise(mean.time = mean(duration.minutes), .groups = 'drop') %>% 
        mutate(day.of.week = factor(day.of.week, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")))
      
      p <- ggplot(filtered_data, aes(x = day.of.week, y = mean.time, fill = student)) +
        geom_col(position = position_dodge(width = 0.9)) +
        labs(
          title = "Średni czas spędzony na wydziale",
          x = "Dzień tygodnia",
          y = "Średni spędzony czas",
          fill = "Student"
        ) +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "transparent", color = NA),  
          plot.background = element_rect(fill = "transparent", color = NA),   
          axis.text = element_text(color = "white"),                         
          axis.title = element_text(color = "white"),                         
          plot.title = element_text(size = 14, face = "bold", color = "white"), 
          legend.text = element_text(color = "white"),                        
          legend.title = element_text(color = "white"),                      
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),  
          panel.grid.minor.x = element_blank(),                 
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        scale_fill_manual(values = c("Maciek" = "blue", "Wojtek" = "orange"))
      
      ggplotly(p) %>%
        layout(plot_bgcolor = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
    }
    else if('Średni czas wolny' %in% input$reszta_wybierz){
      filtered_data <- wolne %>% 
        filter(student %in% input$reszta_osoba,
               date1 >= input$reszta_data[1],
               date1 <= input$reszta_data[2])
      
      if (nrow(filtered_data) == 0) {
        return(NULL) 
      }
      
      filtered_data <- filtered_data %>% 
        group_by(student, date1, day.of.week) %>% 
        summarise(oc.time = sum(duration.minutes)) %>% 
        ungroup() %>% 
        mutate(fr.time = 24*60 - oc.time) %>% 
        group_by(student, day.of.week) %>% 
        summarise(avg.fr.time = mean(fr.time)) %>% 
        ungroup() %>% 
        mutate(day.of.week = factor(day.of.week, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")))

        p <- ggplot(filtered_data, aes(x = day.of.week, y = avg.fr.time, fill = student)) +
          geom_col(position = position_dodge(width = 0.9)) + 
          labs(
            title = "Średni czas wolny każdego dnia",
            x = "Dzień tygodnia",
            y = "Średni czas wolny (minuty)",
            fill = "Student"
          ) +
          theme_minimal() +
          theme(
            panel.background = element_rect(fill = "transparent", color = NA),  
            plot.background = element_rect(fill = "transparent", color = NA),   
            axis.text = element_text(color = "white"),                         
            axis.title = element_text(color = "white"),                         
            plot.title = element_text(size = 14, face = "bold", color = "white"), 
            legend.text = element_text(color = "white"),                        
            legend.title = element_text(color = "white"),                      
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),  
            panel.grid.minor.x = element_blank(),                  
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          scale_y_continuous(
            expand = expansion(mult = c(0, 0)) 
          ) +
          scale_fill_manual(values = c("Maciek" = "purple", "Wojtek" = "yellow"))
        
        ggplotly(p) %>%
          layout(plot_bgcolor = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
    }
  })
}

shinyApp(ui = ui, server = server)



