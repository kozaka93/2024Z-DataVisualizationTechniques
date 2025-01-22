source(file.path("functions", "table_from_relative_path.R"))
source(file.path("functions", "table_merge_from_filepath.R"))

#TODO: might be able to source whole functions folder

source(file.path("plots", "plot_message_length_per_hour.R"))
source(file.path("plots", "plot_longest_xd.R"))
source(file.path("plots", "plot_emote_number.R"))
source(file.path("plots", "plot_longest_message.R"))
source(file.path("plots", "plot_message_number_between_0_and_6.R"))
source(file.path("plots", "wykres_kto_szybciej_odpisuje.R"))
source(file.path("plots", "wykres_ile_procentowo_udzielamy_sie_w_gr.R"))
source(file.path("plots", "plot_najczestsze_slowa.R"))
source(file.path("plots", "plot_ile_wysyla_wiad_w_danej_gr.R"))

source(file.path("plots", "plotter_for_individual_stats.R"))

### DATA ###
#to sa dane przykladowe pozniej trzeba bedzie usunac
tabela_kuba <- table_from_relative_path(file.path("messages", "message_1.json"))
tabela_ola <- table_from_relative_path(file.path("messages", "message_ola.json"))
tabela_krzysztof <- tabela_kuba


kuba <- "Kuba Rybak"
lila <- "Liliana Sirko"



who <- kuba
my_data <- tabela_kuba
######

##### TO SA DANE JUZ GOTOWE POZNIEJ TRZEBA BEDZIE JE DAC DO FUNKCJI ######
kuba <- "Kuba Rybak"

kuba_rodzina <- table_merger(file.path("messages", "kuba_rodzina.json"))
kuba_kolega_ze_studiow <- table_merger(file.path("messages", "kuba_kolega_ze_studiow.json"))
kuba_kolega_spoza_studiow <- table_merger(file.path("messages", "kuba_kolega_spoza_studiow.json"))
kuba_przyjaciel <- table_merger(c(file.path("messages", "kuba_przyjaciel_1.json"), file.path("messages", "kuba_przyjaciel_2.json"), file.path("messages", "kuba_przyjaciel_3.json")))
kuba_grupy <- table_merger(c(file.path("messages", "kuba_grupy_2.json"),file.path("messages", "kuba_grupy_1.json")))

kamila <- "Kamila Więckowska"

kamila_rodzina <- table_merger(file.path("messages", "kamila_rodzina.json"))
kamila_kolega_ze_studiow <- table_merger(c(file.path("messages", "kamila_kolega_ze_studiow_1.json"), file.path("messages", "kamila_kolega_ze_studiow_2.json")))
kamila_kolega_spoza_studiow <- table_merger(file.path("messages", "kamila_kolega_spoza_studiow.json"))
kamila_przyjaciel <- table_merger(c(file.path("messages", "kamila_przyjaciel_1.json"), file.path("messages", "kamila_przyjaciel_2.json"), file.path("messages", "kuba_przyjaciel_3.json")))
kamila_grupy <- table_merger(file.path("messages", "kamila_grupa.json"))


wszyscy <- rbind(kuba_rodzina, kuba_kolega_spoza_studiow, kuba_kolega_ze_studiow, kuba_przyjaciel)
wszyscy2 <- wszyscy %>% 
  mutate(date_of_message = as.POSIXct(date_of_message)) %>% 
  filter(month(date_of_message) %in% c(1,11,3,5)) %>% 
  mutate(date_of_message = case_when(
    month(date_of_message) == 1 ~ date_of_message + months(6),
    month(date_of_message) == 11 ~ date_of_message + months(9),
    month(date_of_message) == 3 ~ date_of_message + months(6),
    month(date_of_message) == 5 ~ date_of_message + months(5)
  )) %>% 
  arrange(date_of_message)

kuba_wszystkie <- rbind(wszyscy, wszyscy2)


wszyscy <- rbind(kamila_rodzina, kamila_kolega_spoza_studiow, kamila_kolega_ze_studiow, kamila_przyjaciel)
wszyscy2 <- wszyscy %>% 
  mutate(date_of_message = as.POSIXct(date_of_message)) %>% 
  filter(month(date_of_message) %in% c(1,11,3,5)) %>% 
  mutate(date_of_message = case_when(
    month(date_of_message) == 1 ~ date_of_message + months(6),
    month(date_of_message) == 11 ~ date_of_message + months(9),
    month(date_of_message) == 3 ~ date_of_message + months(6),
    month(date_of_message) == 5 ~ date_of_message + months(5)
  )) %>% 
  arrange(date_of_message)
kamila_wszystkie <- rbind(wszyscy, wszyscy2)


lila_rodzina <- table_merger(c(file.path("messages", "lila_rodzina.json")))
lila_kolega_ze_studiow <- table_merger(c(file.path("messages", "lila_kolega_ze_studiow.json")))
lila_kolega_spoza_studiow <- table_merger(c(file.path("messages", "lila_kolega_spoza_studiow.json"), file.path("messages", "lila_kolega_spoza_studiow2.json")))
lila_przyjaciel <- table_merger(c(file.path("messages", "lila_przyjaciel.json"), file.path("messages", "lila_przyjaciel2.json")))
lila_grupy <- table_merger(c(file.path("messages", "lila_grupy2.json"),file.path("messages", "lila_grupy1.json"), file.path("messages", "lila_grupy3.json"), file.path("messages", "lila_grupy4.json"), file.path("messages", "lila_grupy5.json")))

wszyscy <- rbind(lila_rodzina, lila_kolega_spoza_studiow, lila_kolega_ze_studiow, lila_przyjaciel)
wszyscy2 <- wszyscy %>%
  mutate(date_of_message = as.POSIXct(date_of_message)) %>%
  filter(month(date_of_message) %in% c(1,11,3,5)) %>%
  mutate(date_of_message = case_when(
    month(date_of_message) == 1 ~ date_of_message + months(6),
    month(date_of_message) == 11 ~ date_of_message + months(9),
    month(date_of_message) == 3 ~ date_of_message + months(6),
    month(date_of_message) == 5 ~ date_of_message + months(5)
  )) %>%
  arrange(date_of_message)

lila_wszystkie <- rbind(wszyscy, wszyscy2)



#####

### SERVER FUNCTION ###
function(input, output, session) {
  ### PLOTS FOR INDIVIDUAL STATS ###############################################
  
  output$plot7 <- renderPlot({
    if(input$name == "Liliana Sirko"){
      plot_message_count_by_dow(data = lila_wszystkie,
                                selected_options = input$filter_option,
                                who = input$name)
    }
    else if(input$name == "Kuba Rybak"){
      plot_message_count_by_dow(data = kuba_wszystkie,
                                selected_options = input$filter_option,
                                who = input$name)
    }
    else {
      plot_message_count_by_dow(data = kamila_wszystkie,
                                selected_options = input$filter_option,
                                who = input$name)
    }

  }, bg = "transparent")

  output$plot8 <- renderPlot({
    if(input$name == "Liliana Sirko"){
      plot_najczestsze_slowa(osoba = input$name,
                             tabela = lila_wszystkie)
    }
    else if(input$name == "Kuba Rybak"){
      plot_najczestsze_slowa(osoba = input$name,
                             tabela = kuba_wszystkie)
    }
    else{
      plot_najczestsze_slowa(osoba = input$name,
                             tabela = kamila_wszystkie)
    }
  }, bg = "transparent")
  
  
  output$plot9 <- renderPlot({
    if(input$name == "Liliana Sirko"){
      plot_message_length_per_hour(data = lila_wszystkie,
                                   who = input$name)
    }
    else if(input$name == "Kuba Rybak"){
      plot_message_length_per_hour(data = kuba_wszystkie,
                                   who = input$name)
    }
    else{
      plot_message_length_per_hour(data = kamila_wszystkie,
                                   who = input$name)
    }
    
  }, bg = "transparent")
  
  output$plot10 <- renderPlot({   # na razie typy grup kuby
    if (input$name == "Liliana Sirko"){
      plot_ile_wysyla_wiad_w_danej_gr(wiadom_rodzina = lila_rodzina,
                                      wiadom_przyj = lila_przyjaciel,
                                      wiadomosci_kolega_studia = lila_kolega_ze_studiow,
                                      wiadom_kol_poza_stud = lila_kolega_spoza_studiow,
                                      osoba = input$name)
    }
    else if(input$name == "Kuba Rybak"){
    plot_ile_wysyla_wiad_w_danej_gr(wiadom_rodzina = kuba_rodzina,
                                    wiadom_przyj = kuba_przyjaciel,
                                    wiadomosci_kolega_studia = kuba_kolega_ze_studiow,
                                    wiadom_kol_poza_stud = kuba_kolega_spoza_studiow,
                                 osoba = input$name)
    }
    else{
      plot_ile_wysyla_wiad_w_danej_gr(wiadom_rodzina = kamila_rodzina,
                                      wiadom_przyj = kamila_przyjaciel,
                                      wiadomosci_kolega_studia = kamila_kolega_ze_studiow,
                                      wiadom_kol_poza_stud = kamila_kolega_spoza_studiow,
                                      osoba = input$name)
    }
  }, bg = "transparent")
  
  
  ###PLOTS FOR RANKING #########################################################
  ### PLOT DEFINITION ###
  
  output$plot6 <- renderPlot({
    plot_longest_xd(kuba_wszystkie, kuba, lila_wszystkie, lila, kamila_wszystkie, kamila)
  }, bg = "transparent")
  output$plot2 <- renderPlot({
    wykres_kto_szybciej_odpisuje(kuba_wszystkie, kuba, lila_wszystkie, lila, kamila_wszystkie, kamila)
  }, bg = "transparent")
  output$plot3 <- renderPlot({
    plot_emote_number(kuba_wszystkie, kuba, lila_wszystkie, lila, kamila_wszystkie, kamila)
  }, bg = "transparent")
  output$plot4 <- renderPlot({
    plot_longest_message(kuba_wszystkie, kuba, lila_wszystkie, lila, kamila_wszystkie, kamila)
  }, bg = "transparent")
  output$plot5 <- renderPlot({
    wykres_ile_procentowo_udzielamy_sie_w_gr(kuba_wszystkie, kuba, lila_wszystkie, lila, kamila_wszystkie, kamila)
  }, bg = "transparent")
  output$plot1 <- renderPlot({
    plot_message_number_between_0_and_6(kuba_wszystkie, kuba, lila_wszystkie, lila, kamila_wszystkie, kamila)
  }, bg = "transparent")
  
  
  
  ###########################################
  
  
  
  ### PLOTTING FUNCTION (for cubes) ###
  licznik_zeby_skonczyc <- rep(0, 6)
  
  
  funkcja_do_toggle <- function(ktory_pokazac) {
    id_ktory_pokazac <- switch(
      ktory_pokazac,
      "first_plot",
      "second_plot",
      "third_plot",
      "fourth_plot",
      "fifth_plot",
      "sixth_plot"
    )
    
    ktory_container <- switch(
      ktory_pokazac,
      "#cube-container-1",
      "#cube-container-2",
      "#cube-container-3",
      "#cube-container-4",
      "#cube-container-5",
      "#cube-container-6"
    )
    
    if (licznik_zeby_skonczyc[ktory_pokazac] >= 1) {
      return()
    }
    
    licznik_zeby_skonczyc[ktory_pokazac] <<- 2
    
    shinyjs::delay(6700, {
      shinyjs::toggle(id = id_ktory_pokazac, anim = TRUE, time = 1.2)
      
      css_code3 <-
        paste0(ktory_container,
               ' { cursor: default; pointer-events: none; }')
      runjs(paste0('$("head").append("<style>', css_code3, '</style>");'))
      
      css_code2 <-
        paste0(
          '<style>',
          ktory_container,
          ':hover { transform: scale(1); z-index: 1000; position: relative; }</style>'
        )
      runjs(paste0('$("head").append("', css_code2, '");'))
      
      css_code <-
        paste0(
          '<style>',
          ktory_container,
          ':hover .face {background: rgba(243, 156, 18, 0.8);}</style>'
        )
      runjs(paste0('$("head").append("', css_code, '");'))
    })
  } #end of plotting function
  
  ### CUBE ANIMATIONS ###
  shinyjs::onclick("first_cube",
                   funkcja_do_toggle(1))
  shinyjs::onclick("second_cube",
                   funkcja_do_toggle(2))
  shinyjs::onclick("third_cube",
                   funkcja_do_toggle(3))
  shinyjs::onclick("fourth_cube",
                   funkcja_do_toggle(4))
  shinyjs::onclick("fifth_cube",
                   funkcja_do_toggle(5))
  shinyjs::onclick("sixth_cube",
                   funkcja_do_toggle(6))
  
  ### CATEGORY TITLES ###
  output$text1 <- renderText({
    "🌙 Biggest night owl"
  })
  output$text2 <- renderText({
    "🐌 Who replies the slowest?"
  })
  output$text3 <- renderText({
    "❤️ Who receives the most reactions?"
  })
  output$text4 <- renderText({
    "📜 Who wrote the longest message?"
  })
  output$text5 <- renderText({
    "👥 Who writes the most in groupchats?"
  })
  
  output$text6 <- renderText({
    "🤣 Longest xdddddddd"
  })
}
