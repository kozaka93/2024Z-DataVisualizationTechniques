library(dplyr)
library(ggplot2)


source(file.path("functions", "ile_odpisuje.R"))

wykres_kto_szybciej_odpisuje <- function(table1, who1, table2, who2, table3, who3){
  
  
  avg_response_time_1 <- ile_srednio_minut_odpisuje(table1, who1)$sredni_czas_odp
  avg_response_time_2 <- ile_srednio_minut_odpisuje(table2, who2)$sredni_czas_odp
  avg_response_time_3 <- ile_srednio_minut_odpisuje(table3, who3)$sredni_czas_odp

  data_to_plot <- tibble(
    who_sent = c(who1, who2, who3),
    Avg_Response_Time_Min = c(avg_response_time_1, avg_response_time_2, avg_response_time_3)
  )
  
  
  data_to_plot <- data_to_plot %>%
    arrange(desc(Avg_Response_Time_Min))
  
  data_to_plot$who_sent <- factor(data_to_plot$who_sent,
                                  levels = c(data_to_plot$who_sent[1],
                                             data_to_plot$who_sent[2],
                                             data_to_plot$who_sent[3]))
  
  plot <- data_to_plot %>%
    rename(Messenger = who_sent) %>% 
    mutate(Messenger = word(`Messenger`, 1)) %>%
    ggplot(aes(x = Messenger, y = Avg_Response_Time_Min, fill = Messenger)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#0797fe", 
                                 "#b93cdc", 
                                 "#ff547e")) +
    scale_y_continuous(expand = c(0, 0)) + 
    theme_minimal() +
    labs(x = " ",
         y = "Average Response Time (minutes)") +
    theme(
      plot.title = element_text(
        color = "white",
        size = 20,
        face = "bold",
        hjust = 0.5
      ),
      axis.title = element_text(color = "white", size = 18),
      axis.text = element_text(color = "white", size = 16),
      axis.text.x = element_text(angle = 30, hjust = 1),  # Rotate x-axis labels
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),  # Adjust margins
      text = element_text(family = "space mono"),  # Apply consistent font
      legend.position = "none"  # Remove the legend
    )
  
  return(plot)
  
}

