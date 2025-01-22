library(ggplot2)
library(dplyr)
library(stringr)

source(file.path("functions", "ile_procentowo_udziala_sie_w_gr.R"))


wykres_ile_procentowo_udzielamy_sie_w_gr <- function(table1, who1, table2, who2, table3, who3) {
  data_to_plot <- tibble(
    who_sent = c(who1, who2, who3),
    Percent = c(
      procentowy_udzial_wiadomosci(table1, who1),
      procentowy_udzial_wiadomosci(table2, who2),
      procentowy_udzial_wiadomosci(table3, who3)
    )
  )
  
  data_to_plot <- data_to_plot %>%
    arrange(desc(Percent))
  
  data_to_plot$who_sent <- factor(data_to_plot$who_sent,
                                  levels = c(data_to_plot$who_sent[1],
                                             data_to_plot$who_sent[2],
                                             data_to_plot$who_sent[3]))
  
  plot <- data_to_plot %>%
    rename(Messenger = who_sent) %>% 
    mutate(Messenger = word(`Messenger`, 1)) %>%
    ggplot(aes(x = Messenger, y = Percent, fill = Messenger)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#0797fe", 
                                 "#b93cdc", 
                                 "#ff547e")) +
    theme_minimal() +
    labs(x = " ",
         y = "Percentage of Messages") +
    scale_y_continuous(expand = c(0, 0)) +
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



