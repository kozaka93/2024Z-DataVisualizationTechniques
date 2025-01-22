# 
# zwraca wykres slupkowy porownojacy 3 osoby
# 
# argumenty:
#   table1 - tabela, who1 - kto jest w tabeli1
#   ...

library(ggplot2)
library(dplyr)
library(stringr)

source(file.path("functions", "message_number_between_0_and_6.R"))

plot_message_number_between_0_and_6 <- function(table1, who1, table2, who2, table3, who3){
  data_to_plot <- rbind(message_number_between_0_and_6(table1, who1), message_number_between_0_and_6(table2, who2), message_number_between_0_and_6(table3, who3))
  data_to_plot <- data_to_plot %>% 
    arrange(-message_number)
  data_to_plot$who_sent <- factor(data_to_plot$who_sent,
                                  levels = c(data_to_plot$who_sent[1],
                                             data_to_plot$who_sent[2],
                                             data_to_plot$who_sent[3]))
  plot <- data_to_plot %>% 
    rename(Messenger = who_sent) %>% 
    mutate(Messenger = word(`Messenger`, 1)) %>% 
    ggplot(aes(x = Messenger, y = message_number, fill = Messenger)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("#0797fe", 
                               "#b93cdc", 
                               "#ff547e")) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    labs(x = " ",
         y = "Number of messages sent between 0-6") +
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