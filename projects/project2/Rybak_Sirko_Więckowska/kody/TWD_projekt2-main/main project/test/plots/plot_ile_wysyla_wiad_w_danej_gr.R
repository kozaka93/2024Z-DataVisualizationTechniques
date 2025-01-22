library(dplyr)
library(ggplot2)

source(file.path("functions", "ile_wysylamy_wiad_w_danej_gr.R"))



plot_ile_wysyla_wiad_w_danej_gr <- function(wiadom_rodzina, wiadom_przyj, wiadomosci_kolega_studia, wiadom_kol_poza_stud, osoba){
  
  data_to_plot <- liczba_wiadomosci_w_danej_gr(
    wiadom_rodzina,
    wiadom_przyj,
    wiadomosci_kolega_studia,
    wiadom_kol_poza_stud,
    osoba
  )
  
  plot <- data_to_plot %>%
    ggplot(aes(x = grupa, y = liczba_wiadomosci, fill = grupa)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#b93cdc", "#b93cdc", "#b93cdc", "#b93cdc")) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    labs(
      title = paste("Number of Messages Sent to a Group"),
      x = "Group",
      y = "Number of Messages"
    ) +
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