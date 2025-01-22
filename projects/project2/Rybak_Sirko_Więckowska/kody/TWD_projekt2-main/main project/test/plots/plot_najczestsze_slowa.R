library(dplyr)
library(ggplot2)

source(file.path("functions", "najczestsze_slowa.R"))

## bez podzialu na kwartaly

font_add_google("Space Mono", "space mono")
showtext_auto()


plot_najczestsze_slowa <- function(tabela, osoba){
  
  data_to_plot <- najczestsze_slowa(tabela, osoba)
  
  plot <- data_to_plot %>%
    ggplot(aes(x = reorder(word, -number), y = number, fill = word)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#ff547e", "#ff547e", "#ff547e")) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    labs(
      title = paste("Most Common Words in Messages"),
      x = "Words",
      y = "Frequency"
    ) +
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


## z podzialem na kwartaly


# plot_najczestsze_slowa <- function(tabela, osoba, kwartal){
#   
#   data_to_plot <- najczestsze_slowa(tabela, osoba, kwartal)
#   
#   plot <- data_to_plot %>%
#     ggplot(aes(x = reorder(word, -number), y = number, fill = word)) +
#     geom_bar(stat = "identity") +
#     scale_fill_manual(values = c("#ff547e", "#ff547e", "#ff547e")) +
#     scale_y_continuous(expand = c(0, 0)) +
#     theme_minimal() +
#     labs(
#       title = paste("Most Common Words Used by", osoba),
#       x = "Words",
#       y = "Frequency"
#     ) +
#     theme(
#       plot.title = element_text(color = "white", size = 20, face = "bold"),
#       axis.title = element_text(color = "white", size = 18),
#       axis.text = element_text(color = "white", size = 16),
#       legend.position = "none",
#       axis.text.x = element_text(angle = 25, hjust = 1)
#     )
#   
#   return(plot)
# }
