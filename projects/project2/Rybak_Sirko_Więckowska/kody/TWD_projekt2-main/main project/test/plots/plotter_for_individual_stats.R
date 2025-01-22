library(ggplot2)
library(showtext)
library(sysfonts)

source(file.path("functions", "message_count_by_dow.R"))
source(file.path("functions", "ile_wysylamy_wiad_w_danej_gr.R"))
source(file.path("functions", "most_frequent_words_for_months_in_quarter.R"))
source(file.path("functions", "message_length_per_hour.R"))
source(file.path("functions", "najczestsze_slowa.R"))

font_add_google("Space Mono", "space mono")
showtext_auto()

### FIRST PLOT ###
plot_message_count_by_dow <- function(data, selected_options, who) {
 #input for selected_options: "TRUE", "FALSE", "total"
  filtered_data <- message_count_by_dow(data, who)
  
  if (!("total" %in% selected_options)) {
    #checks is_holiday value, leaves entries that allign with checked option
    filtered_data <- filtered_data %>%
      filter(as.character(is_holidays) %in% selected_options)
  } else if (!all(c("TRUE", "FALSE") %in% selected_options)) {
    #combine data into total counts if "total" is selected but not both TRUE and FALSE
    filtered_data <- filtered_data %>%
      group_by(day_of_week) %>%
      summarise(count = sum(count)) %>%
      mutate(is_holidays = "total")
  }
  
  #make plot
  ggplot(filtered_data, aes(x = day_of_week, y = count, fill = as.factor(is_holidays))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Message Counts for Each Day of the Week",
      x = "Day of the Week",
      y = "Count"
    ) +
    theme_minimal() +
    scale_fill_manual(values = "#0797fe") +
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
  
  
}



### SECOND PLOT ###
#most_frequent_words_for_months_in_quarter(data, which_quarter)
#!!! why without who? -- add in original function
#app input: quarter -> radio button


####################### nowy ########################

# plot_najczestsze_slowa <- function(tabela, osoba){
#   
#   data_to_plot <- najczestsze_slowa(tabela, osoba)
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

##############################################################



### THIRD PLOT ###
#don't do selection, multiple lines for word, message, sentence
#TODO: add missing lines, functions for missing lines.

plot_average_length_by_hour <- function(data, who){
  avg_message_len <- message_length_per_hour(data, who)
  avg_message_len <- avg_message_len %>% 
    group_by(hour) %>% 
    mutate(avg_length = mean(message_length, NA.rm = TRUE)) %>% 
    mutate(avg_length = ifelse(is.na(avg_length), 0, avg_length))
  
    ggplot(avg_message_len, aes(x=as.factor(hour), y=avg_length, group=1)) + 
      geom_line(color="#ff547e", size = 2) + 
      geom_point(color="#ff547e", size = 3) +
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
  
}