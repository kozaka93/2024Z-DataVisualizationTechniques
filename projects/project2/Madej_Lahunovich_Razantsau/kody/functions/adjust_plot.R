adjust_plot <- function(plot){
  plot +
    theme_minimal() +
    theme(
      # axes and plot title
      axis.title = element_text(color = "white", size = 15, face = "bold"),
      axis.text = element_text(color = "white", size=8),
      plot.title = element_text(colour = "white", size=20, face="bold", hjust = 0.5),
      
      # background
      plot.background = element_rect("#3d3a36"),
      
      #legend
      legend.text = element_text(color="white", size=10),
      legend.title = element_blank(),
      
      # grid
      panel.grid.major = element_line(color = "#5b5c55", linetype = "dashed"),
      panel.grid.minor = element_line(color = "#4f4f4c", linetype = "dotted")
    ) +
    scale_fill_manual(values = c(
      "Ludwik"="#3de60e",
      "Yahor"="#0edfe6",
      "Maxim"="#dbf20a")
      ) +
    scale_color_manual(values = c(
      "Ludwik"="#3de60e",
      "Yahor"="#0edfe6",
      "Maxim"="#dbf20a"))
}