library(dplyr)
library(ggplot2)
library(plotly)

rotate_by_alfa <- function(df,alfa){
  df_temp = df
  df_temp$x = df$x * cos(alfa) 
  df_temp$y = df$x * sin(alfa)
  return(df_temp)
}

step = 0.05

df <- data.frame(x= 0.1,y = 0 , z = seq(0,0.5,step))


df <- rbind(df,data.frame(x = seq(0.1, 0.85, step), y = 0, z = 0.5))

h1 = 0.7
x_0 = 0.85
z_0 = 0.5
df <- rbind(df,data.frame( x = seq(x_0,x_0-h1,-step), y = 0, 
                           z = seq(z_0, z_0+h1, step) ))

x_0 = x_0-h1
z_0 = z_0+h1
df <- rbind(df, data.frame(x = seq(x_0,0.65,step), y = 0 , z = z_0))

h2 = 0.5
x_0 = 0.65
df <- rbind(df, data.frame(x = seq(x_0, x_0 - h2, -step), y = 0, 
                           z = seq(z_0, z_0 + h2, step)))

x_0 = x_0 - h2
z_0 = z_0 + h2
df <- rbind(df, data.frame(x = seq(x_0, 0.40,  step), y = 0 , z = z_0))

h3 = 0.4
x_0 = 0.4
df <- rbind(df, data.frame(x = seq(x_0, x_0 - h3, -step), y = 0, 
                           z = seq(z_0, z_0 + h3, step)))





alfa <- seq(0,2*pi,length.out = 100)
df_temp = data.frame()
for (a in alfa){
  df_temp = rbind(df_temp, rotate_by_alfa(df,a))
}
df_temp$colour ="green"


df_temp_2 <- df_temp
df_temp_2[sample(nrow(df_temp), 700),]$colour = "gold"
df_temp_2[sample(nrow(df_temp), 600),]$colour = "blue"
df_temp_2[sample(nrow(df_temp), 500),]$colour = "red"
df_temp_2[df_temp_2$z < 0.5,]$colour = "brown"
df_temp_2$colour = as.factor(df_temp_2$colour)

plot_ly(df_temp_2, x = ~x, y = ~y, z = ~z, type = "scatter3d",
        mode = "markers", color = ~colour,
        colors = c("green"="darkgreen", "red"="red", "blue"="blue",
                   "gold"="gold", "brown"="#521515"),
        marker = list(size = 15)) %>% layout(showlegend = F)



