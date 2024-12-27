library(dplyr)
library(plotly)

data <- data.frame(
  x = c(
    seq(-4, 4, length.out = 100),
    seq(-3.75, 3.75, length.out = 100),
    seq(-3.5,3.5,length.out=100),
    seq(-3.25,3.25,length.out=100),
    seq(-3,3,length.out=100),
    seq(-2.75,2.75,length.out=100),
    seq(-2.5,2.5,length.out=100),
    seq(-2.25,2.25,length.out=100),
    seq(-2,2,length.out=100),
    seq(-1.75,1.75,length.out=100),
    seq(-3, 3, length.out = 100),
    seq(-2.75,2.75,length.out=100),
    seq(-2.5,2.5,length.out=100),
    seq(-2.25,2.25,length.out=100),
    seq(-2,2,length.out=100),
    seq(-1.75,1.75,length.out=100),
    seq(-1.5,1.5,length.out=100),
    seq(-1.25,1.25,length.out=100),
    seq(-1,1,length.out=100),
    seq(-0.75,0.75,length.out=100),
    seq(-2, 2, length.out = 100),
    seq(-1.8,1.8,length.out=100),
    seq(-1.6,1.6,length.out=100),
    seq(-1.4,1.4,length.out=100),
    seq(-1.2,1.2,length.out=100),
    seq(-1,1,length.out=100),
    seq(-0.8,0.8,length.out=100),
    seq(-0.6,0.6,length.out=100),
    seq(-0.4,0.4,length.out=100),
    seq(-0.2,0.2,length.out=100),
    seq(-1, 1, length.out = 100),
    seq(-0.8,0.8,length.out=100),
    seq(-0.6,0.6,length.out=100),
    seq(-0.4,0.4,length.out=100),
    seq(-0.2,0.2,length.out=100),
    seq(-0.1,0.1,length.out=100),
    0,
    rep(seq(-0.4, 0.4, length.out = 10), each = 10)
  ),
  y = c(
    rep(1, 100),
    rep(1.1,100),
    rep(1.2,100),
    rep(1.3,100),
    rep(1.4,100),
    rep(1.5,100),
    rep(1.6,100),
    rep(1.7,100),
    rep(1.8,100),
    rep(1.9,100),
    rep(2, 100),
    rep(2.1,100),
    rep(2.2,100),
    rep(2.3,100),
    rep(2.4,100),
    rep(2.5,100),
    rep(2.6,100),
    rep(2.7,100),
    rep(2.8,100),
    rep(2.9,100),
    rep(3, 100),
    rep(3.1,100),
    rep(3.2,100),
    rep(3.3,100),
    rep(3.4,100),
    rep(3.5,100),
    rep(3.6,100),
    rep(3.7,100),
    rep(3.8,100),
    rep(3.9,100),
    rep(4, 100),
    rep(4.1,100),
    rep(4.2,100),
    rep(4.3,100),
    rep(4.4,100),
    rep(4.5,100),
    4.6,
    rep(seq(0.5, 0.9, length.out = 10), times = 10)
  ),
  color = c(
    rep("green", 3601),
    rep("brown", 100)
  )
)

snieg<- data.frame(
  x=c(-4,-3,-2,0.75,4,2.25,3.5,-3.75,-4.25,-2.75,-4,-1.5,2,4.4,3.5,0.75,3,
      rep(seq(-5,5,length.out=100),each=60)),
  y=c(4,3,4.5,4.75,4.25,3.75,3,4.9,2.5,1.75,1.25,0.75,2.75,2.1,1.5,0.8,0.6,
      rep(seq(0,0.5,length.out=100),time=60))
)
bombki <- data.frame(
  x=c(-2.5,-1,1,2,-1,1.2,0.7,1.2,-1.5,-0.2,0.1),
  y=c(1.3,1.8,1,1.6,2.5,2.2,2.8,3.3,3.2,3.7,4.2),
  color=c('red','orange','purple','magenta','purple','red','orange','magenta','red','purple','orange')
)
gwiazda <- data.frame(
  x=c(0),
  y=c(4.6)
)
lancuchy <- data.frame(
  x=c(seq(-2.25,0,length.out=20),seq(-1.75,1.5,length.out=25),seq(-1,1.2,length.out=20),seq(2,3,length.out=10)),
  y=c(seq(1.7,1,length.out=20),seq(2,2.6,length.out=25),seq(3.5,3,length.out=20),seq(1,1.4,length.out=10))
)

fig <- plot_ly() %>% 
  add_trace(data=data,
            x = ~x,
            y = ~y,
            type = 'scatter',
            mode = 'markers',
            name='drzewko',
            marker = list(size = 12, color = ~color))%>%
  add_trace(data=snieg,
            x=~x,
            y=~y,
            type = 'scatter',
            mode = 'markers',
            name="śnieg",
            marker = list(size = 12,color='white',symbol='star-diamond')) %>% 
  add_trace(data=bombki,
            x=~x,
            y=~y,
            type='scatter',
            mode='markers',
            name='bombki',
            marker=list(size=25,color=~color)) %>% 
  add_trace(data=gwiazda,
            x=~x,
            y=~y,
            type='scatter',
            mode='markers',
            name='gwiazda',
            marker=list(size=40,color='yellow',symbol='star')) %>% 
  add_trace(data=lancuchy,
            x=~x,
            y=~y,
            type='scatter',
            mode='markers',
            name='lancuchy',
            marker=list(size=15,color='lightblue',symbol='star-triangle-up')) %>% 
  layout(xaxis = list(title = '', showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,range=c(-4.5,4.5)),
         yaxis = list(title = '', showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,range=c(0,5)),
         plot_bgcolor = 'navy',
         paper_bgcolor = 'navy',
        legend = list(title = list(text = 'Ubierz choinkę'), font = list(color = 'white')),
        width=600,height=800)
fig

