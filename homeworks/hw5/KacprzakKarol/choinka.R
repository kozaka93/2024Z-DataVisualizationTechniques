library(ggplot2)
library(dplyr)
{
a<-c()
b<-c()

for(i in seq(1,10,by=0.33)){
    for(j in seq(1,i,by=1)){
      a<-c(a,c(j))
      b<-c(b,c(i))
    }
}

for(i in seq(1,5,by=1)){
  a<- c(a,a+runif(length(a))*0.8)
  b<- c(b,b + runif(length(b))*0.8)
}

a<-c(a,a)+2.1
b<-c(b,-b+20)




df<-data.frame(c(b-10,0,rep(seq(-1,1,by=0.2),20))+0.01,c(a,2,rep(seq(1, 2.9, by = 0.1), each = 11))+0.01)
colnames(df)<-c("x","y")
df<-df %>% 
  mutate(color=case_when(
    y<=3 ~"brown",
     y> abs(x)+9.8 | abs(x+2)+10<y | abs(x-2)+10<y ~"yellow",
     y^3 < round(y^3) & y^3 >round(y^3)-0.03~ "red",
     y^2 >round(y^2) & y^2 <round(y^2)+0.03~ "blue",
     .default = "green"
  ),
  shape=case_when(
    color=="red" | color =="blue" ~16,
    TRUE ~ 17
  )) %>% 
  arrange(-shape)

ggplot(data=df,aes(x=x,y=y, color=as.factor(color)))+
  geom_point(size=df$shape-14,shape=df$shape)+
  theme_minimal()+
  ggtitle("Wesołych Świąt")+
  labs(color = "Kolor")+
  scale_color_manual(
    values = c(
      "brown" = "brown",
      "yellow" = "yellow",
      "red" = "red",
      "blue" = "blue",
      "green" = "green"),
    guide="none")
}

