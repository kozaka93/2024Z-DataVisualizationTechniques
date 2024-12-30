
library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)
#Liscie
TreeData<-data_frame(
  x=NULL,
  y=NULL
)
height<-25
for (i in 1:height) {
      for(s in 1:(2*i-1)){
        newRow<-data_frame(x=0.1*((height-i)+s),y=0.1*((height-i)))
        TreeData<-rbind(TreeData,newRow)
      }
        
}

#Korzen
TrunkData<-data_frame(
  x=NULL,
  y=NULL
)

for(k in 1:2){
  for(j in 1:3){
    newRow<-data_frame(x=2.5+(0.1)*(-1)^(k),y=-0.1*(j))
    TrunkData<-rbind(TrunkData,newRow)
  }
}
#Lampki
x = c(0.5,1,1.5,2,2,2)
y = c(0.2,0.5,0.2,1.5,1,0.2)
z<-5-x
y<-c(rep(y,times=2),c(0.6,1.8))
x<-c(x,z,2.5,2.5)

LampData<-data_frame(
  x = x,
  y = y,
  group=rep(c(1,2),times=7),
  Lcolor=rep(c("A","B"),times=7)
)

#Gwiazdka
StarData<-data_frame(
  x=c(2.5),
  y=c(2.4)
)

p<-ggplot(data=LampData,aes(x=x,y=y,color=Lcolor))+
  geom_point(shape = 23, size = 6,color="darkgreen",fill='darkgreen',data=TreeData)+
  geom_point(shape=22,size=6,color="#501b1b",fill="#501b1b",data=TrunkData)+
  geom_point(shape=16,size=14)+
  geom_point(color="yellow",shape=8,size=14,data=StarData)+
  theme_void()+
  theme(panel.background = element_rect(fill = 'black'),
        legend.position = "none")+
  transition_states(states = group, transition_length = 0, state_length = 1)
p
anim_save("Choinka.gif", animation = p)

