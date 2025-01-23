

karol<-read.csv("data_k.csv",sep=",")
mati<-read.csv("data_j.csv",sep=",")
brandt<-read.csv("data_b.csv",sep=",")

library(dplyr)
library(ggplot2)



ggplot(data=karol,aes(x=battleCount,y=trophies))+
  geom_point()
ggplot(data=mati,aes(x=battleCount,y=trophies))+
  geom_point()
ggplot(data=brandt,aes(x=battleCount,y=trophies))+
  geom_point()

ggplot()+
  geom_line(data = karol, ,mapping=aes(x=battleCount,y=trophies,color="red"))+
  geom_line(data = mati, ,mapping=aes(x=battleCount,y=trophies,color="blue"))+
  geom_line(data = brandt, ,mapping=aes(x=battleCount,y=trophies,color="green"))


ggplot()+
  geom_line(data = karol, ,mapping=aes(x=battleCount,y=wins/battleCount,color="red"))+
  geom_line(data = mati, ,mapping=aes(x=battleCount,y=wins/battleCount,color="blue"))+
  geom_line(data = brandt, ,mapping=aes(x=battleCount,y=wins/battleCount,color="green"))

ggplot()+
  geom_point(data = karol, ,mapping=aes(x=date,y=battleCount,color="red"))+
  geom_point(data = mati, ,mapping=aes(x=date,y=battleCount,color="blue"))+
  geom_point(data = brandt, ,mapping=aes(x=date,y=battleCount,color="green"))

