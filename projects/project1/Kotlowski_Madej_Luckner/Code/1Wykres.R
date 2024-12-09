LungCancer<-read.csv("LungCancerUsa.csv")
LarynxCancer<-read.csv("LarynxCancerUsa.csv")
tobacco<-read.csv("tobacco.csv")
library(dplyr)
library(ggplot2)
library(extrafont)
library(patchwork)
library(stringr)
loadfonts(device = "win") 
loadfonts(device = "pdf")
font_add_google("Open Sans", "open_sans")
showtext_auto()

Lung<-LungCancer%>%
  
  mutate(Case.Count=as.numeric(Case.Count),
         Population=as.numeric(Population),
         LungProc=Case.Count/Population,
         LungCount=Case.Count,
         LungAdj=as.numeric(Age.Adjusted.Rate))

Larynx<-LarynxCancer%>%
  mutate(Case.Count=as.numeric(Case.Count),
         Population=as.numeric(Population),
         LarynxProc=Case.Count/Population,
         LarynxCount=Case.Count,
         LarynxAdj=as.numeric(Age.Adjusted.Rate))

Cancer<-Lung%>%
  inner_join(Larynx,by="Area")%>%
  rename(State=Area)%>%
  select(State,LungProc,LungCount,LungAdj,LarynxProc,LarynxCount,LarynxAdj)

tobacco<-tobacco%>%
  mutate(Smoke.everyday=as.numeric(substring(Smoke.everyday,1,4)))

tobacco_cleared<-tobacco%>%
  group_by(State)%>%
  summarise(Proc_smoking=mean(Smoke.everyday,na.rm=TRUE))


#LUNG CANCER
result1a<-Cancer%>%
  inner_join(tobacco_cleared,by="State")%>%
  filter(!State=="Indiana")%>%
  mutate(States=forcats::fct_reorder(State,Proc_smoking)) 


# wykres1 <- ggplot(data=result1a,aes(x=Proc_smoking,y=States,fill=LungAdj))+#albo lungproc albo lung adj
#   geom_col()+
#   scale_fill_gradient(low="orange",high="blue",)+
#   labs(title="Lung Cancer and smoking in the USA",
#        x="% of everyday smokers",
#        fill="Number of cancer cases \nper 100,000 people") +
#   theme(axis.title.y = element_blank(),
#         axis.text.y = element_blank()) +
#   theme_minimal()+
#   theme(
#     panel.background = element_blank(),    
#     panel.grid.major = element_blank(),     
#     panel.grid.minor = element_blank(),     
#     
#     plot.background = element_rect(fill = "#221A32", color = NA),
#     
#     legend.background = element_rect(fill = "#221A32", color = NA),
#     legend.text = element_text(color = "white"),
#     legend.title = element_text(color = "white"),
#     
#     axis.line = element_line(color = "white"),  
#     
#     axis.title.x = element_text(color = "white"), 
#     axis.text.x = element_text(color = "white"),
#     axis.title.x = element_text(size = 15,family = "open_sans"),
#     axis.text.x = element_text(size = 10,family = "open_sans"),
#     
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     
#     
#   )
# 
# wykres1


#LARYNX CANCER
result2<-Cancer%>%
  inner_join(tobacco_cleared,by="State")%>%
  filter(!State=="Indiana")%>%
  
  mutate(dlugosc_nazwy = nchar(str_trim(State, side="right")),
         dupa=1
         ) %>%
  group_by(dupa) %>%
  mutate(dlugosc_nazwy = (max(dlugosc_nazwy)-dlugosc_nazwy) %/% 2) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(State = paste0(
    paste0(
      paste0(rep(" ", ceiling(dlugosc_nazwy*1.5)+5), collapse = ""),
      State)
    )
    ) %>%
  ungroup() %>% 
  mutate(States = forcats::fct_reorder(State, Proc_smoking))


# wykres2 <- ggplot(data=result2,aes(x=Proc_smoking,y=States,fill=LarynxAdj))+
#   geom_col()+
#   scale_fill_gradient(low="green",high="red",)+
#   labs(title="Larynx Cancer and smoking in the USA               ",
#        x="% of everyday smokers",
#        fill="Number of cancer cases \nper 100,000 people") +
#   scale_x_reverse() +
#   scale_y_discrete(position = "right") +
#   theme(
#         axis.text.x = element_text(size = 34,family = "open_sans"),axis.text.y=element_text(size=34,family = "open_sans"),
#         legend.text = element_text(size = 34,family = "open_sans"),legend.title = element_text(size = 34,family = "open_sans"))+
#   theme_minimal()+
#   theme(
#     panel.background = element_blank(),    
#     panel.grid.major = element_blank(),     
#     panel.grid.minor = element_blank(),     
#      
#     #axis.title = element_text(color = "white"), 
#     
#     plot.background = element_rect(fill = "#221A32", color = NA),
#     
#     legend.background = element_rect(fill = "#221A32", color = NA),
#     legend.text = element_text(color = "white"),
#     legend.title = element_text(color = "white"),
#     legend.position = "left",
#     
#     title = element_text(color = "white"),
#     
#     plot.title = element_text(size=20,family = "open_sans"),
#     
#     axis.text = element_text(color = "white"),
#     axis.line = element_line(color = "white"), 
#     
#     axis.title.x = element_text(size = 15,family = "open_sans"),
#     axis.text.x = element_text(size = 10,family = "open_sans"),
#     
#     axis.title.y = element_blank(),
#     axis.text.y =element_text(size=10,family = "open_sans")
#   )

# wykres2
# wykres2 + wykres1

  # theme(
  #   panel.background = element_blank(),    
  #   panel.grid.major = element_blank(),     
  #   panel.grid.minor = element_blank(),     
  #   axis.line = element_line(color = "white"),  
  #   axis.title = element_text(color = "white"), 
  #   axis.text = element_text(color = "white"),
  #   plot.background = element_rect(fill = "#221A32", color = NA),
  #   legend.background = element_rect(fill = "#221A32", color = NA),
  #   legend.text = element_text(color = "white"),
  #   legend.title = element_text(color = "white")
  # )


wykres2 <- ggplot(data=result2,aes(x=Proc_smoking,y=States,fill=LarynxAdj))+
  geom_col()+
  scale_fill_gradient(low="green",high="red",)+
  labs(title="Larynx Cancer and smoking in the USA               ",
       x="% of everyday smokers",
       fill="Number of cancer cases \nper 100,000 people") +
  scale_x_reverse() +
  scale_y_discrete(position = "right") +
  theme(
    axis.text.x = element_text(size = 34,family = "open_sans"),axis.text.y=element_text(size=34,family = "open_sans"),
    legend.text = element_text(size = 34,family = "open_sans"),legend.title = element_text(size = 34,family = "open_sans"))+
  theme_minimal()+
  theme(
    panel.background = element_blank(),    
    panel.grid.major = element_blank(),     
    panel.grid.minor = element_blank(),     
    
    #axis.title = element_text(color = "white"), 
    
    plot.background = element_rect(fill = "#221A32", color = NA),
    
    legend.background = element_rect(fill = "#221A32", color = NA),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.position = "left",
    
    title = element_text(color = "white"),
    
    plot.title = element_text(size=20,family = "open_sans"),
    
    axis.text = element_text(color = "white"),
    axis.line = element_line(color = "white"), 
    
    axis.title.x = element_text(size = 15,family = "open_sans"),
    axis.text.x = element_text(size = 10,family = "open_sans"),
    
    axis.title.y = element_blank(),
    axis.text.y =element_text(size=10,family = "open_sans")
  )
  

wykres1 <- ggplot(data=result1a,aes(x=Proc_smoking,y=States,fill=LungAdj))+#albo lungproc albo lung adj
  geom_col()+
  scale_fill_gradient(low="orange",high="blue",)+
  labs(title="Lung Cancer and smoking in the USA",
       x="% of everyday smokers",
       fill="Number of cancer cases \nper 100,000 people") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  theme_minimal()+
  theme(
    panel.background = element_blank(),    
    panel.grid.major = element_blank(),     
    panel.grid.minor = element_blank(),     
    
    plot.background = element_rect(fill = "#221A32", color = NA),
    
    legend.background = element_rect(fill = "#221A32", color = NA),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    
    axis.line = element_line(color = "white"),  
    
    plot.title = element_text(size=20,family = "open_sans", colour = "white"),
    
    axis.title.x = element_text(size = 15,family = "open_sans", colour = "white"),
    axis.text.x = element_text(size = 10,family = "open_sans", colour = "white"),
    
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    
    
  )

wykres2 + wykres1

