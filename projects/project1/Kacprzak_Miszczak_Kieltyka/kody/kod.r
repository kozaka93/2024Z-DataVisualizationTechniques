dlugosc_zycia<-read.csv("C:\\Users\\User\\Downloads\\50e9d64e-a408-41fc-9e7f-3f875e91c896.csv")
liczba_med<-read.csv("C:\\Users\\User\\Downloads\\284f24ab-969e-4bac-9bf4-f5fe4fc78ab2.csv")
proc_finans_rząd<-read.csv("C:\\Users\\User\\Downloads\\55973025-4853-4de3-90d7-77c43cf8c121.csv")
happy<-read.csv("C:\\Users\\User\\Downloads\\poprawnedaneszczescie2021.xls")
kontynenty<-read.csv("C:\\Users\\User\\Downloads\\country_continents.csv")

library("dplyr")
library("ggplot2")



sub1_4=function(x){
  return(substr(x,1,4))
}

dlugosc_zycia<-dlugosc_zycia %>% 
  select(Indicator,ParentLocation,Location,Period,Value,SpatialDimValueCode) %>% 
  filter(Period==2021,Indicator=="Life expectancy at birth (years)") %>% 
  mutate(dlugosc_zycia=as.numeric(sub1_4(Value))) %>% 
  select(ParentLocation,Location,dlugosc_zycia,SpatialDimValueCode)

liczba_med<-liczba_med %>% 
  select(ParentLocation,Location,Period,Value) %>% 
  filter(Period==2021) %>% 
  mutate(licz_med=Value) %>% 
  select(ParentLocation,Location,licz_med)

proc_finans_rząd<-proc_finans_rząd %>% 
  select(ParentLocation,Location,Period,Value) %>% 
  filter(Period==2021) %>% 
  mutate(proc_finans=Value) %>% 
  select(ParentLocation,Location,proc_finans)


all <- proc_finans_rząd %>% 
  inner_join(liczba_med) %>% 
  inner_join(dlugosc_zycia) %>% 
  mutate(sovereignt=Location)
  # filter(ParentLocation!="Western Pacific") 








#mapy
all<-all %>% 
  group_by(sovereignt) %>% 
  summarise(proc_finans,licz_med,dlugosc_zycia=mean(dlugosc_zycia))
  


w2hr <- map_data("world") %>% filter(long <= 180)

dlugosc_zycia1<-dlugosc_zycia %>% 
  arrange(Location) %>% 
  filter(rep(c(TRUE,FALSE,FALSE),185)) %>% 
  mutate(Location=case_when( Location=="United States of America" ~ "USA",
                             Location=="Russian Federation" ~ "Russia",
                             Location =="United Kingdom of Great Britain and Northern Ireland" ~"UK",
                             Location == "Bolivia (Plurinational State of)" ~"Boliwia",
                             Location == "Czechia" ~"Czech Republic",
                             Location == "Republic of Korea" ~"South Korea",
                             Location == "Venezuela (Bolivarian Republic of)" ~"Venezuela",
                             Location == "Viet Nam" ~"Vietnam",
                             Location == "Türkiye" ~"Turkey",
                             Location == "Iran (Islamic Republic of)" ~"Iran",
                             Location == "Boliwia" ~"Bolivia",
         .default=Location))

data =full_join(dlugosc_zycia1,w2hr, by=join_by(Location==region),keep=TRUE) %>% 
  filter(region!="Antarctica")


ggplot() + 
  geom_polygon(data = data, aes(x=long, y = lat, group = group, fill= dlugosc_zycia), color = "#202020") + 
  coord_map("mercator")+
  scale_fill_gradient(low="red",high="green")+
  labs(x="",
       y="")+
  scale_y_continuous(labels = NULL)+
  scale_x_continuous(labels = NULL)+
  labs(title="Map of the average life expectancy",
       fill="life expectancy")
  


dlugosc_zycia2<-dlugosc_zycia %>% 
  arrange(Location) %>% 
  filter(rep(c(TRUE,FALSE,FALSE),185)) %>% 
  inner_join(kontynenty, by=join_by(SpatialDimValueCode==Three_Letter_Country_Code))

# kontynenty<-kontynenty %>% 
#   group_by(Country_Name) %>% 
#   summarise(Continent_Name=first(Continent_Name))

ggplot(data=dlugosc_zycia2,mapping = aes(x=dlugosc_zycia,fill=Continent_Name))+
  geom_density(alpha=0.7)+
  labs(title="rozkład średniej długości życia" )


Europe<-kontynenty %>% 
  filter(Continent_Name=="Europe") %>% 
  inner_join(dlugosc_zycia, by=join_by(Three_Letter_Country_Code==SpatialDimValueCode))
Africa<-kontynenty %>% 
  filter(Continent_Name=="Africa")%>% 
  inner_join(dlugosc_zycia, by=join_by(Three_Letter_Country_Code==SpatialDimValueCode))
Asia<-kontynenty %>% 
  filter(Continent_Name=="Asia")%>% 
  inner_join(dlugosc_zycia, by=join_by(Three_Letter_Country_Code==SpatialDimValueCode))
AmericaN<-kontynenty %>% 
  filter(Continent_Name=="North America")%>% 
  inner_join(dlugosc_zycia, by=join_by(Three_Letter_Country_Code==SpatialDimValueCode))
AmericaS<-kontynenty %>% 
  filter(Continent_Name=="South America")%>% 
  inner_join(dlugosc_zycia, by=join_by(Three_Letter_Country_Code==SpatialDimValueCode))
Oceania<-kontynenty %>% 
  filter(Continent_Name=="Oceania")%>% 
  inner_join(dlugosc_zycia, by=join_by(Three_Letter_Country_Code==SpatialDimValueCode))
ggplot()+
  geom_density(data=Europe,alpha=1,,mapping = aes(x=dlugosc_zycia,fill=Continent_Name))+
  geom_density(data=Africa,alpha=0.7,,mapping = aes(x=dlugosc_zycia,fill=Continent_Name))+
  geom_density(data=AmericaS,alpha=0.5,,mapping = aes(x=dlugosc_zycia,fill=Continent_Name))+
  geom_density(data=AmericaN,alpha=0.5,,mapping = aes(x=dlugosc_zycia,fill=Continent_Name))+
  geom_density(data=Asia,alpha=0.5,,mapping = aes(x=dlugosc_zycia,fill=Continent_Name))+
  geom_density(data=Oceania,alpha=0.5,,mapping = aes(x=dlugosc_zycia,fill=Continent_Name))+
  scale_fill_manual(values=c("#e41a1c",
    "#ffff33",
    "#4daf4a",
    "#377eb8",
    "#df8f11",
    "#984ea3"))+
  labs(title="Distribution of average life expectancy by continents",
       fill="Continent",
       x="life expectancy [years]")+
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    title = element_text(size=18),
    legend.text  = element_text(size=14),
    legend.title = element_text(size = 16),
    legend.background = element_rect(fill = "transparent", color = NA), # Tło legendy
    panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
    plot.background = element_rect(fill = "transparent", color = NA)     # Transparent plot background
  )
ggsave("plot2.png", plot = last_plot(), bg = "transparent", width = 10, height = 6, units = "in")





w2hr <- map_data("world") %>% filter(long <= 180)

proc_finans_rząd2<-proc_finans_rząd %>% 
  mutate(Location=case_when( Location=="United States of America" ~ "USA",
                             Location=="Russian Federation" ~ "Russia",
                             Location =="United Kingdom of Great Britain and Northern Ireland" ~"UK",
                             Location == "Bolivia (Plurinational State of)" ~"Boliwia",
                             Location == "Czechia" ~"Czech Republic",
                             Location == "Republic of Korea" ~"South Korea",
                             Location == "Venezuela (Bolivarian Republic of)" ~"Venezuela",
                             Location == "Viet Nam" ~"Vietnam",
                             Location == "Türkiye" ~"Turkey",
                             Location == "Iran (Islamic Republic of)" ~"Iran",
                             Location == "Boliwia" ~"Bolivia",
                             Location == "Liberia" ~"Libia",
                             Location == "" ~"Somalia",
                             .default=Location))

data =full_join(proc_finans_rząd2,w2hr, by=join_by(Location==region),keep=TRUE) %>% 
  filter(region!="Antarctica" & region!="Greenland")
data$proc_finans_label <- ifelse(is.na(data$proc_finans), "Missing", as.character(data$proc_finans))

ggplot() + 
  geom_polygon(data = data, aes(x=long, y = lat, group = group, fill= proc_finans), color = "#202020") + 
  coord_map("mercator")+
  scale_fill_gradient(low="red",high="green")+
  labs(x="",
       y="")+
  scale_y_continuous(labels = NULL)+
  scale_x_continuous(labels = NULL)+
  labs(title="Map of total healthcare expenditure as a percentage of GDP",
       fill='Total healthcare
expenditure
as a percentage
of GDP
       ')+
  theme(
    panel.grid.major = element_blank(),  # Usuń główną siatkę
    panel.grid.minor = element_blank(),   # Usuń mniejszą siatkę
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    title = element_text(size=20),
    legend.text  = element_text(size=12),
    legend.title = element_text(size = 16),
    legend.background = element_rect(fill = "transparent", color = NA), # Tło legendy
    panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
    plot.background = element_rect(fill = "transparent", color = NA)     # Transparent plot background
  )



ggsave("plot.png", plot = last_plot(), bg = "transparent", width = 10, height = 6, units = "in")


library(ggplot2)
library(patchwork)


x_limits <- c(min(c(Europe$dlugosc_zycia, Africa$dlugosc_zycia, 
                    AmericaS$dlugosc_zycia, AmericaN$dlugosc_zycia, 
                    Asia$dlugosc_zycia, Oceania$dlugosc_zycia)),
              max(c(Europe$dlugosc_zycia, Africa$dlugosc_zycia, 
                    AmericaS$dlugosc_zycia, AmericaN$dlugosc_zycia, 
                    Asia$dlugosc_zycia, Oceania$dlugosc_zycia)))


p1 <- ggplot(Africa, aes(x=dlugosc_zycia, fill=Continent_Name)) +
  geom_density(alpha=1) +
  scale_fill_manual(values=c("#e41a1c")) +  # Kolor Afryki
  theme_minimal() +
  theme(legend.position="right", axis.title = element_blank(), 
        axis.text = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA),  
        plot.background = element_rect(fill = "transparent", color = NA))+    
  xlim(x_limits) +
  ylim(c(0,0.08))+
  guides(fill = guide_legend(title = " "))

p2 <- ggplot(Asia, aes(x=dlugosc_zycia, fill=Continent_Name)) +
  geom_density(alpha=1) +
  scale_fill_manual(values=c("#dddd33")) +  # Kolor Azji
  theme_minimal() +
  theme(legend.position="right", axis.title = element_blank(),
        axis.text = element_blank(), plot.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA),  
        plot.background = element_rect(fill = "transparent", color = NA)) +  # Marginesy 0
  xlim(x_limits) + ylim(c(0,0.08))+
  guides(fill = guide_legend(title = NULL))

p3 <- ggplot(Europe, aes(x=dlugosc_zycia, fill=Continent_Name)) +
  geom_density(alpha=1) +
  scale_fill_manual(values=c("#4daf4a")) +  # Kolor Europy
  theme_minimal() +
  theme(legend.position="right", axis.title = element_blank(), 
        axis.text = element_blank(), plot.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA),  
        plot.background = element_rect(fill = "transparent", color = NA))+     # Marginesy 0
  xlim(x_limits) + ylim(c(0,0.08))+
  guides(fill = guide_legend(title = NULL))

p4 <- ggplot(AmericaN, aes(x=dlugosc_zycia, fill=Continent_Name)) +
  geom_density(alpha=1) +
  scale_fill_manual(values=c("#377eb8")) +  # Kolor Ameryki Północnej
  theme_minimal() +
  theme(legend.position="right", axis.title = element_blank(),
        axis.text = element_blank(), plot.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA),  
        plot.background = element_rect(fill = "transparent", color = NA))+     # Marginesy 0
  xlim(x_limits) + ylim(c(0,0.08))+
  guides(fill = guide_legend(title = NULL))

p5 <- ggplot(Oceania, aes(x=dlugosc_zycia, fill=Continent_Name)) +
  geom_density(alpha=1) +
  scale_fill_manual(values=c("#984ea3")) +  # Kolor Oceanii
  theme_minimal() +
  theme(legend.position="right", axis.title = element_blank(),
        axis.text = element_blank(), plot.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA),  
        plot.background = element_rect(fill = "transparent", color = NA))+     # Marginesy 0
  xlim(x_limits) + ylim(c(0,0.08))+
  guides(fill = guide_legend(title = NULL))

p6 <- ggplot(AmericaS, aes(x=dlugosc_zycia, fill=Continent_Name)) +
  geom_density(alpha=1) +
  scale_fill_manual(values=c("#df8f11")) +  # Kolor Ameryki Południowej
  theme_minimal() +
  theme(legend.position="right", plot.title = element_blank(),
        axis.title.y = element_blank(),axis.text.y = element_blank(),
        plot.margin = margin(0.0, 0, 0, 0),
        axis.title.x = element_text(size=26),
        legend.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA),  
        plot.background = element_rect(fill = "transparent", color = NA))+     # Marginesy 0
  xlim(x_limits) + ylim(c(0,0.08))+
  labs(x = "Life expectancy")+
  guides(fill = guide_legend(title = element_blank()))

# Łączenie wykresów w odpowiedniej kolejności: Afryka, Azja, Europa, Ameryka Płn, Oceania, Ameryka Płd
p1 + p2 + p3 + p4 + p5 + p6 +
  patchwork::plot_layout(ncol = 1, guides = "collect") +
  patchwork::plot_annotation(title = "Density of average life expectancy by continents") &
  theme(
    plot.title = element_text(size = 26),
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 26),
    legend.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

ggsave("density_continents2.png", plot = last_plot(), bg = "transparent", width = 10, height = 6, units = "in")

brak<-AmericaN %>% mutate(Continent_Name="No data")
ggplot(brak,aes(x=dlugosc_zycia , fill=Continent_Name))+
  geom_density()+
  scale_fill_manual(values=c("darkgrey"))+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text  = element_text(size=12),
        legend.background = element_rect(fill = "transparent", color = NA), # Tło legendy
        panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
        plot.background = element_rect(fill = "transparent", color = NA)     # Transparent plot background
  )
ggsave("brak_danych.png", plot = last_plot(), bg = "transparent", width = 10, height = 6, units = "in")

