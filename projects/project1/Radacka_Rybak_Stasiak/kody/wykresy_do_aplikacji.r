
library("dplyr")
library("ggplot2")
library("tidyr")
library("purrr")
library("forcats")
library(ggalluvial)
library(showtext)
font_add_google("Montserrat")
showtext_auto()

data<-read.csv("C:/Users/Mateu/OneDrive/Dokumenty/archive/smmh.csv")


data<-as.data.frame(data)
colnames(data)



#t<-unique(data[8])
#typeof(t)
#t<-t[which.max(lapply(t,length))]
#print(t)

#print(unique(data[5]))

pom<-data %>% 
    filter(pull(.,7)=="Yes") %>% 
    select(2,3,8,9,19,21,12,4,13,15,18,16)

colnames(pom)<-c("age","gender","platforms_used","daily_screen_time","depression_issues",
                 "sleep_issues","focus_issues","relationship","distracted","concentration","validation","compare")

#View(pom)

#print(unique(pom["daily_screen_time"]))

# Lista wszystkich możliwych aplikacji
all_apps <- c("Facebook", "Twitter", "Instagram", "YouTube", "Snapchat", "Discord", "Reddit", "Pinterest", "TikTok")

pom <- pom %>%
    mutate(platforms_used = strsplit(platforms_used, ", "))

# Zamiana kolumny 'platforms_used' na wiele kolumn binarnych
pom <- pom %>%
    mutate(Facebook = ifelse(lapply(platforms_used, function(x) "Facebook" %in% x), 1, 0)) %>%
    mutate(Twitter = ifelse(lapply(platforms_used, function(x) "Twitter" %in% x), 1, 0)) %>%
    mutate(Instagram = ifelse(lapply(platforms_used, function(x) "Instagram" %in% x), 1, 0)) %>%
    mutate(YouTube = ifelse(lapply(platforms_used, function(x) "YouTube" %in% x), 1, 0)) %>%
    mutate(Snapchat = ifelse(lapply(platforms_used, function(x) "Snapchat" %in% x), 1, 0)) %>%
    mutate(Discord = ifelse(lapply(platforms_used, function(x) "Discord" %in% x), 1, 0)) %>%
    mutate(Reddit = ifelse(lapply(platforms_used, function(x) "Reddit" %in% x), 1, 0)) %>%
    mutate(Pinterest = ifelse(lapply(platforms_used, function(x) "Pinterest" %in% x), 1, 0)) %>%
    mutate(TikTok = ifelse(lapply(platforms_used, function(x) "TikTok" %in% x), 1, 0)) %>% 
    select(-platforms_used)

#names(pom)[-1] <- all_apps


#View(pom)

pom <- pom %>%
    mutate(daily_screen_time = case_when(
        daily_screen_time == "Between 2 and 3 hours" ~ "3h",
        daily_screen_time == "More than 5 hours" ~ "5h+",
        daily_screen_time == "Between 3 and 4 hours" ~ "4h",
        daily_screen_time == "Between 1 and 2 hours" ~ "2h",
        daily_screen_time == "Less than an Hour" ~ "1h",
        daily_screen_time == "Between 4 and 5 hours" ~ "5h",
        TRUE ~ daily_screen_time  
    ))


#Facebook
facebook<- pom %>% 
    filter(Facebook==1,relationship!="Divorced") %>% 
    mutate(relationship=ifelse(relationship=="Single","Single","In a relationship")) %>% 
    group_by(daily_screen_time, relationship) %>%
    summarise(count = n(), .groups = "drop")

wykres_facebook<-facebook %>% ggplot(aes(x = factor(daily_screen_time, levels = unique(daily_screen_time)), y = count, fill = relationship)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values = c("Single" = "#d276c9", "In a relationship" = "#7adfe2")) +  
    labs(
        x = "Time spent on Facebook",
        y = "Proportion",
        title = "Do single people spend more time on Facebook, than people commited to a relationship?",
        fill = "Relationship Status"
    ) +
    theme_minimal()+
    theme(
        text = element_text(family = "Montserrat", color = "white"), 
        plot.title = element_text(size = 25, face = "bold", color = "white"),
        axis.title = element_text(size = 20, color = "white"),
        axis.text = element_text(size = 17, color = "white"),
        legend.title = element_text(size = 17, color = "white"),
        legend.text = element_text(size = 14, color = "white"),
        panel.background = element_blank(),        
        plot.background = element_blank(),          
        legend.background = element_blank() 
    )

#Youtube
heatMapSingle<-pom %>% 
    filter(YouTube==1) %>% 
    group_by(distracted,concentration) %>% 
    summarise(count=n())

#View(data.frame(validation=5,compare=3,count=0))
heatMapSingle<-rbind(heatMapSingle,data.frame(concentration=5,distracted=2,count=0))
heatMapSingle<-rbind(heatMapSingle,data.frame(concentration=5,distracted=1,count=0))


#heatMapSingle<-rbind(heatMapSingle,data.frame(validation=5,compare=3,count=0))
#View(heatMapSingle)
wykres_youtube<-heatMapSingle %>% 
    ggplot(aes(x=distracted,y=concentration,fill=count))+
    geom_tile()+
    scale_fill_gradient(low = "#8c52ff", high = "#f44e66")+
    
    labs(
        fill="Amount of users",
        x="How easily distracted are you?",
        y="Rate your concentration issues")+
    theme_minimal()+
    theme(
        text = element_text(family = "Montserrat", color = "white"),  
        plot.title = element_text(size = 25, face = "bold", color = "white"),
        axis.title = element_text(size = 20, color = "white"),
        axis.text = element_text(size = 17, color = "white"),
        legend.title = element_text(size = 17, color = "white"),
        legend.text = element_text(size = 14, color = "white"),
        panel.background = element_blank(),         
        plot.background = element_blank(),        
        legend.background = element_blank()
    )

#Snapchat
wyk3<-pom %>% 
    filter(Snapchat==1) %>% 
    filter(gender %in% c("Male","Female")) %>% 
    mutate(Gender=gender)

wykres_snapchat<-wyk3 %>% ggplot( aes(x = compare, fill = Gender,color=Gender)) +
    geom_density(alpha = 0.6) +
    theme_minimal() +
    labs(title = "How often do males and females compare to other people (on a scale 1-5) on Snapchat",
         x="Answer",
         y="Density")+
    scale_fill_manual(values = c("Female" = "#d276c9", "Male" = "#6891d3")) +  
    theme(
        text = element_text(family = "Montserrat", color = "white"), 
        plot.title = element_text(size = 25, face = "bold", color = "white"),
        axis.title = element_text(size = 20, color = "white"),
        axis.text = element_text(size = 17, color = "white"),
        legend.title = element_text(size = 17, color = "white"),
        legend.text = element_text(size = 14, color = "white"),
        panel.background = element_blank(),        
        plot.background = element_blank(),          
        legend.background = element_blank() 
    )

wykres_facebook
wykres_snapchat
wykres_youtube

ggsave("wykres_facebook.png",plot=wykres_facebook,bg="transparent")
ggsave("wykres_snapchat.png",plot=wykres_snapchat,bg="transparent")
ggsave("wykres_youtube.png",plot=wykres_youtube,bg="transparent")


