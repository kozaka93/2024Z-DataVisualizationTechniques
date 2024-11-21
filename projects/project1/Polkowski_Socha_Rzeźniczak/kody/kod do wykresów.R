library(dplyr)
library(ggplot2)
library(tidyr)


year_used = c(2014)
options(scipen = 999)

df_gdp <- read.csv("a3d8e56b-02ed-498a-9d30-3354ac4e442c_Data.csv")
df_gdp_agg<- df_gdp %>%
  filter(Series.Code == "NY.GDP.PCAP.CD")%>%
  select(Country.Name,Country.Code ,X2014..YR2014.)%>%
  filter(X2014..YR2014.!="..")




df_prev <- read.csv("./2014_4_disease_data.csv")
df_prev_agg <- df_prev %>%
  select(-c(age, sex, metric, upper, lower, measure, year))


df_prev_bp <- read.csv("./NCD_RisC_Lancet_2017_BP_age_standardised_countries.csv")
df_prev_bp_agg <- df_prev_bp %>% filter(Year == year_used) %>%
  group_by(Country.Region.World,ISO)%>%
  summarise(prev_hypertension = mean(Prevalence.of.raised.blood.pressure))%>%
  filter(!(Country.Region.World%in%c("Cook Islands")))
  

df_bp_gdp_merged <- df_prev_bp_agg %>% 
  inner_join(df_gdp_agg,by=c("ISO"="Country.Code"))%>%select(-c(ISO,Country.Name))%>%
  mutate(cause = "Hypertension")
df_bp_gdp_merged <- df_bp_gdp_merged%>%rename(val=prev_hypertension)%>%
  relocate(Country.Region.World,X2014..YR2014.,cause,val)



names_to_change_df_prev = c("Bolivia (Plurinational State of)", "Côte d'Ivoire",
                            "Czechia","Democratic Republic of the Congo",
                            "Guinea-Bissau","Iran (Islamic Republic of)",
                            "Lao People's Democratic Republic",
                            "North Macedonia","Republic of Moldova",
                            "Palestine","Republic of Korea",
                            "Eswatini","United Republic of Tanzania",
                            "Venezuela (Bolivarian Republic of)"
                            )


proper_names_df_prev = c("Bolivia","Cote d'Ivoire",
                         "Czech Republic","DR Congo",
                         "Guinea Bissau","Iran",
                         "Lao PDR",
                         "Macedonia (TFYR)","Moldova",
                         "Occupied Palestinian Territory","South Korea",
                         "Swaziland","Tanzania","Venezuela")

for (i in 1:length(names_to_change_df_prev)) {
  df_prev_agg$location[df_prev_agg$location == names_to_change_df_prev[i]] = proper_names_df_prev[i]
}



df_prev_gdp_merged <- df_bp_gdp_merged %>% 
  inner_join(df_prev_agg, by=c("Country.Region.World"="location"))%>%
  select(Country.Region.World,X2014..YR2014.,cause.y,val.y)%>%
  rename(cause = cause.y, val = val.y)



df_final <- rbind(df_bp_gdp_merged,df_prev_gdp_merged) %>% 
  rename(GDP_per_capita = X2014..YR2014.)%>%
  mutate(GDP_per_capita = as.double(GDP_per_capita))%>%
  mutate(cause = case_when(cause == "Anxiety disorders" ~"Stany lękowe",
                           cause == "Depressive disorders" ~ "Depresja",
                           cause == "Diabetes mellitus type 2"~"Cukrzyca typu 2",
                           cause == "Hypertension" ~"Nadciśnienie",
                           cause == "Tracheal, bronchus, and lung cancer" ~ "Nowotwory płuc, tachawicy i oskrzeli"))


quant <- unname(quantile(df_final$GDP_per_capita,c(0.25,0.5,0.75)))

df_final_2 <- df_final %>% mutate(which_quant = case_when(GDP_per_capita<=quant[1] ~ 1,
                                                        GDP_per_capita<=quant[2] ~ 2,
                                                        GDP_per_capita<=quant[3] ~ 3,
                                                        GDP_per_capita>quant[3] ~ 4))

                                                        



 df_final_2 %>% filter(cause%in%c("Nowotwory płuc, tachawicy i oskrzeli",
                                  "Stany lękowe","Depresja","Nadciśnienie"))%>%
   mutate(which_quant = case_when(which_quant==1 ~ "0$ - 1984$",
                                  which_quant==2 ~"1985$ - 6436$", 
                                  which_quant==3 ~"6437$ - 16980$",
                                  which_quant==4 ~"więcej niż 16980$"))%>%
  ggplot(aes(x=factor(which_quant,levels = c("0$ - 1984$","1985$ - 6436$",
                                             "6437$ - 16980$","więcej niż 16980$")),
             y = val*100000,fill=factor(which_quant,levels = c("0$ - 1984$",
                  "1985$ - 6436$", "6437$ - 16980$","więcej niż 16980$") ) )) + 
  scale_y_log10()+labs(x = "Choroba", y = "ilość chorych na 100 000 osób",
                       fill = "PKB per capita",
                       title = "Rozkład krajów o danym rozpowszechnieniu wybranych chorób cywilizacyjnych")+
  geom_violin()+
  theme(
        plot.title = element_text(size = 17, color = 'white'),
       panel.background = element_rect(fill='transparent'),
       plot.background = element_rect(fill='transparent', color=NA),
       legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "grey"),
        strip.text = element_text(color = "white",size = 17),
        strip.background = element_rect(fill='transparent'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 15,color = "white"),
        legend.text = element_text(size= 15,color = "white"),
        legend.title = element_text(size = 17,color = "white"),
        axis.title.y = element_text(size = 17,color = 'white'),
        theme_light())+
   facet_wrap(~cause, scales = "free_y")+scale_fill_brewer(palette = "PuBu")
 

 ggsave("./transparent_graph.png",
        plot = last_plot(), bg = "transparent", units = "px",
        width = 3500, height = 2100)
