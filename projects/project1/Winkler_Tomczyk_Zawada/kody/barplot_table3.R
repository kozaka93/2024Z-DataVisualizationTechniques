library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


Table_1<- read_csv("Table 1 Individual-level descriptive statistics..csv")
Table_2<- read_csv("CORRECTED_Table 2 Country-level descriptive statistics. - Arkusz1.csv")
Table_3<- read_csv("Table 3 Determinants of professional care seeking (HLM Poisson regression)_ individual effects - Arkusz1.csv")
Table_4<- read_csv("Table 4 Determinants of professional care seeking (HLM Poisson regression)_ country level effects and variance components - Arkusz1.csv")

#colnames(Table_2)<-c('Country', 'Count','Divorce_rate','Care_supply')

# Table_2 %>% 
#   ggplot(aes(x = Divorce_rate))+
#   geom_density()
# 
# Table_2 %>% 
#   ggplot(aes(x = Care_supply))+
#   geom_density()
# 
# colnames(Table_1)<-c("data_1" ,   "Tot.N" ,  "Tot.%"  , "Men.N" ,  "Men.%"  , "Women.N", "Women.%", "data_2")
# 
# Table_1 %>% 
#   filter(data_2 == 'Marital status') %>% 
#   ggplot(aes(x=data_1,y=`Tot.%`))+
#   geom_col()



# df1<-cutted_3 %>% 
#   pivot_longer(cols = -Category, names_to = "Column", values_to = "Value")
#   
# df1$Column <- factor(df1$Column, levels = as.character(2:15))

# ggplot(df1, aes(x = Category, y = Value, color = Column)) +
#   geom_point() +
#   geom_line(aes(group = Column), alpha = 0.7) +
#   labs(
#     x = "Category",
#     y = "Value",
#     title = "Wartości dla różnych kategorii"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

cutted_3<-Table_3[c(6,7,8,9),c(1,2,3,5,6,8,9,11,12,14,15)]
colnames(cutted_3)<-c('Category','2','3','5','6','8','9','11','12','14','15')
cutted_3<-cutted_3[,c(1,10)]





cutted_3$`14` <- as.numeric(cutted_3$`14`)
cutted_3$`14` <- cutted_3$`14`+c(1,1,1,1)
cutted_3<-rbind(cutted_3,c('Married',as.numeric(1)))
cutted_3$`14` <- as.numeric(cutted_3$`14`)
cutted_3[1,1]<-"Divorced"
cutted_3$Category <- factor(cutted_3$Category, levels = c("Married", "Divorced","Single", "Widowed", "Other"))

p<-library(scales)

p<-ggplot(cutted_3, aes(x = Category, y = `14`, fill = Category)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(
    aes(label = scales::percent(`14`, accuracy = 0.1)),  # Przekształcenie wartości na procenty
    vjust = -1.0,  # Przesunięcie tekstu nad słupki
    color = "white",  # Kolor tekstu
    size = 5  # Rozmiar tekstu
  ) +
  labs(
    x = "",
    y = "",
    title = ""
  ) +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::percent,  # Użyj skali procentowej
    limits = c(0, 1.5)  # Ustawienie zakresu osi Y od 0 do 150% (1.5)
  ) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  theme(
    axis.text.x = element_blank(),  # Ukryj tekst na osi x
    axis.ticks.x = element_blank(),  # Ukryj znaczniki osi x
    axis.text.y = element_text(size = 14)  # Opcjonalnie: dostosuj rozmiar tekstu na osi y
  ) +
  guides(fill = "none") +
  geom_hline(yintercept = 1, color = "yellow", linetype = "solid", size = 1)


p
ggsave(
  "seeking_mental_transparent.png", 
  plot = last_plot(),  # Wybiera ostatnio utworzony wykres
  width = 6, 
  height = 4, 
  bg = "transparent"  # Ustaw przezroczyste tło
)
