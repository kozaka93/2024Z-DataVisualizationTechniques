library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

df_orders <- read.csv('orders.csv')
df_order_items <- read.csv('order_items.csv')
df_products <- read.csv('products.csv')
df_brands <- read.csv('brands.csv')
df_categories <-  read.csv('categories.csv')
df_customers <- read.csv('customers.csv')


#---------------------------------------------------------------------------------------------
####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.


df_temp <- df_orders %>% merge(df_order_items,by="order_id")

df_temp <- df_temp %>% merge(df_customers,by="customer_id")

df_temp <- df_temp %>% mutate(quarter = substr(quarters(as.Date(order_date)),2,2))

df_temp <- df_temp%>%group_by(state,quarter,product_id)%>%summarise(counter=n())%>%
  ungroup()%>%group_by(state,quarter)%>%arrange(desc(counter))%>%filter(counter==max(counter))

final_df_1 <- merge(df_temp,df_products,by="product_id")%>%select(state,quarter,product_name,counter)%>%
  arrange(state)
## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- final_df_1

#---------------------------------------------------------------------------------------------
####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu?

original<-df_orders%>%mutate(m = month(as.Date(order_date)))%>%filter(shipped_date=="NULL")

final_df_2<- original%>%group_by(m)%>%summarise(perc = n()/nrow(df_orders))



## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- final_df_2

#---------------------------------------------------------------------------------------------
####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

x<- merge(df_order_items,df_orders,by="order_id")

x<- merge(x,df_products,by="product_id")
x<- x%>%mutate(income = (1-discount)*list_price.x*quantity)
x<-x%>%group_by(product_name)%>%summarise(suma = sum(income))%>%
  arrange(desc(suma))%>%nth(1)
final_df_3 <-as.character(x[1,1])
## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- final_df_3

#---------------------------------------------------------------------------------------------
####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

final_df_4<-df_orders%>%mutate(year = year(as.Date(order_date)))%>%
  group_by(customer_id,year)%>%mutate(no_of_orders=n())%>%select(customer_id,year,no_of_orders)%>%
  arrange(desc(no_of_orders))%>%ungroup()%>%group_by(year,no_of_orders)%>%summarise(counter=n())%>%
  filter(no_of_orders==max(no_of_orders))


  

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- final_df_4

#---------------------------------------------------------------------------------------------
####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

final_df_5<-merge(df_orders,df_customers,by="customer_id",suffixes = c("",""))%>%
  mutate(year=year(as.Date(order_date)))%>%
  select(year,email)%>%
  mutate(domain = str_extract(email, "(?<=@)[^.]+"))%>%
  group_by(domain,year)%>%summarise(n=n())%>%filter(n==max(n))




## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- final_df_5
#---------------------------------------------------------------------------------------------
####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?
#odp: TAK:
x<-merge(df_customers,df_orders,by="customer_id")%>%
  filter(state=="TX" | state=="CA")%>%mutate(year = year(as.Date(order_date)))%>%
  group_by(customer_id)%>%summarise(res = any(year==2018))%>%
  filter(res==FALSE)%>%select(customer_id)%>%distinct()
nrow(x)

  

final_df_6<-merge(df_customers,df_orders,by="customer_id")%>%
  filter(state=="TX" | state=="CA")%>%
  group_by(state)%>%summarise(no_clients = n_distinct(customer_id))



## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- final_df_6

#---------------------------------------------------------------------------------------------
####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
x<- merge(df_order_items,df_orders,by="order_id")


x<- merge(x,df_products,by="product_id")
x<- x%>%mutate(income = (1-discount)*list_price.x*quantity)

x<-x%>%group_by(order_id)%>%mutate(quan_005 = quantile(income,0.05),
                                   quan_095 = quantile(income,0.95))


final_df_7<- x%>%mutate(check = ifelse(income<quan_005 | income>quan_095,TRUE,FALSE))%>%
  filter(check==TRUE)%>%ungroup()%>%select(customer_id)%>%distinct()

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- final_df_7

#---------------------------------------------------------------------------------------------
####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

final_df_8 <-df_orders%>%mutate(quarter = quarter(as.Date(order_date)))%>%group_by(order_date)%>%
  mutate(per_day=n())%>%ungroup()%>%group_by(quarter)%>%mutate(per_quarter=sum(per_day),
                                                               min_per_quater=min(per_day),
                                                               max_per_quater=max(per_day),
                                                               median_per_quater = median(per_day))%>%
  select(quarter,min_per_quater,max_per_quater,median_per_quater)%>%distinct()



## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- final_df_8

#---------------------------------------------------------------------------------------------
####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

x<-merge(df_orders,df_customers,by="customer_id",suffixes = c("",""))

x<-x%>%select(state,order_date,shipped_date)%>%filter(shipped_date!="NULL")
x<-x%>%mutate(difference = as.numeric(str_extract(as.Date(shipped_date)-as.Date(order_date), "\\d+")))%>%
  mutate(year = year(as.Date(shipped_date)))

x<-x%>%group_by(year,state)%>%summarise(sredni_czas = mean(difference))

df_final_9 <- pivot_wider(x, names_from = state, values_from = sredni_czas)
## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_final_9

#---------------------------------------------------------------------------------------------
####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

x<-df_orders%>%mutate(year = year(as.Date(order_date)))%>%group_by(customer_id)%>%
  mutate(each_year = n_distinct(year))%>%
  filter(each_year==3)
x<- x%>%merge(df_customers,by="customer_id")%>%select(last_name)
final_df_10<- x%>%mutate(first_letter = substring(last_name,1,1))%>%group_by(first_letter)%>%
  summarise(no=n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- final_df_10

#---------------------------------------------------------------------------------------------
####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

x <- merge(df_orders,df_customers,by="customer_id")
x<- merge(x,df_order_items,by="order_id")
x<- merge(x,df_products,by="product_id")
x<-merge(x,df_categories,by="category_id")
x<-x%>%group_by(customer_id,category_name)%>%summarise(no_bikes=n())
final_df_11<- pivot_wider(x,names_from=category_name,values_from = no_bikes,values_fill = 0)
## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- final_df_11

#---------------------------------------------------------------------------------------------
### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

final_df_12<-df_orders%>%merge(df_order_items,by="order_id")%>%mutate(day = weekdays(as.Date(order_date)))%>%
  mutate(real_income = list_price*quantity,
         discount_income = (1-discount)*list_price*quantity)%>%
  group_by(day)%>%summarise(sum_real_income = sum(real_income),
                            sum_discount_income = sum(discount_income),
                            avg = (sum_real_income-sum_discount_income)/sum_real_income*100)%>%
  select(day,avg)

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- final_df_12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "ZabkowskiBartosz.rds")
