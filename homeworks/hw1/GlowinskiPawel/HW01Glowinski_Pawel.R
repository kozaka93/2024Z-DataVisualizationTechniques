library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_order_items %>% 
  inner_join(df_orders,by="order_id") %>% 
  inner_join(df_customers,by="customer_id") %>%
  inner_join(df_products,by="product_id") %>% 
  mutate(year = strftime(order_date,"%Y"),month = strftime(order_date,"%m"),quarter = case_when(month%in%c("01","02","03")~"Q1",
                                                                                                month%in%c("04","05","06")~"Q2",
                                                                                                month%in%c("07","08","09")~"Q3",
                                                                                                month%in%c("10","11","12")~"Q4")) %>% 
  group_by(state,quarter,year,product_id) %>% 
  mutate(qsum=sum(quantity)) %>% 
  distinct(state,year,quarter,qsum,product_name,model_year) %>% 
  group_by(state,year,quarter)%>%
  filter(qsum==max(qsum)) %>% 
  select(year,quarter,state,product_name,model_year)
  


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>%
  mutate(Date = strftime(order_date,"%Y-%m"),rejected = case_when(order_status==3~1,order_status%in%c(1,2,4)~0)) %>% 
  group_by(Date) %>%
  summarise(rejected_count = sum(rejected==1),total_count=n()) %>% 
  mutate(percent_rejected = rejected_count/total_count*100) %>% 
  select(Date,percent_rejected)
  
  

####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_orders %>% 
  left_join(df_order_items,by = "order_id") %>%
  mutate(Year = strftime(order_date,"%Y")) %>%
  group_by(Year,product_id) %>% 
  mutate(income = sum(quantity*list_price*(1-discount))) %>%
  group_by(Year) %>% 
  mutate(max_income = max(income))%>%
  filter(max_income == income) %>%
  left_join(df_products,by = "product_id") %>% 
  distinct(Year,product_name)
  
  

####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% 
  inner_join(df_customers,by = "customer_id") %>% 
  mutate(Year = strftime(order_date,"%Y")) %>%
  group_by(Year,customer_id) %>% 
  summarise(orders_quantity = n()) %>% 
  group_by(Year,orders_quantity) %>% 
  summarise(customers_quantity = n()) %>%
  filter(orders_quantity == max(orders_quantity))
  

####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_customers %>% 
  inner_join(df_orders,by = "customer_id") %>% 
  mutate(Year = strftime(order_date,"%Y"),domain = sub(".*@","", email)) %>% 
  group_by(Year,domain) %>% 
  summarise(orders_quantity = n()) %>% 
  filter(orders_quantity == max(orders_quantity))
  
  

####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej

active_2018 <- df_customers %>% 
  inner_join(df_orders,by="customer_id") %>% 
  mutate(Year = strftime(order_date,"%Y")) %>% 
  filter(Year%in%c(2018) & state%in%c("TX","CA")) %>% 
  distinct(customer_id,state) %>% 
  group_by(state) %>% 
  summarise(active_2018 = n())

ANS_TASK_06 <- df_customers %>% 
  inner_join(df_orders,by = "customer_id") %>% 
  filter(state%in%c("TX","CA")) %>% 
  distinct(state,customer_id) %>%
  group_by(state) %>% 
  summarise(active_customers = n()) %>% 
  inner_join(active_2018,by="state") %>% 
  mutate(non_active_2018 = active_customers-active_2018) %>% 
  select(state,active_customers,non_active_2018)


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej

orders_value<- df_order_items %>%
  mutate(value = quantity*list_price*(1-discount)) %>% 
  group_by(order_id) %>% 
  summarise(total_value = sum(value))
  

ANS_TASK_07 <- orders_value %>% 
  inner_join(df_orders,by="order_id") %>% 
  filter(total_value<quantile(orders_value$total_value,0.05) | total_value>quantile(orders_value$total_value,0.95)) %>% 
  distinct(customer_id) %>% 
  inner_join(df_customers,by="customer_id") %>% 
  select(customer_id,first_name,last_name)
  


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>% 
  mutate(year = strftime(order_date,"%Y"),month = strftime(order_date,"%m"),quarter = case_when(month%in%c("01","02","03")~"Q1",
                                                                                                month%in%c("04","05","06")~"Q2",
                                                                                                month%in%c("07","08","09")~"Q3",
                                                                                                month%in%c("10","11","12")~"Q4"),day = strftime(order_date,"%d")) %>% 
  group_by(year,month,quarter,day) %>% 
  summarise(count = n()) %>% 
  group_by(year,quarter) %>% 
  summarise(max_orders = max(count),min_orders = min(count),median_orders = median(count))
  
  

####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>% 
  inner_join(df_customers,by="customer_id") %>% 
  mutate(Year = strftime(order_date,"%Y"),shipping_time = as.numeric(as.Date(shipped_date)-as.Date(order_date))) %>% 
  group_by(Year,state) %>% 
  summarise(average_time = mean(shipping_time,na.rm = TRUE)) %>% 
  pivot_wider(names_from = state,values_from = average_time)
  
  
####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej
in_2016 <- df_orders %>% 
  mutate(Year = strftime(order_date,"%Y")) %>% 
  group_by(customer_id) %>% 
  filter(Year == 2016)

in_2017 <- df_orders %>% 
  mutate(Year = strftime(order_date,"%Y")) %>% 
  group_by(customer_id) %>% 
  filter(Year == 2017)

in_2018 <- df_orders %>% 
  mutate(Year = strftime(order_date,"%Y")) %>% 
  group_by(customer_id) %>% 
  filter(Year == 2018)
  
ANS_TASK_10 <- in_2016 %>% 
  inner_join(in_2017,by="customer_id") %>% 
  inner_join(in_2018,by="customer_id") %>% 
  inner_join(df_customers,by="customer_id") %>% 
  mutate(first_letter = substr(last_name,1,1)) %>% 
  group_by(first_letter) %>% 
  summarise(letter_count = n())
  
  
####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej

same_year <- df_products %>% 
  left_join(df_order_items,by="product_id") %>% 
  left_join(df_orders,by="order_id") %>% 
  mutate(Year = strftime(order_date,"%Y")) %>% 
  filter(Year == model_year) %>% 
  group_by(customer_id) %>% 
  summarise(how_many_new = n())
  

ANS_TASK_11 <- df_products %>% 
  left_join(df_order_items,by="product_id") %>% 
  left_join(df_orders,by="order_id") %>% 
  left_join(df_customers,by="customer_id") %>%
  left_join(df_categories,by="category_id") %>% 
  group_by(customer_id,category_name) %>% 
  summarise(quantity = n()) %>% 
  pivot_wider(names_from = category_name,values_from = quantity,values_fill = 0) %>% 
  inner_join(same_year,by="customer_id")


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_order_items %>% 
  inner_join(df_orders,by="order_id") %>% 
  mutate(day = strftime(order_date,"%A")) %>% 
  mutate(normal_price = list_price*quantity,discount_price = list_price*quantity*(discount),difference = (discount_price/normal_price)*100) %>% 
  group_by(product_id,day) %>% 
  summarise(mean_discount = mean(difference)) %>% 
  pivot_wider(names_from = day,values_from = mean_discount)
  

### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "GlowinskiPawel.rds")
