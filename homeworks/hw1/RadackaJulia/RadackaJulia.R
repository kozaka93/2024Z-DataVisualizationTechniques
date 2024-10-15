library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')
df_staffs <- read.csv('homeworks/hw1/dane/staffs.csv')
df_stocks <- read.csv('homeworks/hw1/dane/stocks.csv')
df_stores <- read.csv('homeworks/hw1/dane/stores.csv')

df_orders$order_date <- as.Date(df_orders$order_date)

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.


customers_orders <- df_orders %>%
  inner_join(df_customers, by = "customer_id") %>% 
  select(c('order_id','customer_id','order_date','state'))

customers_orders_products <- df_order_items %>% 
  left_join(customers_orders, by = 'order_id') %>% 
  select(c('order_id','product_id','customer_id','order_date','state'))

customers_orders_products <- customers_orders_products %>% 
  mutate(month = format(order_date,"%m")) %>% 
  mutate(quarter = case_when(month >= '01' & month <= '03' ~ 1,
                             month >= '04' & month <= '06' ~ 2,
                             month >= '07' & month <= '09' ~ 3,
                             month >= '10' & month <= '12' ~ 4))
customers_orders_products <- customers_orders_products %>% 
  left_join(df_products,by = 'product_id') %>% 
  select(c('product_id','state',
           'model_year','quarter'))

df <- customers_orders_products %>%
  group_by(product_id,quarter, state) %>% 
  summarise(n()) 
 

  
df_most_sold <- df %>%
  group_by(quarter, state, product_id) %>%
  summarise(total_sales = sum(`n()`)) %>%  
  ungroup() %>%
  group_by(quarter, state) %>%
  slice_max(total_sales)


final1 <- df_most_sold %>% 
  left_join(df_products, by = 'product_id') %>% 
  select(c('quarter','state','product_name','model_year'))



## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- final1


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu?


orders_each_month <- df_orders %>% 
  mutate(month = format(order_date, "%m")) %>%
  select(c('order_id','month')) %>% 
  group_by(month) %>% 
  summarise(total_amount = n())
  
orders_not_shipped <- df_orders %>% 
  mutate(month = format(order_date, "%m")) %>%
  filter(shipped_date == 'NULL') %>% 
  group_by(month) %>% 
  summarise(not_shipped = n())
final2 <- orders_each_month %>% 
  left_join(orders_not_shipped, by = 'month') %>% 
  mutate(not_shipped = replace_na(not_shipped, 0)) %>% 
  mutate('not_realised(in %)' = not_shipped/total_amount*100) %>% 
  select(c('month','not_realised(in %)')) %>% 
  arrange(month)

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- final2


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?


df1 <- df_orders %>% 
  left_join(df_order_items, by = "order_id") %>% 
  mutate(order_year = format(order_date,"%Y")) %>% 
  mutate(total_price = list_price*quantity*(1-discount)) %>% 
  select('product_id', 'order_year', 'total_price')
  

total <- df1 %>% 
  group_by(product_id,order_year) %>% 
  summarise(total_price = sum(total_price)) %>% 
  group_by(order_year) %>% 
  slice_max(total_price)

final3 <- total %>% 
  left_join(df_products, by = 'product_id') %>% 
  select(c('order_year','product_name'))

  


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- final3


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 



customers_and_orders <- df_orders %>% 
  left_join(df_customers,by = 'customer_id') %>% 
  mutate(year = format(order_date,"%Y")) %>% 
  group_by(customer_id,year) %>% 
  summarise(total_orders = n()) %>% 
  group_by(year) %>% 
  slice_max(total_orders)

final4 <- customers_and_orders %>%
  group_by(year,total_orders) %>% 
  summarise(customer_count = n()) %>% 
  mutate(max_amount_of_orders = total_orders) %>% 
  select(-total_orders)

  

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- final4


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?



final5 <- df_orders %>% 
  left_join(df_customers,by = 'customer_id') %>% 
  mutate(most_used_domain = sub(".*@","",email),
         year = format(order_date,"%Y")) %>% 
  group_by(year,most_used_domain) %>% 
  summarise(amount_of_orders = n()) %>% 
  group_by(year) %>% 
  slice_max(amount_of_orders)
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- final5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

tx_and_ca_customers <- df_orders %>% 
  left_join(df_customers,by='customer_id') %>% 
  filter(state == 'CA' | state == 'TX') %>% 
  mutate(year = format(order_date,"%Y")) %>% 
  group_by(customer_id,year) %>% 
  summarise(each_year_orders = n())

active_customers <- tx_and_ca_customers %>% 
  group_by(customer_id) %>% 
  summarise(total_orders = n())
  
customers_in_2018 <- tx_and_ca_customers %>% 
  filter(year == 2018) %>% 
  select(customer_id)

not_active_in_2018 <- active_customers %>% 
  anti_join(customers_in_2018,by='customer_id') %>% 
  summarise(amount = n())

final6 <- active_customers %>% 
  ungroup() %>% 
  summarise(active_customers = n()) %>% 
  mutate(not_active_in_2018 = not_active_in_2018$amount)


## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- final6


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

orders <- df_orders %>% 
  left_join(df_order_items,by = 'order_id') %>% 
  mutate(value = quantity*list_price*(1-discount)) %>% 
  group_by(order_id, customer_id) %>% 
  summarize(order_value = sum(value))
  

quantiles <- quantile(orders$order_value, probs = c(0.05, 0.95))

q5 <- quantiles[1]
q95 <- quantiles[2]

final7 <- orders %>%
  filter(order_value < q5 | order_value > q95) %>% 
  ungroup() %>% 
  select(customer_id) %>% 
  distinct(customer_id) %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  select(customer_id, first_name, last_name) %>% 
  arrange(customer_id)
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- final7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.


final8 <- df_orders %>% 
  mutate(month = format(order_date,"%m")) %>% 
  mutate(quarter = case_when(month >= '01' & month <= '03' ~ 'I',
                             month >= '04' & month <= '06' ~ 'II',
                             month >= '07' & month <= '09' ~ 'III',
                             month >= '10' & month <= '12' ~ 'IV')) %>% 
  group_by(order_date,quarter) %>% 
  summarize(amount_of_orders = n()) %>% 
  group_by(quarter) %>% 
  summarize(median = median(amount_of_orders),
            min = min(amount_of_orders),
            max = max(amount_of_orders))
  



## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- final8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie


df_orders$shipped_date <- as.Date(df_orders$shipped_date)

final9 <- df_orders %>% 
  left_join(df_customers,by = 'customer_id') %>% 
  mutate(year = format(order_date,"%Y"),
         shipping_time = shipped_date - order_date) %>% 
  group_by(year,state) %>% 
  summarize(average_shipping_time = mean(shipping_time,na.rm = T)) %>% 
  pivot_wider(names_from = state,values_from = average_shipping_time)
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- final9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

customers_with_letters <- df_orders %>% 
  left_join(df_customers,by = 'customer_id') %>% 
  mutate(last_name_letter = substr(last_name,1,1),
         year = format(order_date,"%Y")) %>% 
  group_by(customer_id,last_name_letter,year) %>% 
  summarize(n())
  
custom_in_2016 <- customers_with_letters %>% 
  filter(year == 2016) 
  
custom_in_2017 <- customers_with_letters %>% 
  filter(year == 2017) 

custom_in_2018 <- customers_with_letters %>% 
  filter(year == 2018) 

exercise10 <- customers_with_letters %>% 
  inner_join(custom_in_2016, by = 'customer_id') %>% 
  inner_join(custom_in_2017, by = 'customer_id') %>% 
  inner_join(custom_in_2018, by = 'customer_id') %>% 
  group_by(last_name_letter.x) %>% 
  summarize(amount_of_occuring = n())
  
help10 <- exercise10 %>% 
  ungroup() %>% 
  summarize(sum(amount_of_occuring))

final10 <- exercise10 %>% 
  group_by(last_name_letter.x,amount_of_occuring) %>% 
  summarize('occurence(in %)' = amount_of_occuring/help10$`sum(amount_of_occuring)`*100)

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- final10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

base11 <- df_orders %>% 
  left_join(df_order_items,by = 'order_id') %>% 
  left_join(df_products,by = 'product_id') %>% 
  left_join(df_categories,by = 'category_id') %>% 
  mutate(year = format(order_date,"%Y")) %>% 
  mutate(bought_new = ifelse(model_year == year,1,0))

final11 <- base11 %>% 
  group_by(customer_id,category_name) %>% 
  summarize(amount_bought = n()) %>%
  pivot_wider(names_from = category_name, values_from = amount_bought,values_fill = 0)

inf_about_bougth_new <- base11 %>% 
  group_by(customer_id) %>% 
  summarize(new_models = sum(bought_new))



final11 <- final11 %>% 
  left_join(inf_about_bougth_new)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- final11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat


final12 <- df_orders %>% 
  left_join(df_order_items,by = 'order_id') %>% 
  left_join(df_products,by = 'product_id') %>% 
  mutate(order_day = weekdays(order_date)) %>%
  mutate(actual_price = list_price.x*(1-discount)) %>% 
  group_by(product_id,order_day) %>% 
  summarize(average_discount = sum(list_price.x - actual_price)*100/sum(list_price.x))
  


## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- final12


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "RadackaJulia.rds")

