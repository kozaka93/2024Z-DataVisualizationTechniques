library(dplyr)
library(tidyr)
library(lubridate)


df_customers <- read.csv('homeworks/hw1/dane/customers.csv')
df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_staffs <- read.csv('homeworks/hw1/dane/staffs.csv')
df_stocks <- read.csv('homeworks/hw1/dane/stocks.csv')
df_stores <- read.csv('homeworks/hw1/dane/stores.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.
df_customers_orders <- df_customers %>%
  left_join(df_orders, by = "customer_id") %>%
  select(customer_id, state, order_date, order_id)

df_combined <- df_order_items %>%
  left_join(df_customers_orders, by = "order_id")

df_combined <- df_combined %>%
  mutate(quarter = quarter(order_date))

df_summary <- df_combined %>%
  group_by(product_id, state, quarter) %>%
  summarise(total_quantity = sum(quantity)) %>%
  ungroup()

df_top <- df_summary %>%
  group_by(state, quarter) %>%
  slice_max(total_quantity, with_ties = FALSE) %>% 
  ungroup()

df_final <- df_top %>%
  left_join(df_products, by = "product_id") %>%
  select(product_id, state, quarter, total_quantity, product_name, model_year)



## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_final


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 
df_orders_copy <- df_orders %>%
  mutate(order_date = as.Date(order_date),           
         month = floor_date(order_date, "month")) 

df_status_summary <- df_orders_copy %>%
  group_by(month) %>%
  summarise(
    total_orders = n(),                              
    successful_orders = sum(order_status == 4),        
    failed_orders = sum(order_status != 4),            
    failed_percentage = (failed_orders / total_orders) * 100
  ) %>%
  ungroup()



## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_status_summary


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

df3 <- df_order_items %>%
  left_join(df_orders, by = "order_id") %>%
  filter(order_status == 4)

df3 <- df3 %>%
  mutate(year = year(order_date)) %>%              
  group_by(year, product_id) %>%                   
  summarise(
    total_quantity = sum(quantity, na.rm = TRUE),   
    list_price = first(list_price)                   
  ) %>%
  mutate(money = total_quantity * list_price) %>%
  group_by(year) %>%
  slice_max(order_by = money, n = 1, with_ties = FALSE)
  

  


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df3


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

df4 <- df_orders %>%
  left_join(df_customers, by = "customer_id") %>%
  mutate(year = year(order_date)) %>% 
  group_by(year, customer_id) %>%
  summarise(
    total_orders = n()) %>%
  group_by(year) %>%                                 
  slice_max(order_by = total_orders, with_ties = TRUE) %>%
  left_join(df_customers, by = "customer_id") %>%
  select(year, customer_id, total_orders, first_name, last_name) %>%
  group_by(year) %>%                              
  summarise(
    unique_customers = n_distinct(customer_id),
    total_orders = first(total_orders)) 
                               
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df4


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

df5 <- df_orders %>%
  left_join(df_customers, by = "customer_id") %>%
  select(email, order_date) %>%
  mutate(year = year(order_date),                     
       domain = sub(".*@", "", email)) %>%         
  group_by(year, domain) %>%                          
  summarise(order_count = n(), .groups = 'drop') %>% 
  group_by(year) %>%                                  
  slice_max(order_by = order_count, with_ties = TRUE)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?
df6 <- df_orders %>%
  left_join(df_customers, by = "customer_id") %>%
  select(customer_id, order_date, state) %>%
  filter(state == "CA" | state == "TX") %>%
  mutate(year = year(order_date)) %>%                          
  group_by(state) %>%                                          
  summarise(
    active_clients_number = n_distinct(customer_id),          
    didnt_buy_in_2018_number = sum(!customer_id %in% 
                                     customer_id[year == 2018], 
                                   na.rm = TRUE), 
    .groups = 'drop'                                           
  )


## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- df6


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

df7 <- df_orders %>%
  left_join(df_customers, by = "customer_id") %>%
  select(customer_id, order_id) %>%
  left_join(df_order_items, by = "order_id") %>%
  select(customer_id, order_id, list_price, quantity)

df7 <- df7 %>%
  mutate(total_price = quantity * list_price)  


df7 <- df7 %>%
  group_by(customer_id) %>%                     
  summarise(total_price = sum(total_price, na.rm = TRUE), 
            .groups = 'drop')

quantiles <- quantile(df7$total_price, probs = c(0.05, 0.95), na.rm = TRUE)

df7 <- df7 %>%
  filter(total_price < quantiles[1] | total_price > quantiles[2]) %>%
  distinct(customer_id) %>%
  left_join(df_customers, by = "customer_id") %>%
  select(customer_id, first_name, last_name)
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

df8 <- df_orders %>%
  mutate(order_date = as.Date(order_date)) %>%
  group_by(year = year(order_date), quarter = quarter(order_date), order_date) %>%
  summarise(daily_orders = n(), .groups = 'drop') %>%
  group_by(year, quarter) %>%
  summarise(max_orders = max(daily_orders, na.rm = TRUE),
            min_orders = min(daily_orders, na.rm = TRUE),
            median_orders = median(daily_orders, na.rm = TRUE),
            .groups = 'drop')
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
df9 <- df_orders %>%
  left_join(df_customers, by = "customer_id") %>%
  filter(order_status == 4) %>%
  select(customer_id, order_date, shipped_date, state) %>%
  mutate(
          order_date = as.Date(order_date),      
         shipped_date = as.Date(shipped_date),
        delivery_time = as.numeric(shipped_date - order_date)) %>%
  select(customer_id, order_date, shipped_date, delivery_time, state)

df9 <- df9 %>%
  mutate(
    order_date = as.Date(order_date),
    shipped_date = as.Date(shipped_date),
    delivery_time = as.numeric(shipped_date - order_date), 
    year = year(order_date)  
  ) %>%
  group_by(year, state) %>%
  summarise(
    average_delivery_time = mean(delivery_time, na.rm = TRUE), 
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = state, 
    values_from = average_delivery_time
  )
  
  



## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

df10 <- df_orders %>%
  left_join(df_customers, by = "customer_id") %>%
  select(first_name, last_name, order_date) %>%
  group_by(customer = paste(first_name, last_name)) %>% 
  filter(all(c(2016, 2017, 2018) %in% year(order_date))) %>%
  ungroup() %>%
  mutate(first_letter = substr(last_name, 1, 1)) %>%  
  count(first_letter, sort = TRUE) 
  
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

df11 <- df_orders %>%
  inner_join(df_order_items, by = "order_id") %>%
  inner_join(df_products, by = "product_id") %>%
  inner_join(df_categories, by = "category_id") %>%
  select(customer_id, category_name) 

df11 <- df11 %>%
  group_by(customer_id, category_name) %>%  
  summarise(purchase_count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = category_name, values_from = purchase_count, values_fill = 0) 


## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
df12 <- df_orders %>%
  left_join(df_order_items, by = "order_id") %>%
  left_join(df_products, by = "product_id") %>%
  select(product_name, list_price.x, discount, order_date)

df12 <- df12 %>%
  mutate(
    order_date = as.Date(order_date),  
    day_of_week = weekdays(order_date), 
    revenue_full = list_price.x,  
    revenue_discounted = list_price.x * (1 - discount)  
  ) %>%
  mutate(
    discount_amount = revenue_full - revenue_discounted,  
    discount_percentage = discount_amount / revenue_full * 100  
  ) %>%
  group_by(product_name, day_of_week) %>% 
  summarise(avg_discount = mean(discount_percentage, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = day_of_week, values_from = avg_discount, values_fill = list(avg_discount = 0))%>%
  select(product_name, `poniedziałek`, `wtorek`, `środa`, `czwartek`, `piątek`, `sobota`, `niedziela`)


## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "ZawadaIgor.rds")
