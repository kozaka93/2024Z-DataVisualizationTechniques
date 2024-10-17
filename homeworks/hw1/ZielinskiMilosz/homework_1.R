library(dplyr)
library(tidyr)
library(stringr)
#homeworks/hw1/dane/

df_orders <- read.csv('orders.csv')
df_order_items <- read.csv('order_items.csv')
df_products <- read.csv('products.csv')
df_brands <- read.csv('brands.csv')
df_categories <-  read.csv('categories.csv')
df_customers <- read.csv('customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_order_items %>% 
  left_join(df_orders, by = "order_id") %>% 
  left_join(df_customers, by = "customer_id") %>% 
  mutate(year = as.numeric(format(as.Date(order_date), "%Y")), month = as.numeric(format(as.Date(order_date), "%m")), day = as.numeric(format(as.Date(order_date), "%d"))) %>% 
  mutate(quarter = case_when(
    month <= 3 ~ 1,
    month <= 6 ~ 2,
    month <= 9 ~ 3,
    TRUE ~ 4
  )) %>% 
  select(product_id,quantity,year,quarter,state) %>% 
  group_by(year,quarter,state, product_id) %>% 
  summarise(suma = sum(quantity)) %>% 
  group_by(year,quarter,state) %>% 
  slice_max(suma) %>% 
  left_join(df_products, by = 'product_id') %>% 
  select(year,quarter, state, suma, product_name, model_year)
  


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej

pomocnicza_2 <- df_orders %>% 
  mutate(year = format(as.Date(order_date), "%Y"), month = format(as.Date(order_date), "%m")) %>% 
  group_by(year,month) %>% 
  summarise(counter = n()) 

pomocnicza_22 <- df_orders %>% 
  mutate(year = format(as.Date(order_date), "%Y"), month = format(as.Date(order_date), "%m")) %>% 
  filter(order_status == 4) %>% 
  group_by(year,month) %>% 
  summarise(counter = n(), .groups = 'drop') %>% 
  complete(year, month, fill = list(counter = 0))

ANS_TASK_02 <- pomocnicza_2 %>% 
  right_join(pomocnicza_22, by = c('year','month')) %>% 
  mutate(percentage = (100-counter.y/counter.x*100)) %>% 
  select(year,month,percentage) %>% 
  arrange(year,month)
  
  
  
####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
order_year <- df_orders %>% 
  mutate(year = format(as.Date(order_date), "%Y")) %>% 
  select( order_id,year)

ANS_TASK_03 <- df_order_items %>% 
  mutate(price = quantity * list_price * (1- discount)) %>% 
  inner_join(order_year, by = 'order_id') %>% 
  group_by(year, product_id) %>% 
  summarise(income = sum(price)) %>%  
  filter(income == max(income)) %>% 
  left_join(df_products,by = 'product_id') %>% 
  select(year,product_name)
  


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% 
  mutate(year = format(as.Date(order_date), "%Y")) %>% 
  group_by(year, customer_id) %>% 
  summarise(order_counter = n()) %>% 
  filter(order_counter == max(order_counter)) %>% 
  group_by(year, order_counter) %>% 
  summarise(customer_counter = n())


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej


ANS_TASK_05 <- df_orders %>% 
  mutate(year = format(as.Date(order_date), "%Y")) %>% 
  inner_join(df_customers, by = 'customer_id') %>% 
  mutate(domain = str_extract(email, "(?<=@).+")) %>% 
  group_by(year, domain) %>% 
  summarise(number_of_orders = n()) %>% 
  filter(number_of_orders == max(number_of_orders))
  
####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej
Inactive_2018 <- df_orders %>% 
  mutate(year = format(as.Date(order_date), "%Y")) %>% 
  filter(year == 2018) %>% 
  right_join(df_customers) %>% 
  filter(is.na(order_date) & (state!='NY')) %>% 
  group_by(state) %>% 
  summarise(inactive_in_2018 = n())
  
  
  
ANS_TASK_06 <- df_orders %>% 
  left_join(df_customers, by = "customer_id") %>% 
  filter(state == "CA" | state == "TX") %>% 
  mutate(year = format(as.Date(order_date), "%Y")) %>% 
  select(customer_id, year, state) %>% 
  group_by(state) %>% 
  summarise(Active_clients = n_distinct(customer_id)) %>% 
  left_join(Inactive_2018, by = "state")


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
quantile_5 <- df_orders %>% 
  inner_join(df_order_items, by = 'order_id') %>% 
  mutate(price = (list_price-discount*list_price)*quantity) %>% 
  group_by(order_id) %>% 
  summarise(order_price = sum(price)) %>% 
  summarise(quantile(order_price, 0.05)) %>% 
  as.numeric()

quantile_95 <- df_orders %>% 
  inner_join(df_order_items, by = 'order_id') %>% 
  mutate(price = (list_price-discount*list_price)*quantity) %>% 
  group_by(order_id) %>% 
  summarise(order_price = sum(price)) %>% 
  summarise(quantile(order_price, 0.95)) %>% 
  as.numeric()
## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_orders %>% 
  inner_join(df_order_items, by = 'order_id') %>% 
  mutate(price = (list_price-discount*list_price)*quantity) %>% 
  inner_join(df_customers, by = 'customer_id') %>% 
  group_by(order_id, customer_id, first_name, last_name) %>% 
  summarise(order_price = sum(price), .groups = 'drop')%>% 
  filter(order_price < quantile_5 | order_price > quantile_95) %>% 
  distinct(customer_id, last_name, first_name )


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej
pomocnicza_8 <- data.frame(date = seq.Date(from = as.Date("2016-01-01"), to = as.Date("2018-12-31"), by = "day")) %>% 
  mutate(year = as.numeric(format(as.Date(date), "%Y")), month = as.numeric(format(as.Date(date), "%m")), day = as.numeric(format(as.Date(date), "%d"))) %>% 
  select(year,month,day)

ANS_TASK_08 <- df_orders %>% 
  mutate(year = as.numeric(format(as.Date(order_date), "%Y")), month = as.numeric(format(as.Date(order_date), "%m")), day = as.numeric(format(as.Date(order_date), "%d"))) %>% 
  group_by(year,month, day) %>% 
  summarise(number_of_orders = n()) %>% 
  right_join(pomocnicza_8, by = c('year'='year', 'month'='month', 'day'='day' )) %>% 
  mutate(number_of_orders = replace_na(number_of_orders, 0)) %>% 
  mutate(quarter = case_when(
    month <= 3 ~ 1,
    month <= 6 ~ 2,
    month <= 9 ~ 3,
    TRUE ~ 4
  )) %>% 
  arrange(year,quarter,month, day) %>% 
  group_by(year,quarter) %>% 
  summarise(max=max(number_of_orders),min=min(number_of_orders),median=median(number_of_orders))

####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>% 
  filter(order_status==4) %>% 
  left_join(df_customers, by = "customer_id") %>% 
  select(order_date, shipped_date, state) %>% 
  mutate(shipping_time_days = as.numeric(difftime(shipped_date, order_date)), year = as.numeric(format(as.Date(order_date), "%Y"))) %>% 
  group_by(year,state) %>% 
  summarise(shipping_days_mean = mean(shipping_time_days)) %>% 
  pivot_wider(names_from = state, values_from = shipping_days_mean)
  

####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej

ANS_TASK_10 <- df_orders %>% 
  mutate(year = as.numeric(format(as.Date(order_date), "%Y"))) %>% 
  group_by(customer_id) %>% 
  mutate(number_of_years = n_distinct(year)) %>% 
  filter(number_of_years ==3) %>% 
  distinct(customer_id) %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  select(last_name) %>% 
  mutate(first_letter = substr(last_name,1,1)) %>% 
  group_by(first_letter) %>% 
  summarise(count = n())
  

####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_orders %>% 
  right_join(df_order_items,  by = 'order_id') %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  left_join(df_products, by = 'product_id') %>% 
  mutate(year = as.numeric(format(as.Date(order_date), "%Y")), new = case_when(year == model_year ~ 1, TRUE ~ 0)) %>% 
  select(customer_id, category_id, quantity, new) %>%  
  group_by(customer_id, category_id) %>% 
  summarise(ile = sum(quantity), nowe = sum(new*quantity)) %>% 
  left_join(df_categories,by = 'category_id') %>% 
  select(-category_id) %>% 
  pivot_wider(names_from = category_name, values_from = ile,values_fill = 0) %>% 
  relocate('Cruisers Bicycles', .before='Cyclocross Bicycles') %>%   
  group_by(customer_id) %>% 
  summarise(nowe=sum(nowe), across(starts_with("Children Bicycles"):starts_with("Road Bikes"), sum, na.rm = TRUE))




### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_order_items %>% 
  left_join(df_orders, by = "order_id") %>% 
  mutate(weekdays = weekdays(as.Date(order_date))) %>% 
  select(product_id,quantity, discount, weekdays) %>% 
  group_by(weekdays, product_id) %>% 
  summarise(mean_discount = weighted.mean(discount,quantity)) %>% 
  pivot_wider(names_from = weekdays, values_from =  mean_discount) %>% 
  left_join(df_products, by = 'product_id') %>% 
  select(product_name,Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) 
  


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "ZielinskiMilosz.rds")
