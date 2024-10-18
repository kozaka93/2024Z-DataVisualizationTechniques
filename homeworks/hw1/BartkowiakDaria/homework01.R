library(dplyr)
library(tidyr)


df_orders <- read.csv('/Users/daria1942/Desktop/hw1/orders.csv')
df_order_items <- read.csv('/Users/daria1942/Desktop/hw1/order_items.csv')
df_products <- read.csv('/Users/daria1942/Desktop/hw1/products.csv')
df_brands <- read.csv('/Users/daria1942/Desktop/hw1/brands.csv')
df_categories <-  read.csv('/Users/daria1942/Desktop/hw1/categories.csv')
df_customers <- read.csv('/Users/daria1942/Desktop/hw1/customers.csv')



####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_order_items %>%
  inner_join(df_products, by = 'product_id') %>%
  inner_join(df_orders, by= 'order_id') %>%
  inner_join(df_customers, by= 'customer_id') %>% 
  select(product_id, product_name, order_date, state) %>%
  mutate(quarter = (as.numeric(format(as.Date(order_date), "%m")) - 1) %/% 3 + 1) %>%
  group_by(product_name, quarter, state) %>%
  summarise(quantity = n()) %>%
  group_by(quarter, state) %>%
  slice(which.max(quantity)) %>%
  select(quarter, state, product_name)
  
####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 


## Odpowiedz przypisana do zmiennej

ANS_TASK_02 <- df_orders %>%
  mutate(month = as.integer(format(as.Date(order_date), "%m"))) %>%
  group_by(month) %>%
  summarise(null_count = sum(shipped_date == 'NULL'), total_orders = n(), .groups = 'drop') %>%
  mutate(percentage_of_unrealized = null_count/total_orders *100) %>%
  select(month, percentage_of_unrealized)




####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>%
  inner_join(df_orders, by = 'order_id') %>%
  mutate(income = list_price*quantity*(1 - discount)) %>%
  mutate(year = as.integer(format(as.Date(order_date), "%y"))) %>%
  group_by(year, product_id) %>%
  summarise(total_income = sum(income)) %>%
  group_by(year) %>%
  slice(which.max(total_income)) %>%
  select(year, product_id)


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_customers %>%
  inner_join(df_orders, by = 'customer_id') %>%
  mutate(year = as.integer(format(as.Date(order_date), "%y"))) %>%
  group_by(year, customer_id) %>%
  summarise(orders_quantity = n()) %>%
  group_by(year, orders_quantity) %>% 
  summarise(number_of_clients = n()) %>%
  group_by(year) %>%
  slice(which.max(orders_quantity))


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_customers %>%
  inner_join(df_orders, by = 'customer_id') %>%
  mutate(year = as.integer(format(as.Date(order_date), "%y"))) %>%
  mutate(domain = sub(".*@", "", email)) %>%
  group_by(year, domain) %>%
  summarise(orders_quantity = n()) %>%
  group_by(year) %>%
  slice(which.max(orders_quantity))


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej
active <- df_orders %>%
  left_join(df_customers, by = 'customer_id') %>%
  group_by(customer_id) %>%
  group_by(state) %>%
  summarise(active_clients = n()) %>%
  filter(state == 'CA' | state == 'TX')
active_2018 <- df_customers %>%
  inner_join(df_orders, by = 'customer_id') %>%
  mutate(year = as.integer(format(as.Date(order_date), "%y"))) %>%
  filter(year == '18') %>%
  right_join(df_customers, by = 'customer_id')
non_active <- df_customers %>%
  left_join(active_2018, by= 'customer_id') %>%
  filter(is.na(order_id)) %>%
  filter(state == 'TX' | state == 'CA') %>%
  group_by(state) %>%
  summarise(without_order_2018 = n())
  
ANS_TASK_06 <- active %>%
  left_join(non_active, by = 'state')

####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej


df_orders_values <- df_order_items %>%
  mutate(order_value = quantity * (1 - discount) * list_price) %>%
  group_by(order_id) %>%
  summarise(total_order_value = sum(order_value), .groups = 'drop')

quantiles <- quantile(df_orders_values$total_order_value, probs = c(0.05, 0.95), na.rm = TRUE)
ANS_TASK_07 <- df_orders_values %>%
  filter(total_order_value < quantiles[1] | total_order_value > quantiles[2]) %>%
  left_join(df_orders, by = 'order_id') %>%
  select(customer_id, total_order_value)

####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>%
  mutate(quarter = (as.numeric(format(as.Date(order_date), "%m")) - 1) %/% 3 + 1) %>%
  group_by(order_date, quarter) %>%
  summarise(order_count = n()) %>%
  group_by(quarter) %>%
  summarise(max_orders = max(order_count), min_orders = min(order_count), median_orders = median(order_count), .groups = 'drop')



####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>%
  mutate(order_date = as.Date(order_date)) %>%
  mutate(shipped_date = as.Date(shipped_date)) %>%
  mutate(delivery_time = as.numeric(shipped_date - order_date),year = as.integer(format(as.Date(order_date), "%y"))) %>%
  filter(!is.na(delivery_time)) %>%
  inner_join(df_customers, by = 'customer_id') %>%
  group_by( year, state) %>%
  summarise( mean_time = mean(delivery_time)) %>%
  pivot_wider(names_from = state, values_from = mean_time)



####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_orders %>%
  mutate(year = as.integer(format(as.Date(order_date), "%y"))) %>%
  group_by(customer_id) %>%
  summarise(years_ordered = n_distinct(year), .groups = 'drop') %>%
  filter(years_ordered == 3) %>%
  left_join(df_customers , by = 'customer_id') %>%
  mutate(first_letter = substr(last_name, 1, 1)) %>%
  group_by(first_letter) %>%
  summarise(letter_count = n())
  

####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej

bikes <- sort(unique(df_products$category_id))
bikes_quantity <- df_orders %>%
  left_join(df_customers, by = 'customer_id') %>%
  left_join(df_order_items, by = 'order_id') %>%
  left_join(df_products, by = 'product_id') %>%
  group_by(customer_id, category_id) %>%
  summarise(quantity = n())

ANS_TASK_11 <- bikes_quantity %>%
  pivot_wider(names_from = category_id, values_from = quantity, values_fill = 0)


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_order_items %>%
  mutate(dis_income = quantity * list_price * (1 - discount), normal_income = quantity * list_price, dis_value = (normal_income - dis_income) / normal_income * 100) %>%
  left_join(df_orders, by = 'order_id') %>%
  mutate(day_of_week = format(as.Date(order_date), "%A")) %>%
  group_by(product_id, day_of_week) %>%
  summarise(mean_dis = mean(dis_value)) %>%
  arrange(product_id, day_of_week)

### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "BartkowiakDaria.rds")
