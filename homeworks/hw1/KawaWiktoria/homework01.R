library(dplyr)
library(tidyr)
library(stringr)

df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')
df_staffs <- read.csv('homeworks/hw1/dane/staffs.csv')
df_stocks <- read.csv('homeworks/hw1/dane/stocks.csv')
ds_stores <- read.csv('homeworks/hw1/dane/stores.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

df_orders_copied <- df_orders %>%
  mutate(order_date_month = as.numeric(format(as.Date(order_date), '%m'))) %>%
  mutate(quarter = (order_date_month - 1)%/% 3 + 1)

df_combined <- df_orders_copied %>%
  right_join(df_order_items, by = 'order_id') %>%
  left_join(df_customers, by = 'customer_id') %>%
  select(product_id, quantity, quarter, state)

result_01 <- df_combined %>%
  group_by(product_id, quarter, state) %>%
  summarise(total_quantity = sum(quantity), .groups = 'drop') %>%
  arrange(quarter, state, desc(total_quantity)) %>% 
  group_by(quarter, state) %>%
  slice(1) %>%
  ungroup() %>%
  select(product_id, quarter, state, total_quantity)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- result_01

  

####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

result_02 <- df_orders %>%
  mutate(ordered_month = as.numeric(format(as.Date(order_date), '%m')))%>%
  mutate(finished_month = as.numeric(format(as.Date(shipped_date), '%m'))) %>%
  select(finished_month, ordered_month) %>%
  group_by(ordered_month) %>%
  summarise(total_orders = n(), not_completed_orders = sum(is.na(finished_month)))%>%
  mutate(percent_of_not_completed = (not_completed_orders / total_orders) * 100) %>%
  ungroup()


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- result_02



####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

df_combined <- df_order_items %>% 
  right_join(df_orders, by = 'order_id') %>%
  mutate(order_date_year = as.numeric(format(as.Date(order_date), '%Y'))) %>%
  select(product_id, list_price, discount, order_date_year, quantity)

result_03 <- df_combined %>%
  mutate(stonks = (list_price - discount) * quantity) %>%
  group_by(product_id, order_date_year) %>%
  summarise(product_stonks = sum(stonks), .groups = 'drop') %>%
  arrange(-product_stonks) %>%
  group_by(order_date_year) %>%
  slice(1) %>%
  ungroup()



## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- result_03


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

df_combined <- df_orders %>%
  right_join(df_customers, by = 'customer_id') %>%
  mutate(order_date_year = as.numeric(format(as.Date(order_date), '%Y'))) %>%
  select(customer_id, order_date_year) 

result_04.01 <- df_combined %>%
  group_by(order_date_year, customer_id) %>%
  summarise(orders_customers = n(), .groups = 'drop')

pomocnicza <- result_04.01 %>%
  group_by(order_date_year) %>%
  summarise(max_orders = max(orders_customers), .groups = 'keep')

result_04 <- result_04.01 %>%
  right_join(pomocnicza, by = c('order_date_year', 'orders_customers' = 'max_orders')) %>%
  group_by(order_date_year, orders_customers) %>%
  summarise(most_active_customers = n(), .groups = 'drop') %>%
  select(order_date_year, most_active_customers, orders_customers)

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- result_04



####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

result_05 <- df_orders %>%
  right_join(df_customers, by = 'customer_id') %>%
  mutate(order_date_year = as.numeric(format(as.Date(order_date), '%Y'))) %>%
  select(email, order_date_year, order_date) %>%
  mutate(pomocnicze = str_split(email, pattern = '@', simplify = TRUE)[, 2]) %>%
  mutate(domena = str_split(pomocnicze, pattern = '.com', simplify = TRUE)[ ,1]) %>%
  select(order_date_year, domena) %>%
  group_by(order_date_year, domena) %>%
  summarise(number_of_orders = n(), .groups = 'drop') %>%
  arrange(-number_of_orders) %>%
  group_by(order_date_year) %>%
  slice(1)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- result_05


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

tek_cal_customers <- df_customers %>%
  filter(state == 'CA' | state == "TX") %>%
  left_join(df_orders, by = 'customer_id') %>%
  group_by(customer_id) %>%
  summarise(
    total_orders = n(),
    made_order_2018 = sum(format(as.Date(order_date), '%Y') == '2018'),
    .groups = 'drop'
  )

result_06 <- tek_cal_customers %>%
  mutate(x = nrow(tek_cal_customers)) %>%
  filter(total_orders != 0) %>%
  mutate(active_customers = nrow(tek_cal_customers)) %>%
  mutate(unactive_customes = x - active_customers)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- result_06


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

df_combined <- df_order_items %>%
  left_join(df_orders, by = 'order_id') %>%
  left_join(df_customers, by = 'customer_id') %>%
  select(order_id, quantity, list_price, discount, first_name, last_name) %>%
  mutate(row_price = quantity * (list_price - discount),
         row_full_price = quantity * list_price) %>%
  group_by(order_id, first_name, last_name) %>%
  summarise(full_list_price = sum(row_full_price), 
            full_paid_price = sum(row_price),
            .groups = 'drop') %>%
  as.data.frame()

result_07 <- df_combined %>%
  mutate(
    q5 = quantile(full_list_price, 0.05, na.rm = TRUE),
    q95 = quantile(full_list_price, 0.95, na.rm = TRUE))%>%
  filter(full_paid_price > q95 | full_paid_price < q5) %>%
  select(first_name, last_name)

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- result_07

####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

result_08 <- df_orders_copied <- df_orders %>%
  mutate(order_date_month = as.numeric(format(as.Date(order_date), '%m'))) %>%
  mutate(quarter = (order_date_month - 1)%/% 3 + 1) %>%
  group_by(quarter, order_date) %>%
  summarise(number_of_orders = n(), .groups = 'drop') %>%
  group_by(quarter) %>%
  arrange(number_of_orders) %>%
  summarise(max_orders = max(number_of_orders),
            min_order = min(number_of_orders),
            median_orders = median(number_of_orders),
            .groups = 'drop')
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- result_08


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

df_combined <- df_orders %>%
  left_join(df_customers, by = 'customer_id') %>%
  select(order_date, shipped_date, state) %>%
  mutate(
    delivery_time = as.numeric(difftime(as.Date(shipped_date), as.Date(order_date), units = "days")),
    order_year = as.numeric(format(as.Date(order_date), '%Y'))
  )

average_delivery <- df_combined %>%
  group_by(order_year, state) %>%
  summarise(mean_delivery_time = mean(delivery_time, na.rm = TRUE), .groups = 'drop')

wide_average_delivery <- average_delivery %>%
  pivot_wider(names_from = state, values_from = mean_delivery_time, values_fill = list(mean_delivery_time = NA))

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- wide_average_delivery


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

df_combined <- df_customers %>%
  right_join(df_orders, by = 'customer_id') %>%
  mutate(order_year = as.numeric(format(as.Date(order_date), '%Y'))) %>%
  group_by(customer_id) %>%
  summarise(years_active = n_distinct(order_year), .groups = 'drop') %>%
  filter(years_active == 3)

df_last_names <- df_combined %>%
  left_join(df_customers, by = 'customer_id') %>%
  pull(last_name)
  
letter_count <- data.frame(first_letters = str_sub(df_last_names, 1, 1)) %>%
  group_by(first_letters) %>%
  summarise(count = n(), .groups = 'drop')

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- letter_count


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- NA


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- NA



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "KawaWiktoria.rds")
