library(dplyr)
library(tidyr)
library(lubridate)


df_orders <- read.csv('Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/products.csv')
df_brands <- read.csv('Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/customers.csv')
df_staffs <- read.csv('Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/staffs.csv')
df_stocks <- read.csv('Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/stocks.csv')
df_stores <- read.csv('Documents/uni/sem_3/twd/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/stores.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

df <- df_order_items %>%
  inner_join(df_orders, by = "order_id") %>%
  inner_join(df_products, by = "product_id") %>%
  inner_join(df_customers, by = "customer_id")
df <- df %>%
  mutate(order_month = as.numeric(strftime(order_date, "%m")),
         order_year = strftime(order_date, "%Y"),
         order_quarter = ifelse(order_month %in% c(1, 2, 3), 1,
                                ifelse(order_month %in% c(4, 5, 6), 2,
                                       ifelse(order_month %in% c(7, 8, 9), 3, 4))),
         year_quarter = paste0(order_year, "Q", order_quarter))
most_ordered_products <- df %>%
  group_by(state, year_quarter, product_name, model_year) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(state, year_quarter) %>%
  slice_max(order_by = count, n = 1) %>%
  ungroup()

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- most_ordered_products


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

df_orders <- df_orders %>% 
  mutate(order_month = strftime(order_date, "%Y-%m"))

failed_orders <- df_orders %>% 
  group_by(order_month) %>% 
  summarise(
    total_orders = n(),
    unfulfilled_orders = sum(order_status != 4) 
  ) %>% 
  mutate(percentage_unfulfilled = (unfulfilled_orders/total_orders) * 100) %>% 
  select(order_month, percentage_unfulfilled)
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- failed_orders


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?


df_best_income <- df_orders %>% filter(shipped_date != "NULL") %>% 
  mutate(year = strftime(order_date, "%Y")) %>% 
  inner_join(df_order_items, by = "order_id") %>% 
  select(order_date, quantity, discount, product_id, year) %>% 
  inner_join(df_products, by = "product_id") %>% 
  select(order_date, quantity, list_price, discount, product_name, product_id, year) %>% 
  mutate(revenue = list_price*(1-discount)*quantity) %>%
  group_by(product_id, year) %>% 
  summarise(product_name = first(product_name), revenue = sum(revenue), .groups = "drop") %>% 
  group_by(year) %>% filter(revenue == max(revenue)) %>% 
  ungroup() %>% select(-product_id)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_best_income


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

df_orders <- df_orders %>% 
  mutate(order_year = strftime(order_date, "%Y"))

df_biggest_orders <- df_orders %>% 
  group_by(order_year, customer_id) %>%
  summarise(total_orders = n(), .groups = "drop") %>% 
  arrange(order_year, desc(total_orders)) %>% 
  group_by(order_year) %>% 
  slice_max(order_by = total_orders, n = 1) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  group_by(order_year, total_orders) %>% 
  summarise(number_of_clients = n(), .groups = "drop") %>% 
  select(order_year, number_of_clients, total_orders)


## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_biggest_orders


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

df_orders <- df_orders %>% 
  mutate(order_year = strftime(order_date, "%Y"))

df_mails <- df_orders %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  mutate(email_domain = sub(".*@", "", email)) %>% 
  group_by(order_year, email_domain) %>% 
  summarise(total_orders = n(), .groups = "drop") %>% 
  arrange(order_year, desc(total_orders)) %>% 
  group_by(order_year) %>% 
  slice_max(order_by = total_orders, n = 1) %>% 
  select(order_year, email_domain, total_orders)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_mails


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

df_customers_filtered <- df_customers %>%
  filter(state %in% c("CA", "TX"))
df_merged <- df_customers_filtered %>%
  left_join(df_orders, by = "customer_id") %>%
  distinct(customer_id, .keep_all = TRUE) %>%  
  mutate(order_year = as.numeric(strftime(order_date, "%Y")))
active_clients <- df_merged %>%
  filter(!is.na(order_id)) %>%
  group_by(state) %>%
  summarise(active_clients = n_distinct(customer_id))
not_ordered_2018 <- df_merged %>%
  filter(order_year != 2018) %>%
  group_by(state) %>%
  summarise(not_ordered_2018 = n_distinct(customer_id))
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- active_clients %>%
  left_join(not_ordered_2018, by = "state")


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

df_order_values <- df_orders %>%
  inner_join(df_order_items, by = "order_id") %>%
  inner_join(df_products, by = "product_id") %>%
  mutate(order_value = quantity * list_price.x * (1 - discount)) %>%
  group_by(order_id, customer_id) %>%
  summarise(total_order_value = sum(order_value), .groups = 'drop')

percentile_5 <- quantile(df_order_values$total_order_value, 0.05)
percentile_95 <- quantile(df_order_values$total_order_value, 0.95)

df_extreme_orders <- df_order_values %>%
  filter(total_order_value < percentile_5 | total_order_value > percentile_95)

df_extreme_customers <- df_extreme_orders %>%
  inner_join(df_customers, by = "customer_id") %>%
  select(customer_id, first_name, last_name, total_order_value)

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_extreme_customers


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
df_orders <- df_orders %>% 
  mutate(order_month = as.numeric(strftime(order_date, "%m")),
         order_year = strftime(order_date, "%Y"),
         order_quarter = ifelse(order_month %in% c(1, 2, 3), 1,
                                ifelse(order_month %in% c(4, 5, 6), 2,
                                       ifelse(order_month %in% c(7, 8, 9), 3, 4))),
         year_quarter = paste0(order_year, "Q", order_quarter))
df_orders_daily <- df_orders %>%
  group_by(order_date, year_quarter) %>%
  summarise(orders_per_day = n())  
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders_daily %>%
  group_by(year_quarter) %>%
  summarise(
    max = max(orders_per_day),   
    min = min(orders_per_day),   
    median = median(orders_per_day)  
  )

####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

df_delivery_time <- df_orders %>%
  mutate(order_date = as.Date(order_date),
         shipped_date = as.Date(shipped_date)) %>%
  mutate(delivery_time = as.numeric(shipped_date - order_date)) %>%
  mutate(order_year = as.numeric(format(order_date, "%Y"))) %>%
  left_join(df_customers, by = "customer_id") %>%
  group_by(order_year, state) %>%
  summarise(avg_delivery_time = mean(delivery_time, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = state, values_from = avg_delivery_time, 
              names_prefix = "avg_delivery_time_")

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_delivery_time

####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

customers_with_orders <- df_orders %>%
  mutate(order_year = as.numeric(strftime(order_date, "%Y"))) %>%
  group_by(customer_id) %>%
  summarise(years_count = n_distinct(order_year), .groups = "drop") %>%
  filter(years_count == length(unique(strftime(df_orders$order_date, "%Y"))))

active_customers <- df_customers %>%
  filter(customer_id %in% customers_with_orders$customer_id)

library(stringr)

first_letter_frequency <- active_customers %>%
  mutate(first_letter = substr(last_name, 1, 1)) %>%
  group_by(first_letter) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

every_letter_frequency <- active_customers %>%
  summarise(all_last_names = str_c(last_name, collapse = "")) %>%
  mutate(letters_vector = str_split(all_last_names, "")) %>%
  unnest(letters_vector) %>%
  group_by(letters_vector) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

## Odpowiedz przypisana do zmiennej

# ile raz te litery występowały na początku nazwisk
ANS_TASK_10 <- first_letter_frequency # + every_letter_frequency?


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

df_orders_with_latest <- df_orders %>%
  inner_join(df_order_items, by = "order_id") %>%
  inner_join(df_customers, by = "customer_id") %>%
  inner_join(df_products, by = "product_id") %>%
  mutate(order_year = as.numeric(strftime(order_date, "%Y")),
         is_latest_product = ifelse(order_year == model_year, 1, 0))
df_summary <- df_orders_with_latest %>%
  group_by(customer_id, first_name, last_name, category_id) %>%
  summarise(bikes_purchased = sum(quantity),
            latest_product_purchased = sum(is_latest_product),
            .groups = "drop") 
df_wide_summary <- df_summary %>%
  pivot_wider(names_from = category_id,
              values_from = bikes_purchased,
              values_fill = 0,
              names_prefix = "bike_category_")
df_wide_summary <- df_wide_summary %>% 
  group_by(customer_id, first_name, last_name) %>%
  summarise(
    latest_product_purchased = sum(latest_product_purchased, na.rm = TRUE),
    across(starts_with("bike_category_"), sum, na.rm = TRUE),
    .groups = 'drop'
  )

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_wide_summary


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

df_orders_items <- df_order_items %>%
  inner_join(df_orders, by = "order_id")

df_discount_summary <- df_orders_items %>%
  mutate(order_day_of_week = weekdays(as.Date(order_date))) %>%
  mutate(revenue_full_price = list_price * quantity,
         revenue_after_discount = revenue_full_price * (1 - discount)) %>%
  group_by(product_id, order_day_of_week) %>%
  summarise(avg_discount = mean((revenue_full_price - revenue_after_discount) / revenue_full_price, na.rm = TRUE), .groups = "drop") %>%
  mutate(avg_discount_percentage = avg_discount * 100) %>%
  select(-avg_discount)


## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_discount_summary



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "LahunovichYahor.rds")
