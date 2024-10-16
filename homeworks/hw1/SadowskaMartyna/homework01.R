library(dplyr)
library(tidyr)
library(stringr)


df_orders <- read.csv('../orders.csv')
df_order_items <- read.csv('../order_items.csv')
df_products <- read.csv('../products.csv')
df_brands <- read.csv('../brands.csv')
df_categories <-  read.csv('../categories.csv')
df_customers <- read.csv('../customers.csv')
df_staffs <- read.csv('../staffs.csv')
df_stocks <- read.csv('../stocks.csv')
df_stores <- read.csv('../stores.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

df_1_new_orders <- df_orders  %>%
  mutate(year = strftime(order_date, '%Y'), month = strftime(order_date, '%m'),
         quarter = case_when(
          month < '04' ~ "Q1",
          month < '07' ~ "Q2",
          month < '10' ~ "Q3",
          TRUE ~ "Q4"
  )) %>%
  left_join(df_order_items, by = "order_id") %>% 
  left_join(df_customers, by = "customer_id") %>%
  select(order_id, customer_id, year, quarter, product_id, state, quantity)

df_1_top_products <- df_1_new_orders %>%
  group_by(product_id, year, quarter, state) %>% summarise(count = sum(quantity)) %>%
  group_by(year, quarter, state) %>% slice_max(order_by = count) %>%
  left_join(df_products, by = "product_id") %>%
  select(year, quarter, state, product_name, model_year)

df_1_top_products_2 <- df_1_new_orders %>%
  group_by(product_id, quarter, state) %>% summarise(count = n()) %>%
  group_by(quarter, state) %>% slice_max(order_by = count) %>%
  left_join(df_products, by = "product_id") %>%
  select(quarter, state, product_name, model_year)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_1_top_products


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

df_2_not_completed_orders <- df_orders %>% 
  mutate(year = strftime(order_date, '%Y'), month = strftime(order_date, '%m'), 
         not_completed = ifelse(order_status == 4, 0, 1)) %>%
  group_by(year, month) %>% summarise(percent_of_not_completed = mean(not_completed)) %>%
  mutate(percent_of_not_completed = round(percent_of_not_completed * 100, 2))

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_2_not_completed_orders


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

df_3_orders_income <- df_order_items %>%
  mutate(price = round(quantity * list_price * (1-discount), 2)) %>%
  left_join(df_orders, by = "order_id") %>%
  mutate(year = strftime(order_date, '%Y'))

df_3_max_income <- df_3_orders_income %>%
  left_join(df_products, by = "product_id") %>%
  group_by(year, product_name) %>% summarise(income = sum(price)) %>%
  slice_max(order_by = income) %>% select(year, product_name)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_3_max_income


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

df_4_order_count <- df_orders %>% mutate(year = strftime(order_date, '%Y')) %>%
  group_by(year, customer_id) %>% summarise(order_count = n()) %>%
  slice_max(order_by = order_count) %>% group_by(year, order_count) %>% 
  summarise(customers = length(customer_id))

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_4_order_count


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

df_5_top_orders <- df_orders %>% mutate(year = strftime(order_date, '%Y')) %>%
  left_join(df_customers, by = "customer_id") %>%
  mutate(domain = str_split(email, "@", simplify = TRUE)[,2]) %>%
  group_by(year, domain) %>% summarise(order_count = n()) %>%
  slice_max(order_by = order_count)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_5_top_orders


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

df_6_CA_TX_customers <- df_customers %>% filter(state == "TX" | state == "CA")
df_6_CA_TX_active_customers <- df_6_CA_TX_customers %>% 
  left_join(df_orders, by = "customer_id", multiple = "any") %>%
  mutate(active = case_when(is.na(order_id) ~ "no", TRUE ~ "yes")) %>%
  group_by(active) %>% summarise(count = n())

df_6_CA_TX_inactive_customers_2018 <- df_orders %>%
  filter(strftime(order_date, '%Y') == 2018) %>%
  right_join(df_6_CA_TX_customers, by = "customer_id", multiple = "any") %>%
  mutate(active = case_when(is.na(order_id) ~ "no", TRUE ~ "yes")) %>%
  group_by(active) %>% summarise(count_2018 = n())

df_6_full <- df_6_CA_TX_active_customers %>%
  right_join(df_6_CA_TX_inactive_customers_2018, by = "active") %>%
  replace(is.na(.), 0)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- df_6_full


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

df_7_complete_orders <- df_order_items %>%
  mutate(price = quantity * list_price * (1-discount)) %>%
  left_join(df_orders, by = "order_id") %>%
  group_by(order_id) %>% summarise(order_price = sum(price))

df_7_extreme_clients <- df_7_complete_orders %>%
  mutate(centile = percent_rank(order_price)) %>%
  filter(centile < 0.05 | centile > 0.95) %>%
  left_join(df_orders, by = "order_id") %>% 
  left_join(df_customers, by = "customer_id") %>%
  distinct(customer_id, first_name, last_name)

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_7_extreme_clients


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

v_8_all_dates <- seq(from = as.Date("2016-01-01"), 
                 to = as.Date("2018-12-31"), 
                 by = "day")
df_8_dates <- data.frame(order_date = as.character(v_8_all_dates))

df_8_new_orders <- df_orders %>%
  right_join(df_8_dates, by = "order_date") %>%
  mutate(year = strftime(order_date, '%Y'), month = strftime(order_date, '%m'),
         quarter = case_when(
           month < '04' ~ "Q1",
           month < '07' ~ "Q2",
           month < '10' ~ "Q3",
           TRUE ~ "Q4"
         )) %>%
  group_by(order_date, year, quarter) %>% summarise(order_count = sum(!is.na(order_id)))

df_8_max_min_median <- df_8_new_orders %>%
  group_by(year, quarter) %>%
  summarise(max = max(order_count), min = min(order_count), 
            median = median(order_count))

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_8_max_min_median


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

df_9_time <- df_orders %>% filter(order_status == 4) %>%
  mutate(realisation_time = as.Date(shipped_date) - as.Date(order_date), 
         year = strftime(order_date, '%Y'))

df_9_mean_time <- df_9_time %>% 
  left_join(df_customers, by = "customer_id") %>%
  group_by(year, state) %>% 
  summarise(mean_realisation_time = round(mean(realisation_time), 2))

df_9_wide <- df_9_mean_time %>%
  pivot_wider(names_from = state, values_from = mean_realisation_time)

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_9_wide


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

df_10_annual_customers <- df_orders %>%
  mutate(year = strftime(order_date, '%Y')) %>%
  distinct(year, customer_id) %>%
  group_by(customer_id) %>% summarise(years = n()) %>%
  filter(years == 3)

df_10_annual_letters <- df_10_annual_customers %>%
  left_join(df_customers, by = "customer_id") %>%
  mutate(first_name = substr(last_name, 1, 1)) %>%
  group_by(first_name) %>% summarise(count = n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_10_annual_letters


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

df_11_table <- df_orders %>% left_join(df_order_items, by = "order_id") %>%
  left_join(df_products, by = "product_id") %>%
  right_join(df_customers, by = "customer_id") %>%
  right_join(df_categories, by = "category_id") %>%
  group_by(customer_id, category_name) %>%
  summarise(count = sum(quantity))

df_11_wider <- df_11_table %>%
  pivot_wider(names_from = category_name, values_from = count, values_fill = 0)

df_11_newest <- df_orders %>% left_join(df_order_items, by = "order_id") %>%
  left_join(df_products, by = "product_id") %>%
  mutate(order_year = strftime(order_date, '%Y')) %>%
  filter(order_year == model_year) %>%
  group_by(customer_id) %>% summarise(newest_model = sum(quantity))

df_11_full_table <- df_11_wider %>%
  left_join(df_11_newest, by = "customer_id") %>%
  replace(is.na(.), 0) %>%
  left_join(df_customers, by = "customer_id") %>%
  select(-c(customer_id, phone, email, street, city, state, zip_code)) %>%
  relocate(first_name, last_name, .after = customer_id)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_11_full_table


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

df_12_discounts <- df_order_items %>%
  left_join(df_orders, by = "order_id") %>%
  mutate(new_discount = discount * quantity, 
         weekday = weekdays(as.Date(order_date))) %>%
  left_join(df_products, by = "product_id") %>%
  group_by(product_id, product_name, weekday) %>%
  summarise(mean_discount = round(mean(new_discount), 2))

df_12_wide <- df_12_discounts %>%
  pivot_wider(names_from = weekday, values_from = mean_discount)

# bez podziału na produkty
# df_12_discounts_2 <- df_order_items %>%
#   left_join(df_orders, by = "order_id") %>%
#   mutate(new_discount = discount * quantity, 
#          weekday = weekdays(as.Date(order_date))) %>%
#   group_by(weekday) %>%
#   summarise(mean_discount = round(mean(new_discount), 4))

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_12_wide



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "SadowskaMartyna.rds")
