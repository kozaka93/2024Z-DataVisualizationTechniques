library(dplyr)
library(tidyr)
library(stringr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <-  read.csv('homeworks/hw1/dane/customers.csv')
df_staffs <-  read.csv('homeworks/hw1/dane/staffs.csv')
df_stocks <-  read.csv('homeworks/hw1/dane/stocks.csv')
df_stores <-  read.csv('homeworks/hw1/dane/stores.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_order_items %>% 
  inner_join(df_orders, by = "order_id") %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  mutate(order_month =as.integer(format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%m")),
         order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y"),
         quarter = (order_month - 1) %/% 3 + 1) %>% 
  group_by(order_year, quarter, state, product_id) %>% 
  summarise(order_sum = sum(quantity)) %>% 
  inner_join(df_products, by = "product_id") %>% 
  select(order_year, quarter, state, order_sum, product_name, model_year) %>% 
  slice_max(order_by = order_sum, n=1)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% 
  mutate(order_month = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%m"),
         order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y"),
         not_realized = if_else(order_status == 4, 0, 1)) %>% 
  group_by(order_year, order_month) %>% 
  summarise(percentage_not_realized = sum(not_realized) * 100 / n()) %>% 
  select(order_year, order_month, percentage_not_realized = percentage_not_realized)

####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>%  
  inner_join(df_orders, by = "order_id") %>% 
  mutate(product_income = list_price * (1- discount),
         order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y")) %>% 
  group_by(order_year, product_id) %>% 
  summarise(total_product_income = sum(product_income)) %>% 
  slice_max(order_by = total_product_income, n=1) %>% 
  inner_join(df_products, by = "product_id") %>% 
  select(order_year, product_name)

####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% 
  mutate(order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y")) %>% 
  group_by(order_year, customer_id) %>% 
  summarise(number_of_orders_in_a_year = n()) %>% 
  group_by(order_year, number_of_orders_in_a_year) %>% 
  summarise(customers = n()) %>% 
  slice_max(order_by = number_of_orders_in_a_year, n=1)

####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_orders %>% 
  mutate(order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y")) %>% 
  group_by(order_year, customer_id) %>% 
  summarise(number_of_orders_in_a_year = n()) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  separate(email, into = c("adress", "domain"), sep = "@") %>% 
  group_by(order_year, domain) %>% 
  summarise(order_sum = sum(number_of_orders_in_a_year)) %>% 
  slice_max(order_by = order_sum, n=1)



####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

active_customers_TX_and_CA <- df_orders %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  filter(state == "TX" | state == "CA")

customers_TX_and_CA_in_2018 <- active_customers_TX_and_CA %>% 
  mutate(order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y")) %>% 
  filter(order_year == "2018") %>% 
  distinct(customer_id)

number_of_customers_TX_and_CA <- active_customers_TX_and_CA %>% 
  distinct(customer_id) %>% 
  count()

all_customers_TX_and_CA <- df_customers %>% 
  filter(state == "TX" | state == "CA")

inactive_in_2018 <- all_customers_TX_and_CA %>% 
  anti_join(customers_TX_and_CA_in_2018, by = "customer_id")
inactive_in_2018 # nie pusta - istnieją tacy, którzy nie zrobili żadnego zamówienia w 2018 
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(number_of_customers_TX_and_CA, "tak")

####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_order_items %>% 
  mutate(income = list_price * (1- discount)) %>% 
  group_by(order_id) %>% 
  summarise(total_income = sum(income)) %>% 
  mutate(total_income_quantile = ntile(total_income, 20)) %>% 
  filter(total_income_quantile == 1 | total_income_quantile == 20) %>% 
  inner_join(df_orders, by = "order_id") %>% 
  distinct(customer_id) %>% 
  inner_join(df_customers, by = "customer_id")

####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>% 
  mutate(order_month = as.integer(format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%m")),
         order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y"),
         quarter = (order_month - 1) %/% 3 + 1,
         order_day = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%d")) %>% 
  group_by(order_year, quarter, order_month, order_day) %>% 
  summarise(number_of_orders_in_a_day = n()) %>% 
  group_by(order_year, quarter) %>% 
  summarise(maximum_day = max(number_of_orders_in_a_day), minimum_day = min(number_of_orders_in_a_day), quartery_median = median(number_of_orders_in_a_day))


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>% 
  mutate(order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y"),
         order_date = as.Date(order_date, format ="%Y-%m-%d"),
         shipped_date = as.Date(shipped_date, format ="%Y-%m-%d"),
         date_diff_in_days = as.numeric(shipped_date - order_date)) %>% 
  filter(order_status == 4) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  group_by(order_year, state) %>% 
  summarise(mean_of_shipping_time_in_days = mean(date_diff_in_days)) %>% 
  pivot_wider(names_from = state,
              values_from = mean_of_shipping_time_in_days)

####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_orders %>% 
  mutate(order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y")) %>% 
  group_by(customer_id) %>% 
  summarise(orders_in_unique_years = n_distinct(order_year)) %>% 
  filter(orders_in_unique_years == df_orders %>% 
           mutate(order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y")) %>% 
           summarise(number_of_years = n_distinct(order_year)) %>% 
           pull(number_of_years)) %>% 
  select(customer_id) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  mutate(first_letter_of_last_name = str_sub(last_name, 1, 1)) %>% 
  count(first_letter_of_last_name) %>% 
  mutate(frequency_of_occurence_in_percentages = n * 100 / sum(n)) %>% 
  select(first_letter_of_last_name, frequency_of_occurence_in_percentages)
  

####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej
times_customer_ordered_new_bike <- df_customers %>% 
  inner_join(df_orders, by = "customer_id") %>% 
  mutate(order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y")) %>% 
  inner_join(df_order_items, by = "order_id") %>% 
  inner_join(df_products, by = "product_id") %>% 
  filter(order_year == model_year) %>% 
  group_by(customer_id) %>% 
  summarise(times_customer_ordered_new_bike = n())

times_customer_ordered_new_bike

ANS_TASK_11 <- df_customers %>% 
  inner_join(df_orders, by = "customer_id") %>% 
  mutate(order_year = format(as.POSIXct(order_date , format ="%Y-%m-%d"),"%Y")) %>% 
  inner_join(df_order_items, by = "order_id") %>% 
  inner_join(df_products, by = "product_id") %>% 
  inner_join(df_categories, by = "category_id") %>% 
  group_by(customer_id, category_name) %>% 
  summarise(times_bought = n()) %>% 
  pivot_wider(names_from = category_name,
              values_from = times_bought,
              values_fill = 0) %>% 
  inner_join(times_customer_ordered_new_bike, by = "customer_id")
  

### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_orders %>% 
  mutate(order_date = as.Date(order_date, format ="%Y-%m-%d"),
         week_day = weekdays(order_date)) %>% 
  inner_join(df_order_items, by = "order_id") %>% 
  mutate(actual_discount = quantity * list_price * discount) %>% 
  group_by(week_day, product_id) %>% 
  summarise(average_discount = mean(actual_discount, na.rm = TRUE)) %>% 
  inner_join(df_products, by = "product_id") %>% 
  pivot_wider(names_from = week_day,
              values_from = average_discount) %>% 
  select(c(2,7:13))
  


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "PolkowskiCezary.rds")
