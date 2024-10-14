
library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <-  read.csv('homeworks/hw1/dane/customers.csv')
df_stores <-  read.csv('homeworks/hw1/dane/stores.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_orders %>%
  inner_join(df_order_items) %>%
  inner_join(df_customers) %>%
  inner_join(df_products) %>%
  mutate(order_month = as.numeric(format(as.Date(order_date), "%m")),
         order_year = format(as.Date(order_date), "%Y")) %>%
  select(product_id, product_name, state, order_month, order_year) %>%
  mutate(kwartal = ifelse(order_month >= 1 & order_month <= 3, 1, ifelse(order_month >= 4 & order_month <= 6, 2, ifelse(order_month >= 7 & order_month <= 9, 3, 4)))) %>%
  group_by(kwartal, state, product_id) %>%
  summarize(count = n()) %>%
  group_by(kwartal, state) %>%
  slice_max(count, n = 1) %>%
  inner_join(df_products) %>%
  select(kwartal, state, product_id, count, product_name, model_year)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 
## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>%
  mutate(order_month = format(as.Date(order_date), "%m"),
         received = ifelse(shipped_date == 'NULL', 0, 1)) %>%
  group_by(order_month) %>%
  summarize(percent = 100 * sum(received) / n())


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>%
  inner_join(df_orders) %>%
  mutate(order_year = format(as.Date(order_date), "%Y")) %>%
  group_by(order_year, product_id) %>%
  summarize(income = sum(list_price * quantity)) %>%
  group_by(order_year) %>%
  slice_max(income, n = 1)


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>%
  mutate(order_year = format(as.Date(order_date), "%Y")) %>%
  group_by(order_year, customer_id) %>%
  summarize(count = n()) %>%
  group_by(order_year) %>%
  filter(count == max(count)) %>%
  summarize(how_many_customers = n(), max_orders = max(count))


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_orders %>%
  inner_join(df_customers) %>%
  mutate(domain = sub(".*@(.*?)\\..*", "\\1", email),
         order_year = format(as.Date(order_date), "%Y")) %>%
  group_by(order_year, domain) %>%
  summarize(count = n()) %>%
  group_by(order_year) %>%
  slice_max(count, n = 1)


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?
active_customers <- df_customers %>%
  filter(state == 'CA' | state == 'TX') %>%
  summarize(n())

orders_in_2018 <- df_orders %>%
  mutate(order_year = format(as.Date(order_date), "%Y")) %>%
  filter(order_year == 2018) %>%
  distinct(customer_id) %>%
  summarize(n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(active_customers, ifelse(active_customers - orders_in_2018 > 0, TRUE, FALSE))


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_orders %>%
  inner_join(df_order_items) %>%
  mutate(order_value = list_price * quantity * (1 - discount)) %>%
  group_by(order_id, customer_id) %>%
  summarize(total_value = sum(order_value), .groups = "drop") %>%
  mutate(
    q5 = quantile(total_value, 0.05), 
    q95 = quantile(total_value, 0.95)
  ) %>%
  filter(total_value < q5 | total_value > q95) %>%
  select(customer_id) %>%
  distinct()


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
grouped_by_kwartal <- df_orders %>%
  mutate(order_month = as.numeric(format(as.Date(order_date), "%m")),
         kwartal = ifelse(order_month >= 1 & order_month <= 3, 1, ifelse(order_month >= 4 & order_month <= 6, 2, ifelse(order_month >= 7 & order_month <= 9, 3, 4)))) %>%
  group_by(kwartal, order_date) %>%
  summarize(count = n()) %>%
  group_by(kwartal)

           
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- c(grouped_by_kwartal %>% summarize(max(count)), grouped_by_kwartal %>% summarize(min(count)), grouped_by_kwartal %>% summarize(median(count)))


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>%
  inner_join(df_customers) %>%
  mutate(delivery_days = as.integer(as.Date(shipped_date) - as.Date(order_date))) %>%
  mutate(order_year = format(as.Date(order_date), "%Y")) %>%
  group_by(order_year, state) %>%
  summarize(mean_delivery_days = mean(delivery_days, na.rm = TRUE)) %>%
  pivot_wider(names_from = state, values_from = mean_delivery_days)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_orders %>%
  mutate(order_year = format(as.Date(order_date), "%Y"))  %>%
  group_by(customer_id, order_year) %>%
  summarize(n()) %>%
  group_by(customer_id) %>%
  summarize(years_bought = n_distinct(order_year)) %>%
  filter(years_bought == 3) %>%
  inner_join(df_customers) %>%
  mutate(first_letter = substr(last_name, 1, 1)) %>%
  select(first_letter) %>%
  group_by(first_letter) %>%
  summarize(n())


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_orders %>%
  inner_join(df_order_items, by = "order_id") %>%
  inner_join(df_products, by = "product_id") %>%
  mutate(order_year = format(as.Date(order_date), "%Y")) %>%
  mutate(is_latest = ifelse(order_year == model_year, 1, 0)) %>%
  group_by(customer_id, category_id) %>%
  summarize(
    total_bikes = sum(n()),
    latest_bikes = sum(is_latest), 
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = category_id, 
    values_from = total_bikes, 
    values_fill = 0, 
    names_prefix = "category_"
  ) %>%
  group_by(customer_id) %>%  # Sumujemy dla każdego klienta
  summarize(across(starts_with("category_"), sum), 
            latest_bikes = sum(latest_bikes), 
            .groups = "drop") %>%
  replace(is.na(.), 0)


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_orders %>%
  inner_join(df_order_items, by = "order_id") %>%
  mutate(
    actual_price = list_price * quantity * (1 - discount),
    discount_percent = (list_price * quantity - actual_price) / (list_price * quantity) * 100,
    day_of_week = weekdays(as.Date(order_date))
  ) %>%
  group_by(product_id, day_of_week) %>%
  summarize(
    avg_discount = mean(discount_percent, na.rm = TRUE),
    .groups = "drop"
  )



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "GrabowskiGabriel.rds")

