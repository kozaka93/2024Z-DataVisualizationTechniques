library(dplyr)
library(tidyr)
library(lubridate)


df_orders <- read.csv('dane/orders.csv')
df_order_items <- read.csv('dane/order_items.csv')
df_products <- read.csv('dane/products.csv')
df_brands <- read.csv('dane/brands.csv')
df_categories <-  read.csv('dane/categories.csv')
df_customers <- read.csv('dane/customers.csv')
View(df_orders)
View(df_order_items)
View(df_products)
View(df_brands)
View(df_categories)
View(df_customers)
####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.
zd1 <- df_orders %>% inner_join(df_order_items, by = "order_id") %>% 
  select(customer_id, order_date, product_id, order_id) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  select(state, order_date, product_id, order_id) %>% 
  inner_join(df_products, by = "product_id") %>% 
  select(state, order_date, product_name, model_year, order_id) %>% 
  distinct() %>% 
  mutate(year_quarter = paste(year(order_date), "Q", 
                              quarter(order_date), sep = "")) %>% 
  group_by(state, year_quarter, product_name) %>% 
  summarise(count = n(), model_year = first(model_year), .groups = "drop") %>% 
  group_by(state, year_quarter) %>% 
  filter(count == max(count)) %>% 
  ungroup() %>% select(-count)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- zd1


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 
zd2 <- df_orders %>% 
  mutate(year_month = format(as.Date(order_date), "%Y-%m")) %>% 
  select(year_month, order_status) %>% 
  group_by(year_month) %>% 
  summarise(failed_fraction = (sum(order_status == 3)/n())) %>% 
  mutate(failed_fraction = round(failed_fraction*100, 1)) %>% 
  mutate(failed_fraction = paste(failed_fraction, "%", sep = ""))

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- zd2


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
zd3 <- df_orders %>% 
  inner_join(df_order_items, by = "order_id") %>% 
  select(order_date, quantity, discount, product_id) %>% 
  inner_join(df_products, by = "product_id") %>% 
  select(order_date, quantity, list_price, discount, product_name, product_id) %>% 
  mutate(year = year(order_date), revenue = list_price*(1-discount)*quantity) %>%
  group_by(product_id, year) %>% 
  summarise(product_name = first(product_name), revenue = sum(revenue)) %>% 
  group_by(year) %>% slice_max(order_by = revenue, n = 1) %>% 
  ungroup() %>% select(-product_id)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- zd3


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 
zd4 <- df_orders %>% mutate(year = year(order_date)) %>% 
  select(year, customer_id) %>% group_by(year, customer_id) %>% 
  mutate(number_of_orders = n()) %>% ungroup() %>% group_by(year) %>% 
  filter(number_of_orders == max(number_of_orders)) %>% 
  distinct(customer_id, .keep_all = TRUE) %>% 
  summarise(year = first(year), number_of_clients = n(), number_of_orders = first(number_of_orders))

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- zd4


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
zd5 <- df_orders %>% inner_join(df_customers, by = "customer_id") %>% 
  mutate(year = year(order_date)) %>% select(year, email) %>% 
  mutate(domain = lapply(strsplit(email, "@"), function(x) x[2])) %>% 
  group_by(year, domain) %>% 
  summarize(number_of_orders = n()) %>% 
  group_by(year) %>% filter(number_of_orders == max(number_of_orders)) %>% 
  ungroup()
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- zd5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?
orders2018 <- df_orders %>%
  mutate(year = year(order_date)) %>% filter(year == 2018) %>% 
  distinct(customer_id)

active_customers <- df_orders %>% distinct(customer_id)

zd6 <- df_customers %>% filter(state %in% c("CA", "TX")) %>% 
  filter(customer_id %in% active_customers$customer_id) %>% 
  mutate(ordered_in_2018 = if_else((customer_id %in% orders2018$customer_id), "YES", "NO")) %>% 
  select(customer_id, state, ordered_in_2018) %>% 
  group_by(state) %>% 
  summarise(state = first(state), active_clients = n(), not_ordered_2018 = sum(ordered_in_2018 == "NO"))

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- zd6


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
temp <- df_orders %>% inner_join(df_order_items, by = "order_id") %>% 
  mutate(expenses = quantity*list_price*(1-discount)) %>% group_by(order_id) %>%
  summarise(expenses = sum(expenses), customer_id = first(customer_id))

quantiles <- quantile(temp$expenses, probs = c(0.05, 0.95))

zd7 <- temp %>% filter(expenses <= quantiles[1] | expenses >= quantiles[2]) %>% 
  distinct(customer_id)

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- zd7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
date_seq <- data.frame(order_date = seq.Date(min(as.Date(df_orders$order_date)), 
                                             max(as.Date(df_orders$order_date)), 
                                             by = "day"))

temp <- df_orders %>% 
  select(order_date) %>% group_by(order_date) %>% 
  summarise(number_of_orders = n()) %>% mutate(order_date = as.Date(order_date))

zd8 <- date_seq %>% left_join(temp, by = "order_date") %>% 
  replace_na(list(number_of_orders = 0)) %>% 
  mutate(quarter = paste(year(order_date), 
         quarter(order_date), sep = "Q"), 
         day = paste(month(order_date), 
         day(order_date), sep = "-")) %>%
  group_by(quarter) %>% 
  summarise(max = max(number_of_orders), 
          min = min(number_of_orders), 
          median = median(number_of_orders))

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- zd8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
zd9 <- df_orders %>% filter(shipped_date != "NULL") %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  mutate(year = year(order_date),
         shipping_time = as.numeric(as.Date(shipped_date) - as.Date(order_date))) %>% 
  select(year, state, shipping_time) %>% group_by(year, state) %>% 
  summarize(mean_time = mean(shipping_time)) %>% 
  pivot_wider(names_from = state, values_from = mean_time)

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- zd9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
temp <- df_orders %>% mutate(year = year(order_date)) %>% select(customer_id, year) %>% 
  distinct() %>% mutate(ordered_every_year = TRUE) %>% 
  pivot_wider(names_from = year, values_from = ordered_every_year, values_fill = FALSE) %>% 
  filter(if_all(starts_with("201"))) %>% 
  left_join(df_customers, by = "customer_id") %>% select(last_name)
 
zd10 <-temp %>% summarise(all_letters = paste0(tolower(last_name), collapse = "")) %>% 
  mutate(all_letters = strsplit(all_letters, "")) %>% 
  unnest(all_letters) %>% 
  filter(all_letters %in% tolower(substr(temp$last_name, 1, 1))) %>% 
  count(all_letters, sort = TRUE) %>% 
  rename(first_letter = all_letters, count = n)

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- zd10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)
zd11 <- df_orders %>% mutate(order_year = year(order_date)) %>% 
  select(customer_id, order_id, order_year) %>% 
  inner_join(df_order_items, by = "order_id") %>%
  select(customer_id, product_id, order_year, quantity) %>% 
  inner_join(df_products, by = "product_id") %>%
  select(customer_id, category_id, model_year, order_year, quantity) %>% 
  inner_join(df_categories, by = "category_id") %>% 
  select(customer_id, category_id, category_name, model_year, order_year, quantity) %>% 
  group_by(customer_id, category_id) %>% 
  summarise(name = first(category_name), count = sum(quantity), 
            brand_new_count = sum(as.numeric(model_year) == as.numeric(order_year))) %>%
  select(-category_id) %>% 
  pivot_wider(names_from = name, values_from = count, values_fill = 0) %>% 
  group_by(customer_id) %>% summarise(across(everything(), sum))

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- zd11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
zd12 <- df_orders %>% mutate(day = wday(order_date, label = TRUE)) %>% 
  inner_join(df_order_items, by = "order_id") %>% 
  select(day, product_id, discount) %>% group_by(product_id, day) %>% 
  summarise(mean_discount = mean(discount*100)) %>% 
  pivot_wider(names_from = day, values_from = mean_discount, values_fill = NA)

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- zd12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "VinichenkoOleksii.rds")
