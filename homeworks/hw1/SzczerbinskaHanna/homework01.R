library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

# UWAGA: jesli kilka produktow bylo kupowanych najczesciej (tak samo czesto), to zostawiam je wszystkie

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_order_items %>% 
  select(order_id, product_id, quantity) %>% 
  left_join(df_orders, by = "order_id") %>% 
  select(order_date, customer_id, product_id, quantity) %>% 
  left_join(df_customers, by = "customer_id") %>% 
  select(order_date, state, product_id, quantity) %>% 
  mutate(order_year = strftime(order_date, '%Y'), order_month = strftime(order_date, '%m')) %>% 
  mutate(order_quarter = paste(order_year, 
                               case_when(order_month == "01" | order_month == "02" | order_month == "03" ~ "Q1", 
                                         order_month == "04" | order_month == "05" | order_month == "06" ~ "Q2",
                                         order_month == "07" | order_month == "08" | order_month == "09" ~ "Q3",
                                         order_month == "10" | order_month == "11" | order_month == "12" ~ "Q4"), 
                               sep="_")) %>% 
  select(order_quarter, state, product_id, quantity) %>% 
  group_by(order_quarter, state, product_id) %>% 
  summarise(total_quantity = sum(quantity)) %>% 
  ungroup(product_id) %>% 
  mutate(max_total_quantity = max(total_quantity)) %>% 
  filter(max_total_quantity == total_quantity) %>% 
  ungroup() %>% 
  left_join(df_products, by = "product_id") %>% 
  select(order_quarter, state, product_name, model_year)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% 
  select(order_date, order_status) %>% 
  mutate(order_date = substr(order_date, start = 1, stop = 7)) %>% 
  group_by(order_date, order_status) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = order_status, values_from = count, values_fill = 0) %>% 
  rename(order_month = order_date, status_1 = "1", status_2 = "2", status_3 = "3", status_4 = "4") %>% 
  mutate(orders_sum = rowSums(across(c(status_1, status_2, status_3, status_4))), rejected_percent = status_3/orders_sum*100) %>% 
  select(order_month, rejected_percent)


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

# UWAGA: zakladam ze discount dotyczy kazdego produktu indywidualnie
# tzn jesli ktos zamowil 2 produkty i jest list price i discount
# to traktuje to jako discount do kazdego z tych 2 produktow osobno

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>% 
  left_join(df_orders, by = "order_id") %>% 
  select(order_date, product_id, quantity, list_price, discount) %>% 
  mutate(order_year = strftime(order_date, '%Y'), final_price = quantity*(list_price - discount)) %>% 
  select(order_year, product_id, final_price) %>% 
  group_by(order_year, product_id) %>% 
  summarise(total_price = sum(final_price)) %>% 
  ungroup(product_id) %>% 
  mutate(max_total_price = max(total_price)) %>% 
  filter(max_total_price == total_price) %>% 
  ungroup() %>% 
  left_join(df_products, by = "product_id") %>% 
  select(order_year, product_id, product_name)


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% 
  mutate(order_year = strftime(order_date, '%Y')) %>% 
  group_by(order_year, customer_id) %>% 
  summarise(n_of_orders = n()) %>% 
  ungroup(customer_id) %>% 
  mutate(max_n_of_orders = max(n_of_orders)) %>% 
  filter(max_n_of_orders == n_of_orders) %>% 
  summarise(num_of_max_customers = n(), max_n_of_orders = first(n_of_orders))


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_orders %>% 
  left_join(df_customers, by = "customer_id") %>% 
  mutate(order_year = strftime(order_date, '%Y'), email_domain = sub(".*@", "", email)) %>% 
  select(order_year, email_domain) %>% 
  group_by(order_year, email_domain) %>% 
  summarise(domain_count = n()) %>% 
  ungroup(email_domain) %>% 
  mutate(max_domain_count = max(domain_count)) %>% 
  filter(max_domain_count == domain_count) %>% 
  rename(most_frequent_email_domain = email_domain) %>% 
  select(order_year, most_frequent_email_domain, domain_count)


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej

orders_2018 <- df_orders %>% 
  mutate(order_year = strftime(order_date, '%Y')) %>% 
  filter(order_year == 2018) %>% 
  select(customer_id, order_id) %>% 
  group_by(customer_id) %>% 
  summarise(order_id = first(order_id))
  
ANS_TASK_06 <- df_customers %>% 
  filter(state == "CA" | state == "TX") %>% 
  inner_join(df_orders, by = "customer_id") %>% 
  select(customer_id, state) %>% 
  distinct() %>% 
  left_join(orders_2018, by = "customer_id") %>%
  select(state, customer_id, order_id) %>% 
  group_by(state) %>% 
  mutate(num_of_customers = n()) %>% 
  filter(is.na(order_id)) %>% 
  mutate(didnt_order_in_2018 = n()) %>% 
  select(state, num_of_customers, didnt_order_in_2018) %>% 
  distinct()


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej

order_prices <- df_order_items %>% 
  mutate(total_price = quantity*(list_price-discount)) %>% 
  group_by(order_id) %>% 
  summarise(order_cost = sum(total_price))

quantile_5 <- order_prices %>% 
  summarise(quantile(order_cost, 0.05)) %>% 
  as.numeric()
  
quantile_95 <- order_prices %>% 
  summarise(quantile(order_cost, 0.95)) %>% 
  as.numeric()

ANS_TASK_07 <- order_prices %>% 
  filter(order_cost < quantile_5 | order_cost > quantile_95) %>% 
  inner_join(df_orders, by = "order_id") %>% 
  select(customer_id) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  distinct()


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>% 
  select(order_id, order_date) %>% 
  group_by(order_date) %>% 
  summarise(num_of_orders = n()) %>% 
  mutate(order_year = strftime(order_date, '%Y'), order_month = strftime(order_date, '%m')) %>% 
  mutate(order_quarter = paste(order_year, 
                               case_when(order_month == "01" | order_month == "02" | order_month == "03" ~ "Q1", 
                                         order_month == "04" | order_month == "05" | order_month == "06" ~ "Q2",
                                         order_month == "07" | order_month == "08" | order_month == "09" ~ "Q3",
                                         order_month == "10" | order_month == "11" | order_month == "12" ~ "Q4"), 
                               sep="_")) %>%
  select(order_date, order_quarter, num_of_orders) %>% 
  group_by(order_quarter) %>% 
  summarise(min_orders = min(num_of_orders), max_orders = max(num_of_orders), median_orders = median(num_of_orders))


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>% 
  mutate(order_year = strftime(order_date, '%Y')) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  select(order_year, state, order_date, shipped_date) %>% 
  filter(shipped_date != "NULL") %>% 
  mutate(order_date=as.Date(order_date, format = "%Y-%m-%d")) %>% 
  mutate(shipped_date=as.Date(shipped_date, format = "%Y-%m-%d")) %>% 
  mutate(delivery_time = shipped_date - order_date) %>% 
  group_by(order_year, state) %>% 
  summarise(mean_delivery_time = mean(delivery_time)) %>% 
  pivot_wider(names_from = state, values_from = mean_delivery_time)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej

orders_2016 <- df_orders %>% 
  mutate(order_year = strftime(order_date, '%Y')) %>% 
  filter(order_year == 2016) %>% 
  select(customer_id, order_id)

orders_2017 <- df_orders %>% 
  mutate(order_year = strftime(order_date, '%Y')) %>% 
  filter(order_year == 2017) %>% 
  select(customer_id, order_id)

orders_2018 <- df_orders %>% 
  mutate(order_year = strftime(order_date, '%Y')) %>% 
  filter(order_year == 2018) %>% 
  select(customer_id, order_id)

ANS_TASK_10 <- df_customers %>% 
  inner_join(orders_2016, by = "customer_id") %>% 
  inner_join(orders_2017, by = "customer_id") %>% 
  inner_join(orders_2018, by = "customer_id") %>% 
  mutate(first_letter_of_last_name = substr(last_name, start = 1, stop = 1)) %>% 
  select(first_letter_of_last_name) %>% 
  table()

####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej

newest_bought <- df_order_items %>% 
  inner_join(df_products, by = "product_id") %>% 
  inner_join(df_orders, by = "order_id") %>% 
  mutate(order_year = strftime(order_date, '%Y')) %>% 
  select(customer_id, quantity, order_year, model_year) %>% 
  mutate(is_newest = ifelse(order_year == model_year, 1, 0)) %>% 
  group_by(customer_id) %>% 
  summarise(newest_amount = sum(quantity*is_newest))

ANS_TASK_11 <- df_order_items %>% 
  inner_join(df_products, by = "product_id") %>% 
  inner_join(df_categories, by = "category_id") %>% 
  inner_join(df_orders, by = "order_id") %>% 
  select(customer_id, category_id, quantity) %>% 
  group_by(customer_id, category_id) %>% 
  summarise(bought_from_category = sum(quantity)) %>% 
  pivot_wider(names_from = category_id, values_from = bought_from_category, values_fill = 0) %>% 
  inner_join(newest_bought, by = "customer_id")
  

### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_order_items %>% 
  inner_join(df_orders, by = "order_id") %>% 
  mutate(order_weekday = strftime(order_date, '%A'), promotion_percent = 100*(1-(list_price - discount)/list_price)) %>% 
  select(order_weekday, product_id, quantity, promotion_percent) %>% 
  group_by(order_weekday, product_id) %>% 
  summarise(avg_promotion_percent = weighted.mean(promotion_percent, quantity)) %>% 
  pivot_wider(names_from = order_weekday, values_from = avg_promotion_percent)


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "SzczerbinskaHanna.rds")
