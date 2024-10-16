library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <-  read.csv('homeworks/hw1/dane/customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
library(lubridate)
ANS_TASK_01 <- df_orders %>% 
  mutate(quarter = quarter(order_date, with_year = TRUE)) %>% 
  left_join(df_order_items, by = 'order_id') %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  select(order_id, customer_id, state, order_date, quarter, product_id, quantity) %>% 
  group_by(state, quarter, product_id) %>%    
  summarise(total_quantity = sum(quantity), .groups = 'drop') %>%  
  group_by(quarter) %>%                             
  slice_max(total_quantity, n = 1, with_ties = FALSE) %>% 
  left_join(df_products, by = 'product_id') %>% 
  select(quarter, product_name, model_year)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% 
  mutate(month = format(as.Date(order_date), '%Y-%m')) %>% 
  select(month, order_status) %>% 
  group_by(month) %>% 
  mutate(rejected = ifelse(order_status != 4, 1, 0)) %>% 
  summarise(orders_sum = n(), rejected = sum(rejected), .groups = 'drop') %>% 
  mutate(percent = round(100 * rejected / orders_sum, 2))


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_orders %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>% 
  left_join(df_order_items, by = 'order_id') %>% 
  select(year, order_status, product_id, quantity, list_price, discount) %>% 
  filter(order_status == 4) %>% 
  mutate(earned = quantity * list_price * (1-discount)) %>% 
  select(year, product_id, earned) %>% 
  group_by(year, product_id) %>% 
  summarise(total_earned = sum(earned)) %>% 
  filter(total_earned == max(total_earned))


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_order_items %>% 
  left_join(df_orders, by = 'order_id') %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>% 
  select(year, customer_id, product_id) %>% #Nie bierzemy pod uwagę ilości przedmiotów w jednym zamówieniu (liczymy to jako jedne zakupy)
  group_by(year, customer_id) %>% 
  summarise(noumber_of_purchases = n(), .groups = 'drop') %>% 
  group_by(year) %>% 
  filter(noumber_of_purchases == max(noumber_of_purchases)) %>% 
  group_by(year, noumber_of_purchases) %>% 
  summarise(noumber_of_clients = n())


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
library(stringr)
ANS_TASK_05 <- df_customers %>% 
  left_join(df_orders, by = 'customer_id') %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>%
  select(year, email) %>% 
  mutate(domain = str_extract(email, "(?<=@).*")) %>% 
  select(year, domain) %>% 
  group_by(year, domain) %>% 
  summarise(total_uses = n()) %>% 
  filter(total_uses == max(total_uses)) %>% 
  mutate(noumber_of_orders = total_uses) %>% 
  select(year, domain, noumber_of_orders)

####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- df_orders %>%   #Wynik do pierwszego pytania
  mutate(year = format(as.Date(order_date), '%Y')) %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  select(year, customer_id, state) %>% 
  filter(state != 'NY') %>% 
  distinct(customer_id, .keep_all = TRUE) %>%
  group_by(state) %>% 
  summarise(customer_id = n()) %>% 
  mutate(noumber_of_users = customer_id) %>% 
  select(state, noumber_of_users)

df6 <- df_orders %>%
  mutate(year = format(as.Date(order_date), '%Y')) %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  select(year, customer_id, state) %>% 
  filter(state != 'NY') %>% 
  group_by(year, customer_id) %>% 
  summarise(sum_of_orders = n()) %>% 
  mutate(points = ifelse(year == 2018, 5, 1)) %>% 
  group_by(customer_id) %>% 
  summarise(sum_points = sum(points)) %>% 
  mutate(absent_in_2018 = ifelse(sum_points <= 2, 1, 0)) 
#Z ramki danych zamieszczonej powyżej (df6) widać że istnieją takie osoby i jest ich 316

  
####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
df7 <- df_orders %>% 
  left_join(df_order_items, by = 'order_id') %>% 
  select(customer_id, order_date, product_id, quantity, list_price, discount) %>% 
  mutate(order_value = quantity * list_price * (1 - discount))

quantiles <- quantile(df7$order_value, probs = c(0.05, 0.95))

ANS_TASK_07 <- df7 %>% 
  filter(order_value < quantiles[1] | order_value > quantiles[2]) %>%
  select(customer_id) %>%
  distinct()


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>% 
  mutate(quarter = quarter(order_date, with_year = TRUE)) %>% 
  select(order_date, quarter) %>% 
  group_by(order_date, quarter) %>% 
  summarise(daily_orders = n(), .groups = 'drop') %>%
  group_by(quarter) %>% 
  summarise(max_daily_orders = max(daily_orders),
            min_daily_orders = min(daily_orders),
            median = median(daily_orders))


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  select(order_status, order_date, required_date, state) %>% 
  filter(order_status == 4) %>% 
  mutate(year = format(as.Date(order_date), '%Y'),
         time = as.Date(required_date) - as.Date(order_date)) %>% 
  select(year, state, time) %>% 
  group_by(year, state) %>% 
  summarise(mean_time = round(mean(time), 2)) %>% 
  pivot_wider(names_from = state, values_from = mean_time)
  
  


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej
library(stringr)
ANS_TASK_10 <- df_orders %>%    #Wynikowa ramka danych dla pierwszej częśći zadania
  left_join(df_customers, by ='customer_id') %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>% 
  select(year, first_name, last_name) %>% 
  unique() %>% 
  mutate(full_name = paste(first_name, last_name)) %>% 
  select(year, full_name) %>% 
  group_by(full_name) %>% 
  summarise(years_ordered = n()) %>% 
  mutate(
    lastname = word(full_name, 2),
    initials = substr(lastname, 1, 1)
  )

library(tidyr)
df10 <- ANS_TASK_10 %>%
  mutate(letters = str_split(lastname, "")) %>%
  unnest(letters) %>%                             
  count(letters)
  


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej
library(lubridate)
ANS_TASK_11 <- df_categories %>%   #Rozwiązanie pierwszej części zadania
  left_join(df_products, by = 'category_id') %>% 
  left_join(df_order_items, by = 'product_id') %>% 
  left_join(df_orders, by = 'order_id') %>% 
  select(category_id, category_name, model_year, quantity, customer_id, order_date) %>% 
  group_by(customer_id, category_name) %>%
  summarise(total_quantity = sum(quantity), .groups = 'drop') %>% 
  pivot_wider(
    names_from = category_name,
    values_from = total_quantity,
    values_fill = 0
  )

df11 <- df_categories %>%    #Rozwiązanie drugiej części zadania
  left_join(df_products, by = 'category_id') %>% 
  left_join(df_order_items, by = 'product_id') %>% 
  left_join(df_orders, by = 'order_id') %>% 
  select(category_id, category_name, model_year, quantity, customer_id, order_date) %>% 
  mutate(is_newest = if_else(model_year == year(order_date), 1, 0)) %>% 
  group_by(customer_id, category_name) %>%
  summarise(
    total_quantity = sum(quantity),
    newest_purchases = sum(quantity * is_newest),
    .groups = 'drop'
  )


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_orders %>% 
  left_join(df_order_items, by = 'order_id') %>% 
  select(order_date, list_price, discount) %>% 
  mutate(actual_revenue = list_price * (1 - discount),
    percentage_discount = (list_price - actual_revenue) / list_price * 100,
    day_of_week = wday(order_date, label = TRUE)) %>% 
  group_by(day_of_week) %>%
  summarise(average_discount = mean(percentage_discount, na.rm = TRUE))



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "WinklerMaciej.rds")