library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


df_orders <- read.csv('/Users/lila/Desktop/hw01 -- R/dane/orders.csv')
df_order_items <- read.csv('/Users/lila/Desktop/hw01 -- R/dane/order_items.csv')
df_products <- read.csv('/Users/lila/Desktop/hw01 -- R/dane/products.csv')
df_brands <- read.csv('/Users/lila/Desktop/hw01 -- R/dane/brands.csv')
df_categories <-  read.csv('/Users/lila/Desktop/hw01 -- R/dane/categories.csv')
df_customers <- read.csv('/Users/lila/Desktop/hw01 -- R/dane/customers.csv')
df_staffs <- read.csv('/Users/lila/Desktop/hw01 -- R/dane/staffs.csv')
df_stocks <- read.csv('/Users/lila/Desktop/hw01 -- R/dane/stocks.csv')
df_stores <- read.csv('/Users/lila/Desktop/hw01 -- R/dane/stores.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

df_orders %>% 
  mutate(order_date = as.Date(order_date)) %>% 
  mutate(quarter = case_when(month(order_date) <= 3 ~ 1,
                             month(order_date) <= 6 ~ 2,
                             month(order_date) <= 9 ~ 3,
                             TRUE ~ 4),
         year = year(order_date)) %>% 
  select(order_id, customer_id, quarter, year) %>% 
  left_join(df_customers[c('customer_id', 'state')]) %>% 
  right_join(df_order_items[c('order_id', 'product_id')]) %>% 
  group_by(year, quarter, state, product_id) %>% 
  summarise(count = n()) %>% 
  group_by(year, quarter, state) %>% 
  summarise(product_id = product_id[which.max(count)]) %>% 
  left_join(df_products[c('product_id', 'product_name')]) %>% 
  select(year, quarter, state, product_name) -> ANS_TASK_01
  


## Odpowiedz przypisana do zmiennej

ANS_TASK_01

####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

df_orders %>% 
  mutate(year = year(as.Date(order_date)),
         month = month(as.Date(order_date))) %>% 
  group_by(year, month) %>% 
  summarise(total_count = n(), not_shipped_count = sum(order_status != 4)) %>% 
  mutate(percent_not_shipped = not_shipped_count/total_count*100) %>% 
  select(year, month, percent_not_shipped) -> ANS_TASK_02



## Odpowiedz przypisana do zmiennej
ANS_TASK_02


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

df_order_items %>% 
  mutate(product_total = list_price*quantity) %>% 
  left_join(df_orders[c("order_id", "order_date")]) %>% 
  mutate(year = year(as.Date(order_date))) %>% 
  group_by(year, product_id) %>% 
  summarise(product_total_year = sum(product_total)) %>% 
  group_by(year) %>% 
  summarise(mvp_of_year_id = product_id[which.max(product_total_year)]) -> ANS_TASK_03

## Odpowiedz przypisana do zmiennej
ANS_TASK_03


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

df_orders %>% 
  mutate(year = year(as.Date(order_date))) %>% 
  group_by(year, customer_id) %>% 
  summarise(yearly_order_count = n()) %>% 
  group_by(year) %>% 
  mutate(max_order_count = max(yearly_order_count)) %>% 
  filter(yearly_order_count == max_order_count) %>% 
  group_by(year) %>% 
  summarise(biggest_customer_count = n(),
            max_order_count = max(max_order_count)) -> ANS_TASK_04


## Odpowiedz przypisana do zmiennej
ANS_TASK_04


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

df_orders %>% 
  left_join(df_customers[c("customer_id", "email")]) %>% 
  mutate(domain = str_extract(email, "(?<=@).*"), year = year(as.Date(order_date))) %>% 
  group_by(year, domain) %>% 
  summarise(count = n()) %>% 
  group_by(year) %>% 
  summarise(max_count_per_domain = max(count),
            max_domain = domain[which.max(count)]) -> ANS_TASK_05


## Odpowiedz przypisana do zmiennej
ANS_TASK_05


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

#zkładamy, że aktywni klienci to ci, którzy złożyli zamównienie w 2018

df_orders %>% 
  left_join(df_customers[c("customer_id", "state")]) %>% 
  mutate(year = year(as.Date(order_date))) %>% 
  filter(state == "TX" | state == "CA") %>% 
  group_by(state) %>% 
  summarise(total_count = length(unique(customer_id)),
            active_count = length(unique(customer_id[which(year == 2018)]))) %>% 
  mutate(nonactive_count = total_count - active_count) -> ANS_TASK_06

## Odpowiedz przypisana do zmiennej
ANS_TASK_06


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

bottom_5 <- quantile(values, 0.05)
top_95 <- quantile(values, 0.95)

df_order_items %>% 
  mutate(product_total = list_price*quantity) %>%
  group_by(order_id) %>% 
  summarise(order_total = sum(product_total)) %>% 
  left_join(df_orders[c("order_id", "customer_id")]) %>% 
  filter(order_total <= quantile(order_total, 0.05) | order_total >= quantile(order_total, 0.95)) -> ANS_TASK_07
  
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_07


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

df_orders %>% 
  mutate(order_date = as.Date(order_date)) %>% 
  group_by(order_date) %>% 
  summarise(daily_order_count = n()) %>% 
  mutate(quarter = case_when(month(order_date) <= 3 ~ 1,
                             month(order_date) <= 6 ~ 2,
                             month(order_date) <= 9 ~ 3,
                             TRUE ~ 4),
         year = year(order_date)) %>% 
  group_by(year, quarter) %>% 
  summarise(min = min(daily_order_count),
            max = max(daily_order_count),
            median = median(daily_order_count)) -> ANS_TASK_08


## Odpowiedz przypisana do zmiennej
ANS_TASK_08


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

#w tabeli nie ma delivery date, więc zkładamy, że shipped date to data dostarczenia zamówienia...


df_orders %>% 
  filter(order_status == 4) %>% 
  mutate(year = year(order_date)) %>% 
  left_join(df_customers[c("customer_id", "state")]) %>% 
  mutate(delivery_period = difftime(shipped_date, order_date)) %>% 
  group_by(year, state) %>% 
  summarise(mean_delivery_period = mean(delivery_period)) %>% 
  pivot_wider(names_from = state, values_from = mean_delivery_period) -> ANS_TASK_09


## Odpowiedz przypisana do zmiennej
ANS_TASK_09


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

df_orders %>% 
  mutate(year = year(order_date)) %>% 
  group_by(customer_id) %>% 
  summarise(years_active = n_distinct(year)) %>% 
  filter(years_active == 3) %>% 
  left_join(df_customers[c("customer_id", "last_name")]) %>% 
  mutate(first_letter = str_sub(last_name, 1, 1)) %>% 
  group_by(first_letter) %>% 
  summarise(frequency = n()/12) -> ANS_TASK_10

#poprzez usuniecie ostatniej linijki można zauważyć że jest 12 klentów,
#którzy robili zamówienia co roku


## Odpowiedz przypisana do zmiennej
ANS_TASK_10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej

df_order_items %>% 
  select(order_id, product_id, quantity) %>% 
  left_join(df_orders[c("customer_id", "order_id", "order_date")]) %>% 
  left_join(df_products[c("product_id", "category_id", "model_year")]) -> x

x %>% group_by(customer_id) %>% 
  filter(year(order_date) == model_year) %>%   # Filter rows where A equals B
  summarise(count = n()) -> a

x %>% group_by(customer_id, category_id) %>% 
  summarise(category_count = sum(quantity)) %>% 
  pivot_wider(names_from = category_id, values_from = category_count) %>% 
  replace(is.na(.), 0) %>% 
  left_join(a) -> ANS_TASK_11
  

  



ANS_TASK_11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

df_order_items %>% 
  left_join(df_orders[c("order_id", "order_date")]) %>% 
  mutate(day_of_week = wday(order_date)) %>% 
  group_by(product_id, day_of_week) %>% 
  summarise(mean_discount = mean(discount)) -> ANS_TASK_12



## Odpowiedz przypisana do zmiennej
ANS_TASK_12 



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "SirkoLiliana.rds")
