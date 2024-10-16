library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')
df_staffs <- read.csv('homeworks/hw1/dane/staffs.csv')
df_stocks <- read.csv('homeworks/hw1/dane/stocks.csv')
df_stores <- read.csv('homeworks/hw1/dane/stores.csv')


####### Zadanie 1 @
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
library(lubridate)
ANS_TASK_01 <- df_order_items %>% 
  left_join(df_orders, by = 'order_id') %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  mutate(quarter = quarter(order_date, with_year = TRUE)) %>% 
  group_by(quarter, state, product_id) %>% 
  summarise(ile = sum(quantity)) %>% 
  group_by(quarter, state) %>% 
  filter(ile == max(ile)) %>% 
  left_join(df_products[c('product_id', 'product_name', 'model_year')], by = 'product_id') %>% 
  select(quarter, state, product_name, model_year)


####### Zadanie 2 @
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% 
  mutate(month = format(as.Date(order_date), '%Y-%m')) %>%
  group_by(month) %>% 
  summarise(procent = sum(order_status != 4)/n()*100)


####### Zadanie 3 @
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>% 
  left_join(df_orders, by = 'order_id') %>%
  filter(order_status == 4) %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>%
  group_by(year,product_id) %>% 
  summarise(koszt = sum((1-discount)*(list_price*quantity))) %>% 
  group_by(year) %>% 
  filter(koszt == max(koszt))


####### Zadanie 4 @
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>%
  group_by(year, customer_id) %>% 
  summarise(ile = n()) %>% 
  filter(ile == max(ile)) %>% 
  group_by(year) %>% 
  summarise(klienci_top = n(), ile = unique(ile))


####### Zadanie 5 
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
library(stringr)
ANS_TASK_05 <- df_orders %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  mutate(domain = str_extract(email, "@[a-zA-Z0-9.-]+$")) %>%
  mutate(year = format(as.Date(order_date), '%Y')) %>%
  group_by(year, domain) %>% 
  summarise(ile = n()) %>% 
  filter(ile == max(ile))


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej
library(lubridate)
ANS_TASK_06 <- df_order_items %>% 
  left_join(df_orders, by = 'order_id') %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>%
  filter(state == 'TX' | state == 'CA') %>% 
  group_by(state) %>% 
  summarise(ile = n_distinct(customer_id)) %>% 
  group_by(state) %>% 
  mutate(czy_2018 = ile - sum(df_customers$state == state & lubridate::year(df_orders$order_date) == 2018))


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_order_items %>% 
  mutate(cena_fin = (1-discount)*(list_price*quantity)) %>% 
  group_by(order_id) %>% 
  summarise(ile = sum(cena_fin)) %>% 
  arrange(desc(ile)) %>% 
  mutate(place = row_number()) %>% 
  mutate(kwantyl = (n()-place+1)/n()) %>% 
  left_join(df_orders[c('order_id','customer_id')], by = 'order_id') %>% 
  filter(kwantyl < 0.05 | kwantyl > 0.95) %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  group_by(customer_id) %>% 
  summarise(customer_id, first_name, last_name) %>% 
  unique()


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej
library(lubridate)
ANS_TASK_08 <- df_orders %>% 
  mutate(quarter = quarter(order_date, with_year = TRUE)) %>% 
  group_by(order_date, quarter) %>% 
  summarise(ile = n()) %>% 
  group_by(quarter) %>% 
  summarise(max = max(ile), min = min(ile), mediana = median(ile))


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
library(lubridate)
ANS_TASK_09 <- df_orders %>% 
  left_join(df_customers[c('customer_id', 'state')], by = 'customer_id') %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>%
  filter(order_status == 4) %>% 
  mutate(ile = as.Date(shipped_date) - as.Date(order_date)) %>% 
  group_by(state, year) %>% 
  summarise(mean = mean(ile, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "state", values_from = "mean") 


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_orders %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>%
  group_by(year, customer_id) %>% 
  summarise(ile = n()) %>% 
  group_by(customer_id) %>% 
  summarise(czy_co_roku = n()) %>% 
  filter(czy_co_roku == 3) %>% 
  left_join(df_customers[c('customer_id', 'last_name')]) %>% 
  group_by(last_name) %>% 
  mutate(litery = strsplit(as.character(last_name),"")) %>% 
  unnest(litery) %>% 
  mutate(litery = toupper(litery)) %>% 
  count(litery) %>% 
  mutate(first_letter = substr(last_name, 1, 1)) %>% 
  arrange(litery) %>% 
  pivot_wider(names_from = "litery", values_from = "n")


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_orders %>% 
  left_join(df_order_items, by = "order_id") %>%
  left_join(df_products, by = "product_id") %>%
  left_join(df_categories, by = "category_id") %>%
  mutate(order_year = format(as.Date(order_date), "%Y")) %>%
  group_by(customer_id, category_name) %>%
  summarise(category_purchases = n()) %>%
  pivot_wider(names_from = category_name, values_from = category_purchases, values_fill = 0) %>%
  left_join(df_orders %>%
      left_join(df_order_items, by = "order_id") %>%
      left_join(df_products, by = "product_id") %>%
      left_join(df_categories, by = "category_id") %>%
      mutate(order_year = format(as.Date(order_date), "%Y")) %>%
      filter(order_year == model_year) %>%
      group_by(customer_id) %>%
      summarise(nowe = n()),
    by = "customer_id") %>%
  replace_na(list(nowe = 0))
  

### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_order_items %>%
  left_join(df_orders, by = "order_id") %>%
  mutate(weekday = weekdays(as.Date(order_date))) %>%
  mutate(revenue_list_price = list_price * quantity, revenue_actual = (list_price * quantity) * (1 - discount)) %>%
  mutate(discount_percentage = (revenue_list_price - revenue_actual) / revenue_list_price * 100) %>%
  group_by(product_id, weekday) %>%
  summarise(average_discount = mean(discount_percentage))


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "TomaszkiewiczJulia.rds")
