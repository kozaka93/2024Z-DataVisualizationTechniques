library(dplyr)
library(tidyr)

df_orders <- read.csv('folderek/orders.csv')
df_order_items <- read.csv('folderek/order_items.csv')
df_products <- read.csv('folderek/products.csv')
df_brands <- read.csv('folderek/brands.csv')
df_categories <-  read.csv('folderek/categories.csv')
df_staffs <- read.csv('folderek/staffs.csv')
df_stocks <- read.csv('folderek/stocks.csv')
df_stores <- read.csv('folderek/stores.csv')
df_customers <- read.csv('folderek/customers.csv')

head(df_orders)
head(df_order_items)
head(df_products)
head(df_brands)
head(df_categories)
head(df_staffs)
head(df_stocks)
head(df_stores)
head(df_customers)

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

zadanie_1 <- df_order_items %>% 
  inner_join(df_orders, by = "order_id") %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  select(order_id, product_id, quantity, order_date, state) %>% 
  mutate(quarters = quarters(as.Date(order_date)), year = format(as.Date(order_date),"%Y")) %>% 
  select(order_id, product_id, quantity, state, quarters, year) %>% 
  group_by(product_id, state, quarters, year) %>% 
  summarize(final_quantity = sum (quantity), .groups = "drop") %>% 
  group_by(state, quarters, year) %>%
  slice_max(order_by = final_quantity, n = 1, with_ties = FALSE) %>% 
  inner_join(df_products, "product_id") %>% 
  select(state, year, final_quantity, product_name, model_year)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- zadanie_1

####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

zadanie_2 <- df_orders %>% 
  mutate(month = format(as.Date(order_date),"%m"),
         year = format(as.Date(order_date),"%Y")) %>% 
  select(order_id, order_status, month, year) %>% 
  group_by(month, year) %>% 
  summarise(quantity_1 = n(),
            quantity_2 = sum(order_status != 4),
            Procent_niezrealizowanych = quantity_2/quantity_1*100) %>% 
  select(year, month, Procent_niezrealizowanych)

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- zadanie_2

####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

zadanie_3 <- df_order_items %>% 
  inner_join(df_orders, by = "order_id") %>% 
  select(product_id, order_date, quantity, list_price, discount) %>% 
  mutate(year = format(as.Date(order_date),"%Y"), value = list_price*(1-discount)*quantity) %>% 
  select(product_id, year, value) %>% 
  group_by(year, product_id) %>% 
  summarise(final_value = sum(value), .groups = "drop") %>% 
  group_by(year) %>% 
  slice_max(order_by = final_value, n = 1, with_ties = FALSE) %>% 
  inner_join(df_products, "product_id") %>% 
  select(product_name, year)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- zadanie_3


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 


zadanie_4 <- df_orders %>% 
  mutate(year = format(as.Date(order_date),"%Y")) %>%
  select(order_id, customer_id, year) %>% 
  group_by(year, customer_id) %>% 
  summarise(total = n(), .groups = "drop") %>% 
  group_by(year) %>% 
  slice_max(order_by = total, n = 1, with_ties = TRUE) %>% 
  inner_join(df_customers, "customer_id") %>% 
  select(year, first_name, last_name, total) %>% 
  group_by(year) %>% 
  summarise(zamowien_bylo = sum(total), ilosc_klientow = n(), .groups = "drop")
  
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- zadanie_4


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
zadanie_5 <- df_orders %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  mutate(year = format(as.Date(order_date),"%Y"),
         domena = sub(".*@", "", email)) %>%
  select(order_id, domena, year) %>% 
  group_by(year, domena) %>% 
  summarise(total = n(), .groups = "drop") %>% 
  group_by(year) %>% 
  slice_max(order_by = total, n = 1, with_ties = FALSE) %>% 
  select(year, domena, total)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- zadanie_5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

x <- df_orders %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  select(customer_id, state) %>% 
  filter(state %in% c("CA", "TX")) %>%
  select(customer_id, state) %>% 
  summarise(all_customers = n_distinct(customer_id)) %>% 
  pull(all_customers)
y <- df_orders %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  mutate(year = format(as.Date(order_date),"%Y")) %>% 
  filter(state %in% c("CA", "TX") & year == 2018) %>% 
  select(customer_id, state) %>% 
  summarise(disctinct_customers = n_distinct(customer_id)) %>% 
  pull(disctinct_customers)

z <- as.character(x!=y)
zadanie_6 <- c(x,z)
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- zadanie_6


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

zadanie_7 <- df_orders %>% 
  inner_join(df_order_items,by="order_id") %>% 
  group_by(order_id,customer_id) %>% 
  summarize(money_spent=sum(quantity*list_price*(1-discount)), .groups = "drop") %>% 
  mutate(
    lower_quantile = quantile(money_spent, 0.05),
    upper_quantile = quantile(money_spent, 0.95),
    searched_customer = money_spent < lower_quantile | money_spent > upper_quantile
  ) %>%
  select(searched_customer, customer_id) %>% 
  filter(searched_customer==TRUE) %>% 
  arrange(customer_id)


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- zadanie_7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
zadanie_8 <- df_orders %>% 
  mutate(order_date = as.Date(order_date, format = "%Y-%m-%d")) %>%
  group_by(order_date) %>% 
  mutate(orders_number = n()) %>% 
  ungroup() %>% 
  complete(order_date = seq.Date(min(order_date), max(order_date), by = "day"),
           fill = list(orders_number = 0)) %>%
  mutate(
    week_day = as.POSIXlt(order_date)$wday + 1,
    quarter = quarters(order_date),
    year = format(order_date, "%Y")
  ) %>%
  group_by(year, quarter, week_day) %>% 
  summarize(
    orders_number_max = max(orders_number),
    orders_number_min = min(orders_number),
    orders_number_med = median(orders_number),
    .groups = 'drop'
  )

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- zadanie_8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
zadanie_9 <- df_orders %>% 
  inner_join(df_customers, "customer_id") %>%
  mutate(order_date = as.Date(order_date),
         shipped_date = as.Date(shipped_date),
         delivery_time = ifelse(!is.na(shipped_date), as.numeric(shipped_date - order_date), NA)) %>%
  mutate(year = format(order_date, "%Y")) %>%
  group_by(year, state) %>%
  summarize(avg_delivery_time = mean(delivery_time, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = state, values_from = avg_delivery_time)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- zadanie_9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.


zadanie_10 <- df_orders %>%
  mutate(year = format(as.Date(order_date), "%Y")) %>%
  group_by(customer_id) %>%
  summarize(n = n_distinct(year), .groups = 'drop') %>%
  filter(n == 3) %>% 
  inner_join(df_customers, by = "customer_id") %>%
  mutate(first_letter = substr(last_name, 1, 1)) %>% 
  group_by(first_letter) %>%
  summarize(count = n(), .groups = 'drop')



## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- zadanie_10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

newest <- df_order_items %>%
  inner_join(df_orders, by ='order_id') %>% 
  inner_join(df_customers, by ='customer_id') %>% 
  inner_join(df_products, by ='product_id') %>% 
  group_by(customer_id) %>%
  summarize(newest = sum(format(as.Date(order_date), "%Y") == model_year))
zadanie_11 <- df_order_items %>%
  inner_join(df_orders, by ='order_id') %>% 
  inner_join(df_customers, by ='customer_id') %>% 
  inner_join(df_products, by ='product_id') %>% 
  group_by(customer_id, category_id) %>%
  summarize(how_often = n(), .groups = 'drop') %>%
  complete(customer_id, category_id, fill = list(how_often = 0)) %>%
  spread(category_id, how_often) %>%
  inner_join(newest, by = 'customer_id')


## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- zadanie_11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat


zadanie_12 <- df_orders %>%
  inner_join(df_order_items, by = "order_id") %>%
  mutate(
    order_date = as.Date(order_date),
    week_day = as.POSIXlt(order_date)$wday + 1,
    with_discount = quantity * list_price * (1 - discount),
    without_discount = quantity * list_price
  ) %>%
  group_by(week_day, product_id) %>%
  summarize(mean_discount = mean((without_discount - with_discount) / without_discount * 100, na.rm = TRUE),
            .groups = 'drop')




## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- zadanie_12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "CzarneckiRyszard.rds")

