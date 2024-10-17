library(dplyr)
library(tidyr)
library(stringr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <-  read.csv('homeworks/hw1/dane/customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany 
# z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_order_items %>%
  left_join(df_orders) %>%
  left_join(df_customers) %>%
  mutate(month = as.integer(substr(order_date, 6,7)),
         quarter = case_when(
           month < 4 ~ 1,
           month < 7 ~ 2,
           month < 10 ~ 3,
           TRUE ~ 4
         )) %>%
  transmute(product_id, quantity, year = substr(order_date, 1, 4), quarter, state) %>%
  group_by(year, quarter, state, product_id) %>%
  summarise(purchases = sum(quantity)) %>%
  filter(purchases == max(purchases)) %>%
  left_join(df_products) %>%
  select(product_name, model_year)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <-  df_orders %>%
  filter(shipped_date == "NULL") %>%
  mutate(year = substr(order_date, 1, 4), month = substr(order_date, 6, 7)) %>%
  group_by(year, month) %>%
  summarise(unrealised = n()) %>%
  right_join(
    df_orders %>%
      mutate(year = substr(order_date, 1, 4), month = substr(order_date, 6, 7)) %>%
      group_by(year, month) %>%
      summarise(all = n())
  ) %>%
  transmute(year, month, percentage = 100*replace_na(unrealised, 0)/all)



####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>%
  mutate(true_price = list_price * (1 - discount) * quantity) %>%
  left_join(df_orders) %>%
  group_by(year = substr(order_date, 1, 4), product_id) %>%
  summarise(total_income = sum(true_price)) %>%
  filter(total_income == max(total_income)) %>%
  left_join(df_products) %>%
  select(product_name)


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) 
# w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>%
  group_by(year = substr(order_date, 1, 4), customer_id) %>%
  summarise(n = n()) %>%
  filter(n == max(n)) %>%
  group_by(year, order_count = n) %>%
  summarise(customer_count = n())





####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku?
# Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_orders %>%
  left_join(df_customers) %>%
  group_by(year = substr(order_date, 1, 4),
           domain = str_extract(email, "(?<=@).+"))  %>%
  summarise(order_count = n()) %>%
  filter(order_count == max(order_count))


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <-  df_orders %>% 
  filter(substr(order_date, 1, 4) == "2018") %>%
  right_join(df_customers) %>%
  filter(is.na(order_id), state %in% c("CA", "TX")) %>%
  group_by(state) %>%
  summarise(purchaseless_in_2018 = n()) %>%
  inner_join( df_customers %>%
                filter(state %in% c("CA", "TX")) %>%
                group_by(state) %>%
                summarise(total_customers = n())
  )


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu
# poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_orders %>%
  inner_join(df_order_items) %>%
  mutate(price = list_price*(1-discount)) %>%
  group_by(order_id) %>%
  summarise(order_price = sum(price)) %>%
  filter(order_price < quantile(order_price, 0.05) | order_price > quantile(order_price, 0.95)) %>%
  left_join(df_orders) %>%
  distinct(customer_id) %>%
  left_join(df_customers) %>%
  select(first_name, last_name)


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych
# każdego dnia w poszczególnych kwartalach.


## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>%
  group_by(order_date) %>%
  summarise(n_of_orders = n()) %>%
  right_join(tibble(order_date = format(0:1095 + as.Date("2016-01-01")))) %>%
  mutate(n_of_orders = replace_na(n_of_orders, 0),
         year = as.integer(substr(order_date, 1, 4)),
         month = as.integer(substr(order_date, 6, 7)),
         quarter = case_when(
           month < 4 ~ 1,
           month < 7 ~ 2,
           month < 10 ~ 3,
           TRUE ~ 4
         )
  ) %>%
  group_by(year, quarter) %>%
  summarise(
    max_n_of_orders = max(n_of_orders),
    min_n_of_orders = min(n_of_orders),
    med_n_of_orders = median(n_of_orders),
  )


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  
# stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała 
# informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>%
  mutate(
    year = substr(order_date, 1, 4),
    shipping_time = as.Date(shipped_date) - as.Date(order_date)
  ) %>%
  left_join(df_customers) %>%
  group_by(year, state) %>%
  summarise(avg_shipping_time = mean(shipping_time, na.rm = TRUE)) %>%
  pivot_wider(names_from = state, values_from = avg_shipping_time)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_orders %>%
  mutate(year = substr(order_date, 1, 4)) %>%
  group_by(customer_id, year) %>%
  summarise() %>%
  group_by(customer_id) %>%
  summarise(yearly_customer = n() == 3) %>%
  filter(yearly_customer == TRUE) %>%
  left_join(df_customers) %>%
  transmute(first_letter = substr(last_name, 1, 1)) %>%
  group_by(first_letter) %>%
  summarise(apperences = n())


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower 
# każdej kategorii. Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt 
# (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_orders %>%
  left_join(df_order_items) %>%
  left_join(df_products) %>%
  group_by(customer_id, category_id) %>%
  summarise(n = sum(quantity)) %>%
  left_join(df_categories) %>%
  left_join(df_customers) %>%
  ungroup() %>%
  transmute(full_name = paste(first_name, last_name),
            category = category_name,
            n = n) %>%
  group_by(full_name) %>%
  pivot_wider(names_from = category, values_from = n, values_fill = 0) %>%
  left_join(
    df_orders %>%
      left_join(df_order_items) %>%
      left_join(df_products) %>%
      mutate(order_year = as.integer(substr(order_date, 1, 4))) %>%
      filter(model_year == order_year) %>%
      group_by(customer_id) %>%
      summarise(purchased_new = n()) %>%
      left_join(df_customers) %>%
      transmute(full_name = paste(first_name, last_name), purchased_new)
  )


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym 
# z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_orders %>%
  inner_join(df_order_items) %>%
  mutate(weekday = weekdays(as.Date(order_date))) %>%
  group_by(weekday, product_id) %>%
  summarise(mean_discount = weighted.mean(discount, quantity, na.rm = TRUE)) %>%
  pivot_wider(names_from = weekday, values_from = mean_discount)%>%
  left_join(select(df_products, product_id, product_name)) %>%
  select(product_name, weekdays(as.Date(4:10)))



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "BoteroLeonikSebastian.rds")
