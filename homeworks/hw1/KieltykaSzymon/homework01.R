library(dplyr)
library(tidyr)


df_orders <- read.csv('data_hw1/orders.csv')
df_order_items <- read.csv('data_hw1/order_items.csv')
df_products <- read.csv('data_hw1/products.csv')
df_brands <- read.csv('data_hw1/brands.csv')
df_categories <-  read.csv('data_hw1/categories.csv')
df_customers <- read.csv('data_hw1/customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_order_items %>%
  left_join(df_orders) %>%
  left_join(df_products) %>% 
  left_join(df_customers) %>% 
  mutate(order_date = as.Date(order_date),
         order_month = format(order_date, "%m"),
         order_quarter = case_when(order_month %in% c("01", "02", "03") ~ "Q1",
                                   order_month %in% c("04", "05", "06") ~ "Q2",
                                   order_month %in% c("07", "08", "09") ~ "Q3",
                                   order_month %in% c("10", "11", "12") ~ "Q4")) %>%
  group_by(product_id, order_quarter, state) %>%
  summarise(product_name, model_year, times_purchased = n()) %>% 
  distinct() %>% 
  group_by(order_quarter, state) %>%
  slice(which.max(times_purchased)) %>% 
  select(order_quarter, state, product_name, model_year)

####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% 
  mutate(is_uncompleted = ifelse(order_status == 4, 0, 1),
         order_date = as.Date(order_date),
         order_month = format(order_date, "%m")) %>%
  group_by(order_month) %>%
  summarise(orders_count = n(), uncompleted_orders_count = sum(is_uncompleted)) %>% 
  mutate(uncompleted_orders_percent = uncompleted_orders_count / orders_count * 100) %>% 
  select(order_month, uncompleted_orders_percent)


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>% 
  inner_join(df_orders) %>%
  inner_join(df_products) %>%
  mutate(revenue = round(quantity * list_price * (1-discount), 2),
         order_date = as.Date(order_date),
         order_year = format(order_date, "%Y")) %>% 
  group_by(product_id, order_year) %>%
  summarise(product_name, revenue = sum(revenue)) %>% 
  distinct() %>%
  group_by(order_year) %>%
  slice(which.max(revenue)) %>% 
  select(order_year, product_name)


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_customers %>% 
  inner_join(df_orders) %>% 
  mutate(order_date = as.Date(order_date),
         order_year = format(order_date, "%Y")) %>%
  group_by(customer_id, order_year) %>%
  summarise(orders_count = n()) %>% 
  group_by(order_year) %>%
  filter(orders_count == max(orders_count)) %>%
  group_by(order_year) %>% 
  summarise(customer_max_order_count = n(), orders_count = max(orders_count))


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_orders %>% 
  inner_join(df_customers) %>%
  mutate(email_domain = sapply(strsplit(email, "@"), `[`, 2),
         order_date = as.Date(order_date),
         order_year = format(order_date, "%Y")) %>% 
  group_by(order_year, email_domain) %>%
  summarise(orders_count = n()) %>%
  group_by(order_year) %>%
  slice(which.max(orders_count))


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- list(
  df_customers %>% 
  inner_join(df_orders) %>%
  group_by(customer_id) %>%
  summarise(state, orders_count = n()) %>%
  distinct() %>%
  filter(orders_count > 0,
         state %in% c("CA", "TX")) %>%
  group_by(state) %>%
  summarise(active_customers = n()),
  df_orders %>%
  mutate(order_date = as.Date(order_date),
         order_year = format(order_date, "%Y")) %>%
  filter(order_year == "2018") %>%
  group_by(customer_id) %>%
  summarise(order_count = n()) %>%
  right_join(df_customers) %>%
  filter(is.na(order_count))
  )
# W pierwszym elemencie listy znajduje się informacja o liczbie aktywnych klientów w Kalifornii i Teksasie
# W drugim elemencie znajduje się informacja o klientach, którzy nie zrobili żadnego zamówienia w 2018 roku.

####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_order_items %>% 
  mutate(price = quantity * round(list_price * (1-discount), 2)) %>% 
  inner_join(df_orders) %>% 
  group_by(order_id) %>% 
  summarise(total_price = sum(price)) %>%
  filter(total_price < quantile(total_price, 0.05) | total_price > quantile(total_price, 0.95)) 

####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>%
  mutate(order_date = as.Date(order_date),
         order_month = format(order_date, "%m"),
         order_quarter = case_when(order_month %in% c("01", "02", "03") ~ "Q1",
                                   order_month %in% c("04", "05", "06") ~ "Q2",
                                   order_month %in% c("07", "08", "09") ~ "Q3",
                                   order_month %in% c("10", "11", "12") ~ "Q4")) %>%
  group_by(order_date, order_quarter) %>%
  summarise(orders_count = n()) %>% 
  group_by(order_quarter) %>%
  summarise(min_orders = min(orders_count),
            max_orders = max(orders_count),
            median_orders = median(orders_count))


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>%
  left_join(df_customers) %>%
  mutate(order_date = as.Date(order_date),
         shipped_date = as.Date(shipped_date),
         order_year = format(order_date, "%Y"),
         ship_time = as.numeric(difftime(shipped_date, order_date, units = "days"))) %>%
  group_by(order_year, state) %>%
  summarise(mean_ship_time = mean(ship_time, na.rm = TRUE)) %>% 
  pivot_wider(names_from = state, values_from = mean_ship_time)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej
df_task_10 <- df_orders %>% 
  mutate(order_date = as.Date(order_date),
         order_year = format(order_date, "%Y")) %>%
  group_by(customer_id, order_year) %>%
  summarise(order_count = n()) %>%
  mutate(did_order = ifelse(order_count > 0, 1, 0)) %>%
  group_by(customer_id) %>%
  summarise(years_ordered = sum(did_order)) %>% 
  filter(years_ordered == max(years_ordered)) %>%
  left_join(df_customers) %>% 
  mutate(last_name_first_letter = substr(last_name, 1, 1),
         last_name = tolower(last_name))
ANS_TASK_10 <- list(unique(df_task_10$last_name_first_letter),
                    table(unlist(sapply(df_task_10$last_name, strsplit, ""))))
# W pierwszym elemencie listy znajdują się litery, od których zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# W drugim elemencie listy znajduje się informacja o liczbie wystąpień każdej litery wśród tych nazwisk.

####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_order_items %>% 
  inner_join(df_orders) %>%
  inner_join(df_products) %>%
  inner_join(df_categories) %>%
  group_by(customer_id, category_name) %>%
  summarise(times_purchased = n()) %>%
  pivot_wider(names_from = category_name, values_from = times_purchased, values_fill = 0) %>% 
  inner_join(
    df_order_items %>% 
      inner_join(df_orders) %>%
      inner_join(df_customers) %>%
      inner_join(df_products) %>%
      mutate(order_date = as.Date(order_date),
             model_year = as.numeric(model_year),
             order_year = as.numeric(format(order_date, "%Y"))) %>%
      group_by(customer_id) %>%
      summarise(times_purchased_newest = sum(model_year == order_year)) %>% 
      select(customer_id, times_purchased_newest)
  )


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_orders %>% 
  inner_join(df_order_items) %>%
  mutate(order_date = as.Date(order_date),
         week_day = format(order_date, "%u")) %>% 
  group_by(product_id, week_day) %>%
  summarise(mean_discount = mean(discount, na.rm = TRUE) * 100) %>% 
  pivot_wider(names_from = week_day, values_from = mean_discount)



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "KieltykaSzymon.rds")

