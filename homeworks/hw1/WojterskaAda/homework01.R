library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_stores <-  read.csv('homeworks/hw1/dane/stores.csv')
df_customers <-  read.csv('homeworks/hw1/dane/customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej

TASK_01 <- function(df_order_items, df_orders, df_products, df_customers){
  
  base <- df_order_items %>%
    inner_join(df_orders, by = "order_id") %>%
    inner_join(df_products, by = "product_id") %>%
    inner_join(df_customers, by = "customer_id") %>%
    select(product_id, quantity, order_date, state, model_year)
  
  base <- base %>%
    mutate(order_date = as.Date(order_date),  
           year = format(order_date, "%Y"),
           quarter = as.integer((as.numeric(format(order_date, "%m")) - 1) / 3) + 1) %>%
  group_by(year, quarter, state, product_id) %>%
  summarise(total_quantity = sum(quantity), .groups = 'drop') %>%
  arrange(year, quarter, state, desc(total_quantity)) %>%
  group_by(year, quarter, state) %>%
  slice_max(total_quantity) %>%
  inner_join(df_products, by = "product_id") %>% 
  select(product_name, model_year, total_quantity)

  return(base)
}

ANS_TASK_01 <- TASK_01(df_order_items, df_orders, df_products, df_customers)

####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej


TASK_02 <- function(df_orders){
  
  base <- df_orders %>%
    mutate(order_date = as.Date(order_date),  
           year = format(order_date, "%Y"),
           month = format(order_date, "%m")) %>%
    group_by(year, month) %>%
    summarise(total_orders = n())
  
  base2 <- df_orders %>%
    mutate(order_date = as.Date(order_date),  
           year = format(order_date, "%Y"),
           month = format(order_date, "%m")) %>%
    filter(order_status == 4) %>%
    group_by(year, month) %>%
    summarise(total_orders_completed = n(), .groups = 'drop') %>%
    complete(year, month, fill = list(total_orders_completed = 0))

  result <- left_join(base, base2, by = c("year", "month"))%>%
    mutate(orders_uncomplited_per = 100 - (total_orders_completed / total_orders * 100))%>%
    select(year, month, orders_uncomplited_per)
  
  return(result)
}

ANS_TASK_02 <- TASK_02(df_orders)

####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej

TASK_03 <- function(df_order_items, df_orders, df_products){
  
  base <- df_order_items %>%
    inner_join(df_orders, by = "order_id") %>%
    inner_join(df_products, by = "product_id") %>%
    select(product_id, product_name, order_date, quantity, list_price = list_price.x, discount)
  
  base <- base %>%
    mutate(order_date = as.Date(order_date),  
           year = format(order_date, "%Y"),
           price = (1 - discount) * list_price,
           revenue = quantity * price) %>%
    select(product_id, product_name, year, revenue) %>%
    group_by(product_name, year) %>%
    summarise(total_revenue = sum(revenue)) %>%
    arrange(year, desc(total_revenue)) %>%
    group_by(year) %>% # bo wybuieramy potem top 1 z każdego roku
    slice(1)
  
  return(base)
  
}


ANS_TASK_03 <- TASK_03(df_order_items, df_orders, df_products)


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej

TASK_04 <- function(df_orders){
  
  base <- df_orders %>%
    mutate(order_date = as.Date(order_date),  
           year = format(order_date, "%Y")) %>%
    group_by(year, customer_id) %>%
    summarise(total_orders = n()) %>%
    group_by(year) %>%
    filter(total_orders == max(total_orders)) %>%
    summarise(total_customers = n(), 
              max_orders = max(total_orders)) 
  
  return(base)
  
}

ANS_TASK_04 <- TASK_04(df_orders)


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
TASK_05 <- function(df_orders){
  
  base <- df_customers %>%
    mutate(domain = sub(".*@", "", email)) %>% 
    select(customer_id, domain) %>% 
    inner_join(df_orders, by = "customer_id") %>%
    mutate(order_date = as.Date(order_date),  
           year = format(order_date, "%Y")) %>%
    group_by(year, domain) %>%
    summarise(total_orders = n()) %>%
    arrange(year, desc(total_orders)) %>%
    group_by(year) %>% # bo wybuieramy potem top 1 z każdego roku
    slice(1)
  
  return(base)
  
}


ANS_TASK_05 <- TASK_05(df_orders)


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej
TASK_06 <- function(df_orders, df_customers){
  
  base <- df_customers %>%
    filter(state == "CA" | state == "TX" ) %>%
    inner_join(df_orders, by = c("customer_id")) %>%
    group_by(customer_id) %>%
    slice(1) %>%
    group_by(state) %>%
    summarise(total_active = n())
  
  inactive_2018 <- df_customers %>% 
    filter(state %in% c("CA", "TX")) %>%
    anti_join(df_orders %>% 
                filter(strftime(order_date, '%Y') == "2018"), 
              by = "customer_id") %>% 
    group_by(state) %>%
    summarise(number_of_inactive_2018 = n(), .groups = 'drop')

  base <- base %>%
    inner_join(inactive_2018, by = c("state"))
  
  return(base)
  
}

ANS_TASK_06 <- TASK_06(df_orders, df_customers)


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
TASK_07 <- function(df_orders, df_order_items, df_customers){
  quantile_5 <- df_orders %>% 
    inner_join(df_order_items, by = 'order_id') %>% 
    mutate(thing_price = (1-discount)*list_price*quantity) %>% 
    group_by(order_id) %>% 
    summarise(order_price = sum(thing_price)) %>% 
    summarise(quantile(order_price, 0.05)) %>% 
    as.numeric()
  
  quantile_95 <- df_orders %>% 
    inner_join(df_order_items, by = 'order_id') %>% 
    mutate(thing_price = (1-discount)*list_price*quantity) %>% 
    group_by(order_id) %>% 
    summarise(order_price = sum(thing_price)) %>% 
    summarise(quantile(order_price, 0.95)) %>% 
    as.numeric()
  
  result <- df_orders %>% 
    inner_join(df_order_items, by = "order_id") %>% 
    inner_join(df_customers, by = "customer_id") %>%
    mutate(thing_price = (1-discount)*list_price*quantity) %>% 
    group_by(order_id) %>% 
    summarise(order_price = sum(thing_price)) %>% 
    filter(order_price < quantile_5 | order_price > quantile_95) %>%
    inner_join(df_orders, by = "order_id") %>%
    inner_join(df_customers, by = "customer_id") %>%
    select(customer_id, first_name, last_name) %>%
    group_by(customer_id) %>%
    slice(1)
  
  return(result)
  
}

ANS_TASK_07 <- TASK_07(df_orders,  df_order_items, df_customers)



####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.


TASK_08 <- function(df_orders) {
  
  start_date <- as.Date("2016-01-01")
  end_date <- as.Date("2018-12-31")
  
  all_dates <- data.frame(order_date = seq.Date(from = start_date, to = end_date, by = "day"))
  
  df_orders$order_date <- as.Date(df_orders$order_date)
  
  all_dates <- all_dates %>%
    mutate(year = strftime(order_date, '%Y'),
           quarter = as.integer((as.numeric(strftime(order_date, '%m')) - 1) / 3) + 1,
           month = strftime(order_date, '%m'),
           day = strftime(order_date, '%d'))

  daily_orders <- all_dates %>%
    left_join(df_orders, by = "order_date") %>%
    group_by(year, quarter, month, day) %>%
    summarise(daily_orders = sum(!is.na(order_id)), .groups = 'drop') %>%  # Zlicz zamówienia
    mutate(daily_orders = replace_na(daily_orders, 0))  # Zamień NA na 0
  
  
  result <- daily_orders %>%
    group_by(year, quarter) %>%
    summarise(daily_max = max(daily_orders, na.rm = TRUE),
              daily_min = min(daily_orders, na.rm = TRUE),
              daily_median = median(daily_orders, na.rm = TRUE), .groups = 'drop')
  
  return(result)
}

ANS_TASK_08 <- TASK_08(df_orders)


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
TASK_09 <- function(df_orders, df_customers){
  
  
  result <- df_orders %>% 
    inner_join(df_customers, by = "customer_id") %>% 
    filter(order_status == 4) %>%
    mutate(year = strftime(order_date, '%Y'),
           time_between = as.Date(shipped_date) - as.Date(order_date)) %>% 
    group_by(year, state) %>%
    summarise(mean_time = mean(time_between)) %>%
    pivot_wider(names_from = state, values_from = mean_time)
    
  
  return(result)
  
}

ANS_TASK_09 <- TASK_09(df_orders,  df_customers)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej
TASK_10 <- function(df_orders, df_customers){
  
  
  result <- df_orders %>% 
    inner_join(df_customers, by = "customer_id") %>% 
    mutate(year = strftime(order_date, '%Y')) %>% 
    group_by(customer_id, last_name) %>% #po customer id bo liczę, że dwie osoby z tym samym nazwiskiem to inne osoby:)
    summarise(years_ordered = n_distinct(year)) %>% # to nam zwróci ilość unikalnych wartości roku
    filter(years_ordered == 3) %>% # bo 2016, 2017 i 2018
    mutate(first_letter = substr(last_name, 1, 1)) %>% #wyciągamy  pierwszą literę
    group_by(first_letter) %>% 
    summarise(count = n())
  
  return(result)
  
}

ANS_TASK_10 <- TASK_10(df_orders,  df_customers)


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej
TASK_11 <- function(df_orders, df_order_items, df_products, df_categories){
  
  new_products <- df_orders %>%
    inner_join(df_order_items, by = 'order_id') %>%
    inner_join(df_products, by = 'product_id') %>%
    filter(model_year == as.integer(strftime(order_date, '%Y'))) %>%
    group_by(customer_id) %>%
    summarise(new_product_quantity = sum(quantity), .groups = 'drop')
  
  result <- df_orders %>%
    inner_join(df_order_items, by = 'order_id') %>% 
    inner_join(df_products, by = 'product_id') %>% 
    inner_join(df_categories, by = 'category_id') %>% 
    inner_join(df_customers, by = 'customer_id') %>% 
    group_by(customer_id, category_name) %>% 
    summarise(category_quantity = sum(quantity)) %>%
    pivot_wider(names_from = category_name, values_from = category_quantity, values_fill = 0) %>%
    left_join(new_products ,by = 'customer_id') %>%
    mutate(new_product_quantity = replace_na(new_product_quantity, 0)) 
    
  
  return(result)
  
}

ANS_TASK_11 <- TASK_11(df_orders, df_order_items, df_products, df_categories)

### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
TASK_12 <- function(df_orders, df_order_items){
  
  result <- df_orders %>%
    inner_join(df_order_items, by = 'order_id') %>% 
    mutate(day_of_the_week = strftime(order_date, '%A')) %>% 
    group_by(day_of_the_week) %>% 
    summarise(mean = weighted.mean(discount, quantity, na.rm = TRUE)) %>% 
    mutate(day_order = case_when(
      day_of_the_week == "poniedziałek" ~ 1,
      day_of_the_week == "wtorek" ~ 2,
      day_of_the_week == "środa" ~ 3,
      day_of_the_week == "czwartek" ~ 4,
      day_of_the_week == "piątek" ~ 5,
      day_of_the_week == "sobota" ~ 6,
      day_of_the_week == "niedziela" ~ 7
    )) %>% 
    arrange(day_order) %>%  
    select(day_of_the_week, mean)
  
  return(result)
  
}

ANS_TASK_12 <- TASK_12(df_orders, df_order_items)



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "WojterskaAda.rds")
