library(dplyr);
library(tidyr);

df_orders <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/orders.csv');
df_order_items <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/order_items.csv');
df_products <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/products.csv');
df_brands <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/brands.csv');
df_categories <-  read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/categories.csv');
df_customers <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/customers.csv');
df_staffs <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/staffs.csv');
df_stocks <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/stocks.csv');
df_stores <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/stores.csv');

head(df_orders)
head(df_order_items)
head(df_products)
head(df_brands)
head(df_categories)
head(df_customers)
head(df_staffs)
head(df_stocks)
head(df_stores)

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- function(df_orders, df_order_items, df_customers, df_products) {
  
  o0 <- select(df_orders, order_id, customer_id, order_date)
  c0 <- select(df_customers, customer_id, state)
  oi0 <- select(df_order_items, order_id, item_id, product_id, quantity)
  t0 <- left_join(oi0, o0)
  p0 <- select(df_products, product_id, product_name)
  t1 <- left_join(t0, c0)
  t2 <- left_join(t1, p0)
  t2 <- mutate(t2, order_date = as.Date(order_date, format = "%Y-%m-%d"))
  t2 <- mutate(t2, month = as.integer(format(order_date, "%m")))
  t2 <- select(t2, product_id, quantity, state, product_name, month)
  t2 <- mutate(t2, quarter = case_when(1 <= month & month <= 3 ~ 1, 
                                       4 <= month & month <= 6 ~ 2,
                                       7 <= month & month <= 9 ~ 3, 
                                       10 <= month & month <= 12 ~ 4))
  
  t2 <- t2 %>%
    group_by(quarter, state) %>%
    count(product_name, wt = quantity) %>% 
    slice(which.max(n))
  
  t2
}

ANS_TASK_01(df_orders, df_order_items, df_customers, df_products)

####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej

ANS_TASK_02 <- function(df_orders) {
  
  o <- select(df_orders, order_id, order_status, order_date)
  o <- mutate(o, order_date = as.Date(order_date, format = "%Y-%m-%d"))
  o <- mutate(o, month = as.integer(format(order_date, "%m")))
  
  o <- o %>% 
    group_by(month) %>% 
    summarise(percent_undelivered = (sum(order_status != 4)/n())*100, .groups = "drop")
  
  o
}

ANS_TASK_02(df_orders)


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- function(df_orders, df_order_items, df_products) {
  
  o <- df_orders %>% 
    select(order_id, order_date) %>% 
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d")) %>% 
    mutate(year = as.integer(format(order_date, "%Y"))) %>% 
    select(-order_date)
  
  oi <- df_order_items %>% 
    select(order_id, product_id, quantity, list_price, discount) %>% 
    mutate(profit = list_price * (1 - discount) * quantity) %>% 
    select(-c(quantity, list_price, discount))
  
  p <- df_products %>% 
    select(product_id, product_name)
  
  t <- oi %>% 
    left_join(o) %>% 
    left_join(p) %>% 
    group_by(year, product_name) %>%
    summarize(product_profit = sum(profit), .groups = 'drop') %>%
    arrange(year, desc(product_profit)) %>%
    group_by(year) %>%
    slice(1) %>%  
    ungroup()
  
  t
}

ANS_TASK_03(df_orders, df_order_items, df_products)

####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej

ANS_TASK_04 <- function(df_orders) {
  
  o <- df_orders %>% 
    select(order_id, customer_id, order_date) %>% 
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d")) %>% 
    mutate(year = as.integer(format(order_date, "%Y"))) %>% 
    select(-order_date) %>% 
    group_by(year, customer_id) %>% 
    tally() %>% 
    rename(number_of_orders = n) %>% 
    arrange(year, desc(number_of_orders)) %>%
    group_by(year) %>% 
    filter(number_of_orders == max(number_of_orders)) %>% 
    summarize(number_of_orders = first(number_of_orders),
              n_of_cus_with_max_orders = n(), .groups = 'drop')
  
  o
}

ANS_TASK_04(df_orders)

####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej

ANS_TASK_05 <- function(df_orders, df_customers) {
  
  o <- df_orders %>% 
    select(order_id, customer_id, order_date) %>% 
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d")) %>% 
    mutate(year = as.integer(format(order_date, "%Y"))) %>% 
    select(-order_date)
  
  cus <- df_customers %>% 
    select(customer_id, email)
  
  t <- o %>% 
    left_join(cus) %>% 
    separate(email, into = c('user', 'domain'), sep = '@') %>% 
    select(-user) %>% 
    group_by(year, domain) %>% 
    summarize(domain_encounters = n(), .groups = 'drop') %>% 
    arrange(year, desc(domain_encounters)) %>% 
    group_by(year) %>% 
    slice(1)
  
  t
}

ANS_TASK_05(df_orders, df_customers)
  
####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej

ANS_TASK_06 <- function(df_customers, df_orders) {
  
  o <- df_orders %>% 
    select(order_id, customer_id, order_date) %>% 
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d")) %>% 
    mutate(year = as.integer(format(order_date, "%Y"))) %>% 
    select(-order_date) %>% 
    filter(year == 2018)
  
  cus <- df_customers %>% 
    select(customer_id, state) %>% 
    filter(state == "CA" | state == "TX")
  
  n_a_cus_ca_tx = nrow(cus)
  
  n_a_cus_ca_tx_o_2018 <- o %>% 
    left_join(cus) %>% 
    filter(!is.na(state)) %>% 
    distinct(customer_id) %>% 
    nrow()
  
  ans <- c(n_a_cus_ca_tx, n_a_cus_ca_tx - n_a_cus_ca_tx_o_2018 > 0)
  ifelse(ans[2] == 1, a <- c(ans[1], "TRUE"), a<- c(ans[1], "FALSE"))
  a
}

ANS_TASK_06(df_customers, df_orders)


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej

ANS_TASK_07 <- function(df_orders, df_customers, df_order_items) {
  
  cus <- df_customers %>% 
    select(customer_id, first_name, last_name) %>% 
    mutate(name = paste(first_name, last_name, sep = " ")) %>% 
    select(customer_id, name)
  
  o <- df_orders %>% 
    select(order_id, customer_id) 
  
  oi <- df_order_items %>% 
    select(order_id, product_id, quantity, list_price, discount) %>% 
    mutate(profit = list_price * (1 - discount) * quantity) %>% 
    select(-c(quantity, list_price, discount)) %>% 
    group_by(order_id) %>% 
    summarize(value_of_order = sum(profit))
  
  t <- oi %>% 
    left_join(o) %>% 
    left_join(cus) %>% 
    mutate(quantile = ntile(value_of_order, 20)) %>% 
    arrange(desc(quantile)) %>% 
    filter(quantile == 1 | quantile == 20) %>% 
    arrange(desc(value_of_order))
  t
}

ANS_TASK_07(df_orders, df_customers, df_order_items)

####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej

ANS_TASK_08 <- function(df_orders) {
  
  o <- df_orders %>% 
    select(order_id, order_date) %>% 
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d")) %>% 
    mutate(month = as.integer(format(order_date, "%m"))) %>% 
    # mutate(day = as.integer(format(order_date, "%d"))) %>%
    mutate(quarter = case_when(1 <= month & month <= 3 ~ 1, 
                               4 <= month & month <= 6 ~ 2,
                               7 <= month & month <= 9 ~ 3, 
                               10 <= month & month <= 12 ~ 4)) %>% 
    mutate(day_month = as.character(format(order_date, "%m-%d"))) %>% 
    select(-month)
  
  o1 <- o %>%
    group_by(order_date, quarter) %>% 
    tally() %>% 
    rename(n_of_orders = n) %>% 
    left_join(o) %>% 
    ungroup() %>% 
    group_by(day_month, quarter) %>% 
    summarise(n_per_day_of_year = sum(n_of_orders)) %>% 
    ungroup() %>% 
    group_by(quarter) %>%
    summarise(max_of_the_day = max(n_per_day_of_year),
              min_of_the_day = min(n_per_day_of_year),
              mean_of_the_day = mean(n_per_day_of_year))
  o1
}

ANS_TASK_08(df_orders)
# Tak, wiem, ze minimalna ilosc w kwartale 3 i 4 musi byc 0, ale nie zdazylem tego zrobic
  
####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- NA


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- NA


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- NA


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- NA



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "SochaKarol.rds")
