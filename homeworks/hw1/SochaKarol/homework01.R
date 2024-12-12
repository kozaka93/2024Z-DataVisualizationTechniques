library(dplyr);
library(tidyr);
library(stringr);

df_orders <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/orders.csv');
df_order_items <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/order_items.csv');
df_products <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/products.csv');
df_brands <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/brands.csv');
df_categories <-  read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/categories.csv');
df_customers <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/customers.csv');
df_staffs <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/staffs.csv');
df_stocks <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/stocks.csv');
df_stores <- read.csv('C:/Users/kerel/OneDrive/Dokumenty/R_scripts/TWD_projects/2024Z-DataVisualizationTechniques/homeworks/hw1/dane/stores.csv');



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



####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej

ANS_TASK_02 <- function(df_orders) {
  
  o <- df_orders %>% 
    select(order_id, order_status, order_date) %>% 
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d"),
           month = as.integer(format(order_date, "%m"))) %>% 
    group_by(month) %>% 
    summarize(percent_undelivered = sum(order_status != 4)/n()*100)
  
  o
}



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
    summarize(max_number_of_orders = first(number_of_orders),
              n_of_cus_with_max_orders = n(), .groups = 'drop')
  
  o
}



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



####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej

ANS_TASK_08 <- function(df_orders) {
  
  o <- df_orders %>% 
    select(order_id, order_date) %>% 
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d")) %>% 
    mutate(month = as.integer(format(order_date, "%m"))) %>% 
    mutate(quarter = case_when(1 <= month & month <= 3 ~ 1, 
                               4 <= month & month <= 6 ~ 2,
                               7 <= month & month <= 9 ~ 3, 
                               10 <= month & month <= 12 ~ 4)) %>% 
    mutate(day_month = as.character(format(order_date, "%m-%d"))) %>% 
    select(-month)
  
  all_dates <- data.frame(seq(as.Date("2016-01-01"),as.Date("2016-12-31"),by="1 day"))
  
  colnames(all_dates) <- c("dates")
  
  all_dates <- all_dates %>% 
    mutate(day_month = as.character(format(dates, "%m-%d")),
           month = as.integer(format(dates, "%m"))) %>% 
    select(day_month, month) %>% 
    mutate(quarter = case_when(1 <= month & month <= 3 ~ 1, 
                               4 <= month & month <= 6 ~ 2,
                               7 <= month & month <= 9 ~ 3, 
                               10 <= month & month <= 12 ~ 4))
  
  o1 <- o %>%
    group_by(order_date, quarter) %>% 
    tally() %>% 
    rename(n_of_orders = n) %>% 
    left_join(o) %>% 
    ungroup() %>% 
    group_by(day_month, quarter) %>% 
    summarise(n_per_day_of_year = sum(n_of_orders)) %>% 
    ungroup() 
  
  o2 <- all_dates %>% 
    left_join(o1, by = c("day_month", "quarter")) %>% 
    mutate(n_per_day_of_year = replace_na(n_per_day_of_year, 0)) %>% 
    group_by(quarter) %>%
    summarise(max_of_the_day = max(n_per_day_of_year),
              min_of_the_day = min(n_per_day_of_year),
              mean_of_the_day = mean(n_per_day_of_year))
  
  o2
}



####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej

ANS_TASK_09 <- function(df_orders, df_customers) {
  
  o <- df_orders %>% 
    select(order_id, customer_id, order_status, order_date, shipped_date) %>% 
    filter(order_status == 4)
  
  cus <- df_customers %>% 
    select(customer_id, state)
  
  o1 <- o %>% 
    left_join(cus) %>% 
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d"),
           shipped_date = as.Date(shipped_date, format = "%Y-%m-%d")) %>% 
    mutate(o_year = as.integer(format(order_date, "%Y")),
           o_month = as.integer(format(order_date, "%m")),
           o_day = as.integer(format(order_date, "%d")),
           d_year = as.integer(format(shipped_date, "%Y")),
           d_month = as.integer(format(shipped_date, "%m")),
           d_day = as.integer(format(shipped_date, "%d"))) %>% 
    mutate(przestepny = case_when(o_year %% 4 == 0 ~ 1,
                                  o_year %% 4 != 0 ~ 0)) %>% 
    mutate(days_in_month = case_when(o_month %in% c(1, 3, 5, 7, 8, 10, 12) ~ 31,
                                     o_month %in% c(4, 6, 9, 11) ~ 30,
                                     o_month == 2 & przestepny == 0 ~ 28,
                                     o_month == 2 & przestepny == 1 ~ 29)) %>% 
    select(order_id, customer_id, state, o_year, o_day, d_day, days_in_month) %>% 
    mutate(delivery_time = case_when(d_day - o_day >= 0 ~ d_day - o_day,
                                     d_day - o_day < 0 ~ d_day - o_day + days_in_month)) %>% 
    select(order_id, customer_id, state, o_year, delivery_time) %>% 
    group_by(o_year, state) %>% 
    summarize(mean_delivery_time_in_days = mean(delivery_time)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = state, values_from = mean_delivery_time_in_days)
  
  o1
}


  
####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej

ANS_TASK_10 <- function(df_orders, df_customers) {
  
  o <- df_orders %>% 
    select(order_id, customer_id, order_date)
  
  cus <- df_customers %>% 
    select(customer_id, last_name)
  
  t <- o %>% 
    left_join(cus) %>% 
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d"),
           year = as.integer(format(order_date, "%Y"))) %>% 
    arrange(last_name) %>% 
    select(last_name, year)
  
  t_2016 <- t %>% 
    filter(year == 2016) %>% 
    select(last_name)
  
  t_2017 <- t %>% 
    filter(year == 2017) %>% 
    select(last_name)
  
  t_2018 <- t %>% 
    filter(year == 2018) %>% 
    select(last_name)
  
  t_inter <- intersect(t_2016, t_2017)
  
  t_inter <- intersect(t_inter, t_2018)
  
  t_letters <- t_inter %>%
    mutate(letters = str_split(last_name, "")) %>%
    unnest(letters) %>%
    group_by(last_name, letters) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    arrange(letters) %>%
    group_by(letters) %>%
    summarize(occurances = sum(count))
  
  t_letters 
  
}



####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

## Odpowiedz przypisana do zmiennej

ANS_TASK_11 <- function(df_orders, df_order_items, df_customers, df_products, df_categories) {
  
  o <- df_orders %>% 
    select(order_id, customer_id, order_date) %>% 
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d"),
           o_year = as.integer(format(order_date, "%Y"))) %>% 
    select(-order_date)
  
  oi <- df_order_items %>% 
    select(-c(list_price, discount))
  
  cus <- df_customers %>% 
    select(customer_id, first_name, last_name) %>% 
    mutate(customer_name = paste(first_name, last_name, sep = " ")) %>% 
    select(-c(first_name, last_name))
  
  p <- df_products %>% 
    select(-c(brand_id,product_name, list_price))
  
  c <- df_categories
  
  t <- o %>% 
    left_join(cus) %>% 
    left_join(oi) %>% 
    left_join(p) %>% 
    left_join(c) %>% 
    select(-c(item_id, category_id, customer_id))
  
  t1 <- t %>% 
    group_by(customer_name, category_name) %>% 
    summarize(quantity_of_category = sum(quantity)) %>% 
    pivot_wider(names_from = category_name, values_from = quantity_of_category) %>% 
    replace(is.na(t1), 0) %>% 
    ungroup()
  
  t2 <- t %>% 
    mutate(newest_quantity = case_when(o_year == model_year ~ quantity,
                                       o_year != model_year ~ 0)) %>% 
    group_by(customer_name) %>% 
    summarize(how_many_newest = sum(newest_quantity)) %>% 
    ungroup()
  
  t3 <- t1 %>% 
    left_join(t2)
  
  t3
}


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej

ANS_TASK_12 <- function(df_orders, df_order_items, df_products) {
  
  oi <- df_order_items %>% 
    select(order_id, product_id, quantity, list_price, discount)
  
  p <- df_products %>% 
    select(product_id, product_name)
  
  o <- df_orders %>% 
    select(order_id, order_date)
  
  t <- oi %>% 
    left_join(o) %>% 
    left_join(p) %>% 
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d"),
           day_of_week = weekdays(order_date),
           list_profit = quantity * list_price,
           real_profit = list_price * (1 - discount) * quantity) %>% 
    select(-c(product_id, order_date, quantity, list_price, discount)) %>%
    group_by(product_name, day_of_week) %>% 
    summarize(sum_list_profit = sum(list_profit),
              sum_real_profit = sum(real_profit)) %>% 
    mutate(discount = (sum_list_profit - sum_real_profit) / sum_list_profit * 100) %>% 
    select(-c(sum_list_profit, sum_real_profit)) %>% 
    pivot_wider(names_from = day_of_week, values_from = discount) %>% 
    relocate(product_name, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>% 
    left_join(p) %>% 
    relocate(product_id) %>% 
    arrange(product_id) 
  
  t
}



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "SochaKarol.rds")

