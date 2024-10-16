library(dplyr)
library(tidyr)

df_orders <- read.csv("dane/orders.csv")
df_order_items <- read.csv("dane/order_items.csv")
df_products <- read.csv("dane/products.csv")
df_brands <- read.csv("dane/brands.csv")
df_categories <- read.csv("dane/categories.csv")
df_stores <- read.csv("dane/stores.csv")
df_customers <- read.csv("dane/customers.csv")
df_staffs <- read.csv("dane/staffs.csv")

str(df_orders)
str(df_order_items)
str(df_products)
str(df_brands)
str(df_categories)
str(df_stores)

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci?
# Podaj nazwę produktu i rok jego produkcji.

res1 <- df_orders %>%
    mutate(order_date = as.Date(order_date)) %>%
    mutate(quarter = case_when(
        format(order_date, "%m") %in% c("01", "02", "03") ~ "Q1",
        format(order_date, "%m") %in% c("04", "05", "06") ~ "Q2",
        format(order_date, "%m") %in% c("07", "08", "09") ~ "Q3",
        TRUE ~ "Q4"
    )) %>% 
    select(order_id, customer_id, quarter) %>% 
    merge(df_customers[, c("customer_id", "state")], by = "customer_id") %>% 
    merge(df_products[, c("product_id", "product_name", "model_year")]) %>% 
    group_by(quarter, state, product_name, model_year) %>% 
    summarise(total_purchases = n(), .groups = 'drop') %>% 
    group_by(quarter, state) %>% 
    top_n(1, total_purchases) %>% 
    ungroup() %>% 
    distinct(product_name, model_year)

View(res1)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- res1


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 
all_orders <- df_orders  %>% 
    mutate(order_date = as.Date(order_date)) %>% 
    mutate(order_month = format(order_date, "%m")) %>% 
    group_by(order_month) %>% 
    summarise(order_count = n())

rejected_orders <- df_orders  %>% 
    mutate(order_date = as.Date(order_date)) %>% 
    mutate(order_month = format(order_date, "%m")) %>% 
    filter(order_status == 3) %>% 
    group_by(order_month) %>% 
    summarise(order_count = n(),  .groups = 'drop') %>% 
    merge(order_month)

res2 <- all_orders %>% 
    left_join(rejected_orders, by = "order_month") %>%
    replace_na(list(order_count.y = 0)) %>% 
    mutate(percent_of_rejected = order_count.y/order_count.x *100 ) %>% 
    select(order_month, percent_of_rejected)   

View(res2)

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- res2  

####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
res3 <- df_orders  %>% 
    mutate(order_date = as.Date(order_date)) %>% 
    mutate(order_year = format(order_date, "%Y")) %>% 
    select(order_year, order_id) %>% 
    merge(df_order_items, by = 'order_id') %>% 
    merge(df_products[, c('product_name', 'product_id')], by = 'product_id') %>% 
    mutate(order_income = (list_price*(1-discount)*quantity)) %>% 
    group_by(order_year, product_name) %>% 
    summarise(total_year_income_product = sum(order_income )) %>% 
    top_n(1, total_year_income_product) %>% 
    select(order_year, product_name)

View(res3)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- res3


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 
res4 <- df_orders %>% 
    mutate(order_date = as.Date(order_date)) %>% 
    mutate(order_year = format(order_date, "%Y")) %>% 
    group_by(customer_id, order_year) %>% 
    summarise(order_count = n(), .groups = 'drop') %>% 
    group_by(order_year) %>% 
    top_n(1, order_count) %>% 
    group_by(order_year, order_count) %>% 
    summarise(customer_count = n())

View(res4)

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- res4


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
res5 <- df_orders %>% 
    mutate(order_date = as.Date(order_date)) %>% 
    mutate(order_year = format(order_date, "%Y")) %>% 
    merge(df_staffs) %>% 
    mutate(domain = sapply(strsplit(email, "@"), `[`, 2)) %>% 
    group_by(order_year, domain) %>% 
    summarise(order_count = n()) 

View(res5)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- res5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
res6_1 <- df_orders %>% 
    merge(df_stores, by='store_id') %>% 
    filter(state %in% c("CA", "TX")) %>% 
    summarise(customers_count = n_distinct(customer_id), .groups = 'drop')

# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?
customers_2018 <- df_orders %>% 
    mutate(order_date = as.Date(order_date)) %>% 
    mutate(order_year = format(order_date, "%Y")) %>% 
    filter(order_year == 2018) %>% 
    distinct(customer_id)

customers_all <- df_customers %>% 
    filter(state %in% c("CA", "TX")) %>% 
    select(customer_id)

res6_2 <- !all(customers_all %in% customers_2018)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- list(res6_1, res6_2)
ANS_TASK_06

####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
df_orders_income <- df_orders %>%
    merge(df_order_items, by = 'order_id') %>%
    mutate(order_income = list_price * (1 - discount) * quantity) %>% 
    group_by(order_id) %>% 
    summarise(customer_order_value = sum(order_income), .groups = 'drop')

q05 <- quantile(df_orders_income$customer_order_value , 0.05)
q95 <- quantile(df_orders_income$customer_order_value , 0.95)

extreme_customers <- df_orders_income %>%
    merge(df_orders, by = 'order_id') %>% 
    filter(customer_order_value  < q05 | customer_order_value  > q95) %>%
    select(customer_id)

# Wyświetlenie ekstremalnych klientów
extreme_customers

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- extreme_customers

####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
res8 <- df_orders %>%
    mutate(order_date = as.Date(order_date)) %>%
    group_by(order_date) %>% 
    summarise(order_count = n(), .groups = 'drop') %>% 
    mutate(quarter = case_when(
        format(order_date, "%m") %in% c("01", "02", "03") ~ "Q1",
        format(order_date, "%m") %in% c("04", "05", "06") ~ "Q2",
        format(order_date, "%m") %in% c("07", "08", "09") ~ "Q3",
        TRUE ~ "Q4"
    )) %>% 
    group_by(quarter) %>% 
    summarise(max = max(order_count), min = min(order_count), median = median(order_count))

View(res8)

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- res8

####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
res9 <- df_orders %>%
    mutate(order_date = as.Date(order_date),
           shipped_date = as.Date(shipped_date),
           delivery_time = as.numeric(shipped_date-order_date)) %>%  
    merge(df_customers, by = 'customer_id') %>%      
    mutate(order_year = format(order_date, "%Y")) %>%  
    group_by(order_year, state) %>%                  
    summarise(avg_delivery_time = mean(delivery_time, na.rm = TRUE)) %>% 
    pivot_wider(names_from = state,                
                values_from = avg_delivery_time)

View(res9)

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- res9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
res10 <- df_orders %>% 
    merge(df_customers, by = 'customer_id') %>% 
    mutate(order_date = as.Date(order_date),
            order_year = format(order_date, "%Y")) %>% 
    group_by(customer_id) %>% 
    summarise(year_count = n_distinct(order_year), .groups = 'drop') %>% 
    merge(df_customers, by = 'customer_id') %>% 
    filter(year_count == 3) %>% 
    mutate(first_letter = substr(last_name, 1, 1)) %>% 
    group_by(first_letter) %>% 
    summarise(letter_count = n(), .groups = 'drop')

View(res10)

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- res10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

df_orders_products <- df_orders %>%
    merge(df_order_items, by = 'order_id') %>% 
    merge(df_products, by = 'product_id') %>% 
    merge(df_customers, by = 'customer_id')

df_orders_products <- df_orders_products %>%
    mutate(order_year = format(as.Date(order_date), "%Y"),
        is_latest = ifelse(order_year == model_year, 1, 0))

category_summary <- df_orders_products %>%
    group_by(customer_id, category_id) %>%
    summarise(bike_count = n(), .groups = 'drop') %>% 
    pivot_wider(names_from = category_id,
                values_from = bike_count,
                values_fill = 0)

latest_product_summary <- df_orders_products %>%
    group_by(customer_id) %>%                           
    summarise(latest_bike_count = sum(is_latest), .groups = 'drop')

res11 <- category_summary %>%
    merge(latest_product_summary, by = 'customer_id', all = TRUE)

View(res11)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- res11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
res12 <- df_orders %>%
  merge(df_order_items, by = 'order_id') %>%
  mutate(order_date = as.Date(order_date),
         week_day = weekdays(order_date),
         list_price_income = list_price * quantity,
         actual_income = (list_price * (1 - discount) * quantity),
         discount_percentage = (1 - actual_income / list_price_income) * 100) %>%  
  group_by(product_id, week_day) %>%              
  summarise(avg_discount = mean(discount_percentage, na.rm = TRUE), .groups = 'drop') %>%
  merge(df_products, by = 'product_id') %>%         
  select(product_name, week_day, avg_discount) %>%  
  arrange(product_name, week_day) 

View(res12)  


## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- res12


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "KwokaKlaudia.rds")
