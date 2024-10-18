library(dplyr)
library(tidyr)
library(stringi)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')
df_staffs <- read.csv('homeworks/hw1/dane/staffs.csv')
df_stocks <- read.csv('homeworks/hw1/dane/stocks.csv')
df_stores <- read.csv('homeworks/hw1/dane/stores.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- inner_join(inner_join(df_orders |> select(order_id, customer_id,order_date), 
                                     df_customers |> select(customer_id,state), by='customer_id'),
                          inner_join(df_order_items |> select(order_id,product_id,quantity),
                                     df_products |> select(product_id,product_name, model_year), by='product_id'), 
                          by='order_id') |> 
  # przyjmujemy osobne grupowanie dla każdego roku, tzn. np. 1 kwartał 2016 i 1 kwartał 2017 są rozważane osobno
  group_by(order_date = paste(stri_sub(order_date,1,4),(as.numeric(stri_sub(order_date,6,7))-1)%/%3+1,sep='_'),
           state, product_name, model_year)  |> summarise(sum_quantity=sum(quantity)) |> group_by(order_date,state) |>
  filter(sum_quantity == max(sum_quantity)) |> select(-sum_quantity)

  
# Alternatywne rozwiązanie zwracające listę (z ties pogrupowanymi jako jeden element) przy użyciu group_map i sapply
  # sapply(
  # (inner_join(inner_join(df_orders |> select(order_id, customer_id,order_date), df_customers |> select(customer_id,state),
  #                    by='customer_id'),
  #                    inner_join(df_order_items |> select(order_id,product_id,quantity),
  #                               df_products |> select(product_id,product_name, model_year), by='product_id'), 
  #                    by='order_id') |> select(-c(customer_id, product_id, order_id)) |> 
  # mutate('order_date'=paste(stri_sub(order_date,1,4),(as.numeric(stri_sub(order_date,6,7))-1)%/%3+1,sep='_')) |>
  # group_by(product_name,model_year, order_date, state) |> summarise('sum_quantity'=sum(quantity)) |>
  # group_by(order_date,state) |> 
  # group_map( ~ slice_max(.x,.x$sum_quantity,n=1))),#with_ties=TRUE, więc w niektórych przypadkach jest kilka najczęstszych zakupów
  #                     select,-'sum_quantity' )


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu?

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders |> select(order_status, order_date) |> 
  # przyjmujemy osobne grupowanie dla każdego roku, tzn. np. styczeń 2016 i styczeń 2017 są rozważane osobno
  group_by(order_date = stri_sub(order_date,1,7)) |>
  summarise(orders_not_fulfilled_percent = 100*sum(order_status != 4)/n()) # przyjmujemy, że zamówienia niezrealizowane mają inny
                                                                           # order_status niż completed

# Alternatywne rozwiązanie zwracające listę przy użyciu group_map
# unlist(df_orders |> select(order_status, order_date) |> mutate('order_date'=stri_sub(order_date,1,7)) |>
# group_by(order_date) |> group_map( ~ 100*count(filter(.x, order_status < 4))/count(.x)))


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- inner_join(inner_join(df_orders |> select(order_date,order_id),
                                     df_order_items |> select(- item_id), by='order_id'),
                          df_products |> select(product_id, product_name), by='product_id') |>
  group_by(order_year = stri_sub(order_date,1,4), product_name) |>
  summarise(sum_revenue = sum(quantity*(list_price*(1 - discount)))) |> group_by(order_year) |>
  filter(sum_revenue  == max(sum_revenue)) |> select(-sum_revenue)


# Alternatywne rozwiązanie zwracające listę przy użyciu group_map i sapply
# sapply(
#   (inner_join(inner_join(df_orders |> select(order_date,order_id),
#                                 df_order_items |> select(- item_id), by='order_id'),
#                      df_products |> select(product_id, product_name), by='product_id') |>
#   mutate('order_date' = stri_sub(order_date,1,4), 'revenue' = quantity*(list_price*(1 - discount))) |>
#   select(order_date, product_name, revenue) |> group_by(order_date, product_name) |> summarise('sum_revenue'=sum(revenue)) |>
#   group_by(order_date) |> group_map( ~ slice_max(.x, .x$sum_revenue, n = 1))),
#   select,'product_name')

####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders |> select(customer_id, order_date) |> 
  group_by(order_year = stri_sub(order_date,1,4), customer_id) |> summarise(n_orders = n()) |>
  group_by(order_year) |> filter(n_orders == max(n_orders)) |> summarise(n_of_clients=n(), max_n_orders=max(n_orders))


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- inner_join(df_orders |> select(order_id, customer_id, order_date), df_customers |> select(customer_id,email),
                          by = 'customer_id') |> 
  group_by(year = stri_sub(order_date,1,4), email=stri_sub(stri_extract_all_regex(email,'@.+'),2)) |> summarise(n_orders = n()) |>
  arrange(-n_orders) |> filter(row_number()==1)


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(df_customers |> filter(state %in% c('CA', 'TX')) |> group_by(state) |>
                   summarise(clients_number_in_CA_TX = n()),
                 left_join(df_customers |> filter(state %in% c('CA', 'TX')) |> select(customer_id, state),
                           df_orders |> filter(stri_sub(order_date,1,4) == '2018') |> select(customer_id,order_date ),
                           by='customer_id') |> filter(is.na(order_date)) |> summarise(clients_without_orders_2018 =(n() != 0)) 
)


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl
#wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- inner_join(inner_join(df_order_items |> mutate(revenue = quantity*(list_price*(1 - discount))) |>
                                       group_by(order_id) |> summarise(sum_by_order=sum(revenue)),
                                     df_orders |> select(order_id,customer_id), by='order_id'),
                          df_customers, by='customer_id') |> 
  filter(sum_by_order < quantile(sum_by_order,0.05) | sum_by_order > quantile(sum_by_order,0.95)) |>
  select(-c(order_id,sum_by_order)) |> distinct()


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
 
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- left_join(tibble(days=as.character(seq(as.Date('2016-01-01'),as.Date('2018-12-31'),by='day'))),
                         df_orders |> group_by(order_date) |> summarise(number_per_day=n()) |>
                           mutate(days = as.character(order_date)), by='days') |>  
  mutate(number_per_day = ifelse(is.na(number_per_day), 0, number_per_day),
         # przyjmujemy osobne grupowanie dla każdego roku, tzn. np. 1 kwartał 2016 i 1 kwartał 2017 są rozważane osobno       
         year_quater=paste(stri_sub(days,1,4),(as.numeric(stri_sub(days,6,7))-1)%/%3+1,sep='_'),
         days = NULL, order_date = NULL) |> group_by(year_quater) |>
  summarise(min=min(number_per_day),median=median(number_per_day), max=max(number_per_day))


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- pivot_wider(
  inner_join(df_orders |> select(customer_id, order_date, shipped_date) |> filter(shipped_date != 'NULL'),
  df_customers |> select(customer_id, state), by='customer_id') |>
  select(-customer_id) |> group_by(Year = stri_sub(order_date,1,4),state) |> # przyjmujemy rok = rok zamówienia (a nie wysyłki)
  summarise(time_mean = mean(difftime(as.Date(shipped_date),as.Date(order_date)))),
  
                           names_from = state,values_from = time_mean)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- inner_join(df_orders |> select(customer_id, order_date), df_customers |> select(customer_id, last_name) |>
                            mutate(last_name = stri_sub(last_name,1,1)), by = 'customer_id') |> group_by(customer_id, last_name) |>
  summarise(annually = n_distinct(stri_sub(order_date,1,4))) |> filter(annually == 3) |> group_by(first_letter = last_name) |>
  summarise(count = n())


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku,
# kiedy złożono zamówienie)

temp <- df_orders |> select(order_id, order_date, customer_id) |>
  inner_join(df_order_items |> select(order_id, product_id, quantity), by = 'order_id') |>
  inner_join(df_products |> select(product_id, category_id, model_year), by = 'product_id') |>
  inner_join(df_categories, by='category_id') 

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- inner_join(
  temp |> group_by(customer_id, category_name) |>  summarise(sum_quantity = sum(quantity)) |>
    pivot_wider(names_from=category_name, values_from=sum_quantity, values_fill=0),
  temp |> group_by(customer_id) |>
    summarise(new_model_count= sum(quantity * ifelse(stri_sub(order_date, 1, 4) == model_year,1,0))),
  by = 'customer_id')  


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym
# uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- inner_join(df_orders |> select(order_id, order_date), df_order_items |>
             select(order_id, product_id, quantity, list_price, discount), by='order_id') |>
  group_by(week_day=strftime(order_date, "%A"), product_id) |>
  summarise(discount_mean = 100*(1-sum(quantity*(1-discount)*list_price)/sum(quantity*list_price)))
    
    



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "BojskiMikolaj.rds")
