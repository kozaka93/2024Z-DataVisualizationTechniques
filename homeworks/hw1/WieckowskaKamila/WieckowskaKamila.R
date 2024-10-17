library(dplyr)
library(tidyr)
library(lubridate)



# df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
# df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
# df_products <- read.csv('homeworks/hw1/dane/products.csv')
# df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
# df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')

setwd("C:/Studia/3 sem/techniki wizualizacji/homeworks")


df_orders <- read.csv('hw1/dane/orders.csv')
df_order_items <- read.csv('hw1/dane/order_items.csv')
df_products <- read.csv('hw1/dane/products.csv')
df_brands <- read.csv('hw1/dane/brands.csv')
df_categories <-  read.csv('hw1/dane/categories.csv')
df_customers <- read.csv('hw1/dane/customers.csv')
df_staffs <- read.csv('hw1/dane/staffs.csv')
df_stocks <- read.csv('hw1/dane/stocks.csv')
df_stores <- read.csv('hw1/dane/stores.csv')




###################################################################################################################

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.



  

# Odpowiedz przypisana do zmiennej
ANS_TASK_01 <-  df_order_items %>% 
  left_join(df_orders, by = "order_id") %>% 
  left_join(df_stores, by = 'store_id') %>% 
  select(product_id, quantity, order_date, state) %>% 
  mutate(order_date = as.Date(order_date)) %>% 
  mutate(month = as.integer(format(order_date, '%m'))) %>% 
  mutate(quarter = case_when(
    month %in% 1:3 ~ 1,
    month %in% 4:6 ~ 2,
    month %in% 7:9 ~ 3,
    month %in% 10:12 ~ 4
  )) %>% 
  group_by(state, quarter, product_id) %>%
  summarise(total_quantity = sum(quantity), .groups = 'drop') %>%
  group_by(state, quarter) %>% 
  slice_max(order_by = total_quantity) %>% 
  select(state, quarter, product_id, total_quantity) %>% 
  left_join(df_products, by = 'product_id') %>% 
  select(state, quarter, product_name, model_year)



###################################################################################################################


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu?    



## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% 
  select(order_id, order_status, order_date) %>% 
  mutate(order_date = as.Date(order_date)) %>% 
  mutate(month = as.integer(format(order_date, '%m')), year = as.integer(format(order_date, '%Y'))) %>% 
  group_by(year, month) %>% 
  select(-order_date) %>% 
  summarise(total_orders_count = n(), rejected_orders = sum(order_status != 4), percentage_of_rejected = (rejected_orders/total_orders_count)*100) %>%  
  select(year, month, percentage_of_rejected)




###################################################################################################################


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?





## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>% 
  left_join(df_orders, by = 'order_id') %>% 
  mutate(total_sum = list_price*(1-discount)*quantity, order_date = as.Date(order_date), year = as.integer(format(order_date, '%Y'))) %>% 
  group_by(year, product_id) %>% 
  summarise(income = sum(total_sum), .groups = 'drop') %>% 
  group_by(year) %>% 
  slice_max(order_by = income) %>% 
  left_join(df_products, by = 'product_id') %>% 
  select(year, product_name, income)






###################################################################################################################


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 





## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% 
  select(order_id, customer_id, order_date) %>% 
  mutate(order_date = as.Date(order_date), year = as.integer(format(order_date, '%Y'))) %>% 
  select(-c(order_date, order_id)) %>% 
  group_by(year, customer_id) %>% 
  summarise(number_of_orders = n()) %>%
  slice_max(order_by = number_of_orders) %>% 
  group_by(year) %>% 
  summarise(number_of_customers = n(), .groups = 'drop', number_of_orders = sum(number_of_orders)/number_of_customers)




###################################################################################################################


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?



## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_orders %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  mutate(order_date = as.Date(order_date), year = as.integer(format(order_date, '%Y'))) %>% 
  select(year, email) %>% 
  group_by(year, email) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  group_by(year) %>% 
  slice_max(order_by = count)





###################################################################################################################


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie?      CO TO ZNACZY AKTYWNY????????????????
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?




## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- df_customers %>% 
  full_join(df_orders, by = 'customer_id') %>% 
  filter(is.na(order_id) == FALSE) %>%    # ODRZUCAMY NIEAKTYWNYCH KLIENTOW
  select(customer_id, state) %>% 
  filter(state == 'CA' | state == 'TX') %>% 
  group_by(state, customer_id) %>% 
  summarise(.groups = 'drop') %>% 
  left_join(df_orders, by = 'customer_id') %>% 
  mutate(order_date = as.Date(order_date), year = as.integer(format(order_date, '%Y'))) %>% 
  select(state, customer_id, year) %>% 
  group_by(customer_id) %>% 
  summarise(orders_all = any(year == 2016, year == 2017, year == 2018),
            orders_18 = any(year == 2018), .groups = 'drop') %>% 
  filter(orders_18 == FALSE) %>% 
  nrow()    # ilsoc osob ktore nie zlozyly zamowieni w 2018




###################################################################################################################


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?




## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_order_items %>% 
  mutate(price_of_product = list_price*(1-discount)*quantity) %>% 
  group_by(order_id) %>% 
  summarise(order_price = sum(price_of_product), .groups = 'drop') %>% 
  mutate(quantile_5 = quantile(order_price, probs = 0.05),
         quantile_95 = quantile(order_price, probs = 0.95)) %>% 
  filter(order_price < quantile_5 | order_price > quantile_95) %>% 
  left_join(df_orders, by = 'order_id') %>% 
  select(customer_id)



###################################################################################################################


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.




## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>% 
  mutate(order_date = as.Date(order_date)) %>% 
  mutate(month = as.integer(format(order_date, '%m')), day = as.integer(format(order_date, '%d'))) %>% 
  mutate(quarter = case_when(
    month %in% 1:3 ~ 1,
    month %in% 4:6 ~ 2,
    month %in% 7:9 ~ 3,
    month %in% 10:12 ~ 4
  )) %>% 
  group_by(quarter, day) %>% 
  summarise(number_of_orders = n(), .groups = 'drop') %>% 
  group_by(quarter) %>% 
  summarise(minimum = min(number_of_orders), maximum = max(number_of_orders), median = median(number_of_orders))




###################################################################################################################


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie





## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- 
  df_orders %>% 
  mutate(order_date = as.Date(order_date), shipped_date = as.Date(shipped_date)) %>%                 
  filter(is.na(shipped_date) == FALSE) %>% 
  mutate(delivery_time = as.numeric(shipped_date - order_date), year = as.integer(format(order_date, '%Y'))) %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  select(delivery_time, year, state) %>% 
  group_by(year, state) %>% 
  summarise(average_delivery_time = mean(delivery_time), .groups = 'drop') %>% 
  pivot_wider(names_from = state, values_from = average_delivery_time)









###################################################################################################################


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.




## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_orders %>% 
  select(customer_id, order_date) %>% 
  mutate(order_date = as.Date(order_date), year = as.integer(format(order_date, '%Y')), ) %>% 
  group_by(year, customer_id) %>% 
  mutate(cnt16 = case_when(
    year == 2016 ~ 1,
    TRUE ~ 0
  ),
  cnt18 = case_when(
    year == 2018 ~ 1,
    TRUE ~ 0
  ), cnt17 = case_when(
    year == 2017 ~ 1,
    TRUE ~ 0
  )) %>% 
  group_by(customer_id) %>% 
  summarise(cnt16 = sum(cnt16), cnt17 = sum(cnt17), cnt18 = sum(cnt18)) %>% 
  filter(cnt16 != 0 & cnt17 != 0 & cnt18 != 0) %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  select(last_name) %>% 
  mutate(firt_letter = substr(last_name, 1, 1)) %>% 
  group_by(firt_letter) %>% 
  summarise(n = n())








###################################################################################################################


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)





## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_order_items %>% 
  left_join(df_orders, by = 'order_id') %>% 
  left_join(df_products, by = 'product_id') %>% 
  select(customer_id, quantity, category_id, order_date, model_year) %>% 
  mutate(order_date = as.Date(order_date), year = as.integer(format(order_date, '%Y')), new_bike = case_when(
    model_year == year ~ quantity,
    TRUE ~ 0
  ), category_id = case_when(
    category_id == 1 ~ 'cat1',
    category_id == 2 ~ 'cat2',
    category_id == 3 ~ 'cat3',
    category_id == 4 ~ 'cat4',
    category_id == 5 ~ 'cat5',
    category_id == 6 ~ 'cat6',
    category_id == 7 ~ 'cat7',
    TRUE ~ 'nie ma'
  )) %>% 
  group_by(customer_id, category_id) %>% 
  summarise(number_in_category = sum(quantity), new_bike = sum(new_bike), .groups = 'drop') %>% 
  pivot_wider(names_from = category_id, 
              values_from = number_in_category, 
              values_fill = list(number_in_category = 0)) %>%
  group_by(customer_id) %>% 
  summarise(new_bike = sum(new_bike), cat1 = sum(cat1), cat2 = sum(cat2), cat3 = sum(cat3), cat4 = sum(cat4), cat5 = sum(cat5), cat6 = sum(cat6), cat7 = sum(cat7))




###################################################################################################################


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

  

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- 
  df_order_items %>% 
  left_join(df_orders, by = 'order_id') %>% 
  select(product_id, quantity, list_price, discount, order_date) %>% 
  mutate(wday = wday(order_date, label = TRUE, abbr = FALSE), discount = discount * 100) %>% 
  group_by(wday, product_id) %>% 
  summarise(average_discount = mean(discount)) %>% 
  pivot_wider(names_from = product_id, values_from = average_discount)  






###################################################################################################################



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "WieckowskaKamila.rds")
