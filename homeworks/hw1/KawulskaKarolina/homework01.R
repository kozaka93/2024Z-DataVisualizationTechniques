library(dplyr)
library(tidyr)


df_orders <- read.csv('C:/Users/Karo/Desktop/iad/orders.csv')
df_order_items <- read.csv('C:/Users/Karo/Desktop/iad/order_items.csv')
df_products <- read.csv('C:/Users/Karo/Desktop/iad/products.csv')
df_brands <- read.csv('C:/Users/Karo/Desktop/iad/brands.csv')
df_categories <-  read.csv('C:/Users/Karo/Desktop/iad/categories.csv')
df_customers <- read.csv('C:/Users/Karo/Desktop/iad/customers.csv')
df_staffs <- read.csv('C:/Users/Karo/Desktop/iad/staffs.csv')
df_stocks <- read.csv('C:/Users/Karo/Desktop/iad/stocks.csv')
df_stores <- read.csv('C:/Users/Karo/Desktop/iad/stores.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

tmp <- df_orders %>%
  mutate(month = substr(order_date,6,7),
         quarter = case_when(
           month %in% c("01","02","03") ~ "1",
           month %in% c("04","05","06") ~ "2",
           month %in% c("07","08","09") ~ "3",
           month %in% c("10","11","12") ~ "4"
         )) %>%
  inner_join(df_order_items,by = "order_id") %>%
  select(order_id,customer_id,quarter,product_id)
tmp <- tmp %>%
  inner_join(df_customers,by = "customer_id") %>%
  select(order_id,quarter,product_id,state) %>%
  group_by(state,product_id,quarter) %>%
  summarise(count = n(),.groups = "drop_last") %>%
  arrange(quarter,state,-count) %>%
  group_by(quarter,state) %>%
  slice_max(count, n = 1)
final01 <- tmp %>%
  inner_join(df_products,by ="product_id") %>%
  select(quarter,state,product_name,model_year)
 
## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- final01


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

tmp <- df_orders %>%
  mutate(month = substr(order_date,6,7),
         quarter = case_when(
           month %in% c("01","02","03") ~ "1",
           month %in% c("04","05","06") ~ "2",
           month %in% c("07","08","09") ~ "3",
           month %in% c("10","11","12") ~ "4"
         )) %>%
  select(shipped_date,month) %>%
  group_by(month) %>%
  summarise(null_count = sum(shipped_date == "NULL"),.groups = "drop_last")
final02 <- df_orders %>%
  mutate(month = substr(order_date,6,7),
         quarter = case_when(
           month %in% c("01","02","03") ~ "1",
           month %in% c("04","05","06") ~ "2",
           month %in% c("07","08","09") ~ "3",
           month %in% c("10","11","12") ~ "4"
         )) %>%
  select(shipped_date,month) %>%
  group_by(month) %>%
  summarise(count = n(),.groups = "drop_last")
final02 <- final02 %>%
  inner_join(tmp,by = "month") %>%
  group_by(month) %>%
  summarise(unrealised_orders = (null_count/count)*100,.groups = "drop_last") 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- final02


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

tmp <- df_orders %>%
  mutate(year = substr(order_date,1,4)) %>%
  inner_join(df_order_items,by = "order_id") %>%
  mutate(final_price = quantity*list_price*(1 - discount)) %>%
  select(year,product_id,final_price) %>%
  group_by(year,product_id) %>%
  summarise(income = sum(final_price),.groups = "drop_last") %>%
  group_by(year) %>%
  slice_max(income, n = 1)
final03 <- tmp %>%
  inner_join(df_products, by = "product_id") %>%
  select(year,product_name)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- final03


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

final04 <- df_orders %>%
  mutate(year = substr(order_date,1,4)) %>%
  inner_join(df_customers, by = "customer_id") %>%
  select(customer_id,year) %>%
  group_by(customer_id,year) %>%
  summarise(count = n(),.groups = "drop_last") %>%
  group_by(year) %>%
  slice_max(count, n = 1) %>%
  group_by(year,count) %>%
  summarise(top_customers_count = n(),.groups = "drop_last")

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- final04


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

final05 <- df_customers %>%
  mutate(domain = sub(".*@","",email)) %>%
  inner_join(df_orders, by = "customer_id") %>%
  mutate(year = substr(order_date,1,4)) %>%
  group_by(year,domain) %>%
  summarise(orders = n(), .groups = "drop_last") %>%
  group_by(year) %>%
  slice_max(orders,n = 1)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- final05


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

tmp <- df_customers %>%
  inner_join(df_orders,by = "customer_id") %>%
  filter(state == "CA" | state == "TX") %>%
  select(customer_id,state) %>%
  distinct() %>%
  group_by(state) %>%
  summarise(count = n(),.groups = "drop_last")
final06 <- df_customers %>%
  inner_join(df_orders,by = "customer_id") %>%
  mutate(year = substr(order_date,1,4)) %>%
  filter(state == "CA" | state == "TX") %>%
  select(customer_id,year,state) %>%
  mutate(orders_2018 = case_when(
    year == 2018 ~ 1,
    year != 2018 ~ 0)) %>%
  group_by(customer_id) %>%
  mutate(orders = n()) %>%
  group_by(customer_id) %>%
  mutate(orders_2018 = sum(orders_2018)) %>%
  filter(orders_2018/orders == 0) %>%
  distinct() %>%
  group_by(state) %>%
  summarise(count_non_2018 = n(),.groups = "drop_last")
final06 <- final06 %>%
  inner_join(tmp,by = "state")

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- final06


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
  
final07 <- df_order_items %>%
  mutate(price = quantity*list_price*(1-discount)) %>%
  inner_join(df_orders,by = "order_id")
quantile08 <- quantile(final07$price, probs = c(0.05,0.95))
final07 <- final07 %>%
  inner_join(df_customers,by = "customer_id") %>%
  filter(price < quantile08[1] | price > quantile08[2]) %>%
  select(first_name,last_name,price)

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- final07


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

# Przez zamowienia zlozone kazdego dnia w poszczegolnych kwartalach rozumiem, ze nie rozniozniamy miesiecy 
# (tzn. 1 stycznia, 1 lutego, 1 marca są w tej samej kategorii)

final08 <- df_orders %>%
  mutate(day = substr(order_date,9,10)) %>%
  mutate(month = substr(order_date,6,7),
         quarter = case_when(
           month %in% c("01","02","03") ~ "1",
           month %in% c("04","05","06") ~ "2",
           month %in% c("07","08","09") ~ "3",
           month %in% c("10","11","12") ~ "4"
         )) %>%
  group_by(quarter,day) %>%
  summarise(orders = n_distinct(order_id),.groups = "drop_last") %>%
  group_by(quarter) %>%
  summarise(min = min(orders),max = max(orders),median = median(orders),.groups = "drop_last")

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- final08


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

final09 <- df_orders %>%
  mutate(year = substr(order_date,1,4)) %>%
  mutate(order_date = as.Date(order_date),
         shipped_date = as.Date(shipped_date),
         delivery_time = as.numeric(shipped_date - order_date)) %>%
  inner_join(df_customers,by = "customer_id") %>%
  select(year,state,delivery_time) %>%
  filter(!is.na(delivery_time)) %>%
  group_by(year,state) %>%
  summarise(mean_delivery_time = mean(delivery_time),.groups = "drop_last")
final09 <- final09 %>%
  pivot_wider(
    names_from = state,
    values_from = mean_delivery_time,
  )

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- final09


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

final10 <- df_customers %>%
  mutate(letter = substr(last_name,1,1)) %>%
  inner_join(df_orders,by = "customer_id") %>%
  mutate(year = substr(order_date,1,4)) %>%
  group_by(customer_id,letter) %>%
  filter(year %in% c(2016,2017,2018)) %>%
  summarise(years_of_order = n_distinct(year),.groups = "drop_last") %>%
  filter(years_of_order == 3) %>%
  select(customer_id,letter) %>%
  group_by(letter) %>%
  summarise(count = n(),.groups = "drop_last")

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- final10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

tmp <- df_customers %>%
  inner_join(df_orders, by = "customer_id") %>%
  inner_join(df_order_items, by = "order_id") %>%
  inner_join(df_products, by = "product_id") %>%
  inner_join(df_categories, by = "category_id") %>%
  select(customer_id, category_name,quantity) %>%
  group_by(customer_id,category_name) %>%
  mutate(quantity = sum(quantity)) %>%
  unique()
final11 <- tmp %>%
  pivot_wider(
    names_from = category_name,
    values_from = quantity,
    values_fill = list(quantity = 0)
  )
tmp1 <- df_customers %>%
  inner_join(df_orders, by = "customer_id") %>%
  inner_join(df_order_items, by = "order_id") %>%
  inner_join(df_products, by = "product_id") %>%
  inner_join(df_categories, by = "category_id") %>%
  mutate(year = substr(order_date,1,4)) %>%
  select(customer_id, category_name,quantity,year,model_year) %>%
  mutate(new_product = case_when(
    year == model_year ~ 1,
    year != model_year ~ 0
  )) %>%
  group_by(customer_id) %>%
  summarise(new_product = sum(new_product))
final11 <- final11 %>%
  inner_join(tmp1, by = "customer_id")

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- final11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

final12 <- df_orders %>%
  mutate(order_date = as.Date(order_date),
         weekday = format(order_date, "%A")) %>%
  inner_join(df_order_items,by = "order_id") %>%
  inner_join(df_products,by = "product_id") %>%
  group_by(weekday,product_id,product_name) %>%
  summarise(mean_discount = weighted.mean(discount*100,quantity),.groups = "drop_last") 

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- final12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "KawulskaKarolina.rds")
