library(dplyr)
library(tidyr)


df_orders <- read.csv('C:/Users/alapr/Desktop/Techniki wizualizacji danych/homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('C:/Users/alapr/Desktop/Techniki wizualizacji danych/homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('C:/Users/alapr/Desktop/Techniki wizualizacji danych/homeworks/hw1/dane/products.csv')
df_brands <- read.csv('C:/Users/alapr/Desktop/Techniki wizualizacji danych/homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('C:/Users/alapr/Desktop/Techniki wizualizacji danych/homeworks/hw1/dane/categories.csv')
df_stores <- read.csv('C:/Users/alapr/Desktop/Techniki wizualizacji danych/homeworks/hw1/dane/stores.csv')
df_stocks <- read.csv('C:/Users/alapr/Desktop/Techniki wizualizacji danych/homeworks/hw1/dane/stocks.csv')
df_staffs <-read.csv('C:/Users/alapr/Desktop/Techniki wizualizacji danych/homeworks/hw1/dane/staffs.csv')
df_customers <- read.csv('C:/Users/alapr/Desktop/Techniki wizualizacji danych/homeworks/hw1/dane/customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

#rozwiązanie zakładające, że np. styczeń 2016 roku i styczeń 2017 roku nie należa do tego samego kwartału; docelowa odpowiedź zawiera 12 różnych kwartałów

#wybranie potrzebnych kolumn
customers_1 <- df_customers %>% select('customer_id', 'state')
orders_1 <- df_orders %>% select('order_id', 'order_date', 'customer_id')
order_items_1 <- df_order_items %>% select('order_id', 'product_id')
products_1 <- df_products %>% select('product_id', 'product_name', 'model_year')

#połączenie w ramkę o nazwie n
order_items_1 %>% left_join(orders_1, by='order_id') %>% left_join(customers_1, by='customer_id') -> n 
n$month <- as.integer(substring(n$order_date, 6, 7))
n$year<- as.integer(substring(n$order_date, 1, 4))

mapping <- data.frame(month = 1:12 , quarter = rep(1:4, each  = 3))

# order_items_2016 <- n %>% filter(year == 2016)
# order_items_2017 <- n %>% filter(year ==2017)
# order_items_2018 <- n %>% filter(year ==2018)
# 
# left_join(order_items_2016, mapping, by = 'month') -> order_items_2016
# left_join(order_items_2017, mapping, by = 'month') -> order_items_2017
# left_join(order_items_2018, mapping, by = 'month') -> order_items_2018
left_join(n, mapping, by = 'month') -> order_items_check

#2016
# order_items_2016 %>% select(year, product_id, state, quarter) %>%
#   group_by(state, quarter, product_id) %>%
#   mutate(amount = n()) %>%  
#   distinct()-> order_2016
# 
# order_2016 %>% arrange(quarter, desc(amount)) %>% 
#   group_by(quarter, state) %>% 
#   filter(amount == max(amount))-> order_2016
# 
# order_2016 %>% left_join(products_1, by = 'product_id') %>% 
#   select('year', 'quarter', 'product_name', 'state', 'model_year') -> order_2016
#2017
# order_items_2017 %>% select(year, product_id, state, quarter) %>%
#   group_by(state, quarter, product_id) %>%
#   mutate(amount = n()) %>%  
#   distinct()-> order_2017
# 
# order_2017 %>% arrange(quarter, desc(amount)) %>% 
#   group_by(quarter, state) %>% 
#   filter(amount == max(amount))-> order_2017
# 
# order_2017 %>% left_join(products_1, by = 'product_id') %>% 
#   select('year', 'quarter', 'product_name', 'state', 'model_year') -> order_2017
#2018
# order_items_2018 %>% select(year, product_id, state, quarter) %>%
#   group_by(state, quarter, product_id) %>%
#   mutate(amount = n()) %>%  
#   distinct()-> order_2018
# 
# order_2018 %>% arrange(quarter, desc(amount)) %>% 
#   group_by(quarter, state) %>% 
#   filter(amount == max(amount))-> order_2018
# 
# order_2018 %>% left_join(products_1, by = 'product_id') %>% 
#   select('year', 'quarter', 'product_name', 'state', 'model_year') -> order_2018

order_items_check %>% select(year, product_id, state, quarter) %>%
  group_by(state, quarter, year, product_id) %>%
  mutate(amount = n()) %>%  
  distinct()-> order_check

order_check %>% arrange(year, quarter, desc(amount)) %>% 
  group_by(year, quarter, state) %>% 
  filter(amount == max(amount))-> order_check

order_check %>% left_join(products_1, by = 'product_id') %>% 
  select('year', 'quarter', 'product_name', 'state', 'model_year') -> r1
## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- r1


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

orders_2 <- df_orders %>% select('order_status', 'order_date', 'order_id')
n$month <- as.integer(substring(n$order_date, 6, 7))
orders_2 %>% mutate(month =as.integer(substring(orders_2$order_date, 6, 7)), year = as.integer(substring(orders_2$order_date, 1, 4)) ) ->orders_2
orders_2 %>% group_by(year, month) %>% mutate(sum_of_month = n()) -> all_orders
orders_2 %>% filter(order_status== 3) %>% group_by(year, month) %>% mutate(sum_of_month_rejected = n()) -> rejected_orders

all_orders %>% select(year, month, sum_of_month) %>% distinct() -> all_orders
rejected_orders %>% select(year, month, sum_of_month_rejected) %>% distinct() -> rejected_orders
left_join(all_orders, rejected_orders, by =c("year", "month")) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  mutate(percent_rej = (sum_of_month_rejected/sum_of_month)*100) %>% 
  select(year, month, percent_rej) -> r2
## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- r2

####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
orders_3 <- df_orders %>% select(order_status, order_date, order_id)
order_items_3 <- left_join(df_order_items, orders_3, by = 'order_id')
order_items_3 %>% mutate(year = as.integer(substring(order_items_3$order_date, 1, 4)), finall_price = round((order_items_3$list_price*order_items_3$quantity)*(1-order_items_3$discount), digits = 2)) %>% 
  select(product_id, order_status, year, finall_price)-> order_items_3
order_items_3 %>% filter(order_status==4) %>% 
  group_by(year, product_id) %>% 
  mutate(sum = sum(finall_price)) %>% 
  arrange(year, desc(sum)) %>% 
  select(product_id, year, sum) %>% 
  distinct()-> order_items_3
order_items_3 %>%  group_by(year) %>% filter(sum==max(sum)) -> order_items_3
left_join(order_items_3, df_products[,c('product_id','product_name')], by='product_id') %>%
  select(year, product_name)-> r3
## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- r3


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 
df_orders %>% select(customer_id, order_date, order_id) -> customers_3
customers_3 %>% mutate(year = as.integer(substring(customers_3$order_date, 1, 4))) %>% 
  group_by(year, customer_id) %>% select(year, customer_id, order_id) %>% 
  group_by(year, customer_id) %>% mutate(sum_orders = n_distinct(order_id)) -> customers_3
customers_3 %>% group_by(year) %>% mutate(max_order= max(sum_orders)) -> customers_3

customers_3 %>% filter(sum_orders==max_order) %>% select(year, customer_id, max_order) %>% unique()-> customers_3
customers_3 %>% group_by(year) %>% mutate(number_of_customers = n_distinct(customer_id)) %>% 
  select(year,number_of_customers, max_order) %>% distinct() -> r4
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- r4

####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
library(stringr)
left_join(df_customers[c("customer_id", 'email')], df_orders[c("customer_id", "order_date", "order_id")], by = "customer_id") -> mail
mail %>% mutate(domain = str_extract(email, "@[a-zA-Z0-9.-]+."), year = as.integer(substring(order_date, 1, 4)) ) %>%
  group_by(year, domain) %>% mutate(number_orders = n_distinct(order_id)) %>% 
  select(year, domain, number_orders) %>% distinct() %>% 
  group_by(year) %>% filter(number_orders==max(number_orders)) -> r5
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- r5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?
#aktywni klienci w każdym stanie w ciągu 3lat łącznie
left_join(df_orders[c("customer_id", "order_date", "order_id")], df_customers[c("customer_id", 'state')], by ="customer_id") -> customers_6
customers_6 %>% filter(state == "CA" | state == "TX") %>% arrange(customer_id) %>%
  group_by(state) %>% mutate(number_activ = n_distinct(customer_id)) %>%
  select(state, number_activ) %>% distinct() -> active
#klienci z tych stanów, którzy nie zrobili zakupów w 2018 roku z podziałem na stany
df_customers[c("customer_id", 'state')] %>% filter(state == "CA" | state == "TX") -> customers_state
  customers_6 %>% mutate(year =as.integer(substring(order_date, 1, 4)) ) %>% 
    filter(state == "CA" | state == "TX") %>% filter(year == 2018) %>% 
    right_join(customers_state, by = c('customer_id', 'state')) %>% 
    filter(is.na(order_date)) %>% group_by(state) %>% 
    mutate(no_orders_2018 = n_distinct(customer_id)) %>% 
    select(state, no_orders_2018) %>% distinct() %>% inner_join(active, by ='state') -> r6
    
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- r6


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
left_join(df_order_items[c('order_id', 'product_id', 'quantity', 'list_price', 'discount')], df_orders[c('order_id', 'customer_id')], by = 'order_id') -> w
w %>% mutate(part_price = round((list_price*quantity)*(1-discount), digits = 2)) %>% 
  group_by(order_id, customer_id) %>%
  mutate(finall_price = sum(part_price)) %>% select(customer_id, order_id, finall_price) %>% distinct()-> w
  unname(quantile(w$finall_price, probs = c(0.05, 0.95))) -> val
  w %>% filter(finall_price < val[1] | finall_price > val[2]) %>% arrange(customer_id) %>%
    left_join(df_customers[c('customer_id', 'first_name', 'last_name')], by = 'customer_id')-> a
  a[c('first_name', 'last_name', 'customer_id')] %>% distinct() -> r7
## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- r7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
mapping <- data.frame(month = 1:12 , quarter = rep(1:4, each  = 3))
df_orders %>% select(order_id, order_date) %>% group_by(order_date) %>%
  mutate(sum_dayly = n_distinct(order_id), year = as.integer(substring(order_date, 1, 4)), month = as.integer(substring(order_date, 6, 7))) %>% 
  left_join(mapping, by = 'month') %>% group_by(year, quarter) %>% 
  mutate(max = max(sum_dayly), min= min(sum_dayly), median = median(sum_dayly)) %>%
  select(year, quarter, max, min, median) %>% distinct() -> r8
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- r8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
left_join(df_orders[c('order_id', 'customer_id', 'order_date', 'shipped_date', 'order_status')], df_customers[c('customer_id', 'state')], by = 'customer_id') %>% filter(order_status == 4)-> z
z %>% mutate(year_order = as.integer(substring(order_date, 1, 4)), month_order = as.integer(substring(order_date, 6, 7)), day_order = as.integer(substring(order_date, 9, 10))) %>%
  mutate(year_shipped = as.integer(substring(shipped_date, 1, 4)), month_shipped = as.integer(substring(shipped_date, 6, 7)), day_shipped = as.integer(substring(shipped_date, 9, 10)))-> k
k %>% mutate(time = case_when(year_order==year_shipped & month_order==month_shipped ~ (day_shipped - day_order),
                              year_order==year_shipped & month_order==2 & month_shipped == month_order+1 & year_order%%4==0 ~ (29-day_order+day_shipped),
                              year_order==year_shipped & month_order==2 & month_shipped == month_order+1 & year_order%%4!=0 ~ (28-day_order+day_shipped),
                              year_order==year_shipped & ((month_order%%2 == 0 & month_order< 7) | (month_order%%2==1 & month_order > 7)) & month_order!= 2 & month_shipped==month_order+1 ~ (30-day_order+day_shipped),
                              year_order==year_shipped &((month_order%%2 == 1 & month_order<= 7) | (month_order%%2==0 & month_order > 7)) & month_shipped==month_order+1 ~ (31-day_order+day_shipped),
                              year_order != year_shipped ~(31-day_order+day_shipped),
                              TRUE~-1)) -> l
l %>% group_by(year_order, state) %>% mutate(mean_time = mean(time)) %>%
  select(year_order, state, mean_time) %>% distinct()-> l
spread(l, state, mean_time) -> r9
## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- r9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
library(stringr)
left_join(df_orders[c('order_id', 'customer_id', 'order_date')], df_customers[c('customer_id', 'last_name')], by='customer_id') -> r10
r10 %>% mutate(year=as.integer(substring(order_date, 1, 4)), first_letter = str_sub(last_name, 1, 1)) %>% 
  select(customer_id, year, first_letter) %>% distinct() %>% 
  group_by(customer_id) %>% 
  mutate(sum_of_years=sum(year)) %>% filter(sum_of_years==6051) %>% 
  select(first_letter, customer_id) %>% distinct() %>% group_by(first_letter) %>% 
  mutate(number_of_letters=n_distinct(customer_id)) %>% 
  select(first_letter, number_of_letters) %>% 
  distinct() -> r10
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- r10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)
left_join(df_order_items[c('order_id', 'product_id', 'quantity')], df_orders[c('order_id', 'customer_id', 'order_date')], by = 'order_id') -> r11
r11 %>% left_join(df_products[c('product_id', 'category_id', 'model_year')], by='product_id') %>% left_join(df_categories, by='category_id')-> r11
r11 %>% group_by(customer_id, category_id) %>% 
  mutate(bought_times=sum(quantity), year_bought = as.integer(substring(order_date, 1, 4))) %>% 
  group_by(customer_id) %>% mutate(newest = sum(ifelse(year_bought==model_year, 1, 0))) %>%
  select(customer_id, category_name, bought_times, newest) %>% distinct() -> r11
spread(r11, category_name, bought_times, fill = 0) -> r11
## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- r11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
left_join(df_order_items[c('order_id', 'product_id', 'quantity', 'list_price', 'discount')], df_orders[c('order_id', 'order_date')], by='order_id')-> r12
r12 %>% mutate(weekday = strftime(order_date, '%A'), real=round(quantity*list_price*(1-discount), 2), list = quantity*list_price) %>% 
  group_by(weekday, product_id) %>% mutate(sum_list_price=sum(list), sum_real_price=sum(real)) %>% 
  mutate(percent = 100*(sum_list_price - sum_real_price)/sum_list_price) %>%
  select(product_id, weekday, percent) %>% distinct() %>% 
  arrange(product_id) %>% spread(weekday, percent)-> r12
## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- r12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "AlicjaPrzezdziecka.rds")
