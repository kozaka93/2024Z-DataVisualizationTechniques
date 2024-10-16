library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')


df_orders <- read.csv('/Users/malgorzataluckner/Documents/TWD/Homeworks/HW1/orders.csv')
df_order_items <- read.csv('/Users/malgorzataluckner/Documents/TWD/Homeworks/HW1/order_items.csv')
df_products <- read.csv('/Users/malgorzataluckner/Documents/TWD/Homeworks/HW1/products.csv')
df_brands <- read.csv('/Users/malgorzataluckner/Documents/TWD/Homeworks/HW1/brands.csv')
df_categories <- read.csv('/Users/malgorzataluckner/Documents/TWD/Homeworks/HW1/categories.csv')
df_customers <- read.csv('/Users/malgorzataluckner/Documents/TWD/Homeworks/HW1/customers.csv')
df_staffs <- read.csv('/Users/malgorzataluckner/Documents/TWD/Homeworks/HW1/staffs.csv')
df_stocks <- read.csv('/Users/malgorzataluckner/Documents/TWD/Homeworks/HW1/stocks.csv')
df_stores <- read.csv('/Users/malgorzataluckner/Documents/TWD/Homeworks/HW1/stores.csv')



####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

df_orders%>%
  inner_join(df_customers, by = "customer_id")%>%
  inner_join(df_order_items, by = 'order_id')%>%
  select(order_id, order_date, customer_id, state, product_id, quantity) -> tpdf_1

tpdf_1%>%
  mutate(Year = substr(order_date, 1, 4))%>%
  mutate(Quarter = ceiling(as.integer(substr(order_date, 6, 7)) / 4))%>%
  select(Year, Quarter, state, product_id, quantity)%>%
  group_by(Year, Quarter, state, product_id)%>%
  summarise(products_bought = sum(quantity))-> tpdf_2

tpdf_2%>%
  slice_max(order_by = products_bought, n = 1, with_ties = FALSE)%>%
  inner_join(df_products, by = 'product_id')%>%
  select(Year, Quarter, state, product_name, model_year)-> zad_1

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- zad_1


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

df_orders%>%
  mutate(Year = substr(order_date, 1, 4))%>%
  mutate(Month = substr(order_date, 6, 7))%>%
  select(Year, Month, order_id, order_date, shipped_date)%>%
  group_by(Year, Month)%>%
  summarise(Orders = n(), Successful = sum(!shipped_date == "NULL")) -> tpdf_1

tpdf_1%>%
  mutate(successful_percentage = (1 - Successful/Orders)*100)%>%
  select(Year, Month, failed_percentage) -> tpdf_2

zad_2 <- tpdf_2
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- zad_2


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
df_orders%>%
  mutate(Year = substr(order_date, 1, 4))%>%
  inner_join(df_order_items, by = 'order_id')%>%
  mutate(income = quantity * list_price*(1 - discount))%>%
  select(Year, product_id, income)%>%
  group_by(Year, product_id)%>%
  summarise(total_income = sum(income)) -> tpdf_1

tpdf_1%>%
  slice_max(order_by = total_income, n = 1, with_ties = FALSE)%>%
  inner_join(df_products, by = 'product_id')%>%
  select(Year, product_name) -> zad_3
## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- zad_3


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

df_orders%>%
  mutate(Year = substr(order_date, 1, 4))%>%
  group_by(Year, customer_id)%>%
  summarise(n_orders = n())%>%
  slice_max(order_by = n_orders, n = 1, with_ties = TRUE) -> tpdf_1

tpdf_1%>%
  group_by(Year, n_orders)%>%
  summarise(n_customers = n())%>%
  select(Year, n_customers, n_orders)-> zad_4
  


## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- zad_4


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

df_orders%>%
  mutate(Year = substr(order_date, 1, 4))%>%
  inner_join(df_stores, by = 'store_id')%>%
  group_by(Year, email)%>%
  summarise(n_orders = n())%>%
  slice_max(order_by = n_orders, n = 1, with_ties = TRUE)-> zad_5

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- zad_5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

df_orders%>%
  inner_join(df_stores, by = 'store_id')%>%
  filter(state != 'NY')%>%
  summarise(unique_customers = n_distinct(customer_id)) -> zad_6p1

df_customers%>%
  inner_join(df_orders, by = 'customer_id')%>%
  mutate(Year = substr(order_date, 1, 4))%>%
  filter(state == 'TX' | state == 'CA')%>%
  filter(Year == '2018')%>%
  select(customer_id)%>%
  group_by(customer_id)-> tpdf_1

df_orders%>%
  inner_join(df_customers, by = 'customer_id')%>%
  mutate(Year = substr(order_date, 1, 4))%>%
  filter(state == 'TX' | state == 'CA')%>%
  select(customer_id)%>%
  anti_join(tpdf_1, by = 'customer_id')%>%
  summarise(no_orders_2018 = n_distinct(customer_id))-> tpdf_2

zad_6 <- bind_cols(v1 = tpdf_2, v2 = zad_6p1)


## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- zad_6


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

df_orders%>%
  inner_join(df_order_items, by = 'order_id')%>%
  select(customer_id, order_id, quantity, list_price, discount)%>%
  group_by(customer_id, order_id)%>%
  summarise(order_price = sum(quantity * list_price * (1- discount))) -> tpdf_1

tpdf_1%>%
  filter(order_price < quantile(order_price, probs = c(0.05))[[1]])-> df_1

df_1 <- tpdf_1[tpdf_1$order_price<quantile(tpdf_1$order_price,c(0.05)), c('customer_id', 'order_price')]
df_2 <- tpdf_1[tpdf_1$order_price>quantile(tpdf_1$order_price,c(0.95)), c('customer_id', 'order_price')]

colnames(df_1) <- c("worst_customer_id", "order_price_wc")
colnames(df_2) <- c("best_customer_id", "order_price_bc")


zad_7 <- data_frame(df_1, df_2)

  
## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- zad_7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

dates <- seq(from = as.Date("2016-01-01"), to = as.Date("2018-12-31"), by = "day")

df <- data.frame(date = as.character(dates))


df%>%
  full_join(df_orders, by = c('date' = 'order_date'))%>%
  mutate(Year = substr(date, 1, 4))%>%
  mutate(Quarter = ceiling(as.integer(substr(date, 6, 7)) / 3))%>%
  mutate(Month = substr(date, 6, 7))%>%
  mutate(Day  = substr(date, 9, 10))%>%
  group_by(Year, Quarter, Month, Day)%>%
  summarise(orders_day = sum(!is.na(order_id)), .groups = 'drop')-> tpdf_1

tpdf_1%>%
  group_by(Year, Quarter)%>%
  summarise(max_orders = max(orders_day), min_orders = min(orders_day), median_orders = median(orders_day))-> zad_8


## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- zad_8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

day_diff <- (as.Date(df_orders$shipped_date) - as.Date(df_orders$order_date))
tpdf_1 <- data.frame(df_orders, day_diff)


tpdf_1%>%
  inner_join(df_customers, by = 'customer_id')%>%
  mutate(Year = substr(order_date, 1, 4))%>%
  select(Year, order_id, state, day_diff)%>%
  filter(!is.na(day_diff))%>%
  group_by(Year, state)%>%
  summarise(avg_del = mean(day_diff)) -> tpdf_2

tpdf_2%>%
  pivot_wider(names_from = state, values_from = avg_del) -> zad_9
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- zad_9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

df_orders%>%
  mutate(Year = substr(order_date, 1, 4))%>%
  group_by(customer_id, Year)%>%
  summarise(orders_year = n()) -> tpdf_1

tpdf_1%>%
  group_by(customer_id)%>%
  summarise(years_ordering = n())%>%
  filter(years_ordering == 3)%>%
  inner_join(df_customers, by = 'customer_id')%>%
  mutate(first_letter = substr(last_name, 1, 1))%>%
  select(customer_id, first_letter)-> tpdf_2

tpdf_2%>%
  group_by(first_letter)%>%
  summarise(occurences = n()) -> zad_10

  
## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- zad_10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

df_orders%>%
  inner_join(df_customers, by = 'customer_id')%>%
  inner_join(df_order_items, by = 'order_id')%>%
  inner_join(df_products, by = 'product_id')%>%
  inner_join(df_categories, by = 'category_id')%>%
  select(customer_id, order_id, category_name, model_year, order_date, quantity)%>%
  mutate(Year = substr(order_date, 1, 4))%>%
  group_by(customer_id, category_name) -> tpdf_1
  
tpdf_1%>%
  group_by(customer_id)%>%
  summarise(new_products = sum(ifelse(Year == model_year, quantity, 0))) -> tpdf_2

tpdf_1%>%
  summarise(products_bought = sum(quantity))%>%
  pivot_wider(names_from = category_name, values_from = products_bought)%>%
  mutate_all(~replace_na(.,0))%>%
  inner_join(tpdf_2, by = 'customer_id')-> zad_11


## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- zad_11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

df_orders%>%
  mutate(Weekday = weekdays(as.Date(order_date)))%>%
  inner_join(df_order_items, by = 'order_id')%>%
  inner_join(df_products, by = 'product_id')%>%
  select(Weekday, order_id, discount, product_name)%>%
  group_by(product_name, Weekday)%>%
  summarise(avg_discount = mean(discount))%>%
  pivot_wider(names_from = Weekday, values_from = avg_discount)%>%
  mutate_all(~replace_na(.,0))%>%
  select(product_name, Monday, Tuesday, Wednesday, Thursday, 
         Friday, Saturday, Sunday)-> zad_12

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- zad_12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "LucknerAleksander.rds")
