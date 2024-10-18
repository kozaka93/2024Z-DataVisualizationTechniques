library(dplyr)
library(tidyr)
library(stringr)

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

df_orders$order_date <- as.Date(df_orders$order_date)
df_orders$quarter <- quarters(df_orders$order_date)

A <- merge(df_orders, df_order_items, by = 'order_id')
A <- A[, c(1,2,9,11,12)]

B <- merge(A, df_customers, by = 'customer_id')
B <- B[, c(2,3,4,5,12)]

B <- B %>% 
  group_by(state, quarter, product_id ) %>% 
  summarise(total_count = sum(quantity)) %>% 
  arrange(state, quarter, desc(total_count)) %>% 
  group_by(state, quarter) %>% 
  slice(1)

C <- merge(B, df_products, by = 'product_id')
C <- C[, c(2,3,5,8)]
C <- C %>% 
  arrange(state, quarter)


## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- C


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

A <- df_orders %>% 
  filter(shipped_date=='NULL') %>% 
  mutate(month = format(as.Date(order_date), '%m')) %>% 
  select(month, shipped_date) %>% 
  group_by(month) %>% 
  table()
A <- data.frame(A)

B <- df_orders %>% 
  mutate(month = format(as.Date(order_date), '%m')) %>% 
  select(month, order_id) %>% 
  count(month) %>% 
  left_join(A, by = 'month')

C <- B %>% 
  select(month, Freq, n) %>% 
  transmute(month = month, percentage = round(Freq/n*100, 2))

C[1,2] <- 0.00


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- C


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

A <- df_order_items %>% 
  mutate(price = round(list_price - list_price*discount, 2)) %>% 
  mutate(final_price = quantity*price) %>% 
  left_join(df_orders, by = 'order_id') %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>% 
  select(product_id, final_price, year) %>% 
  arrange(year, product_id, desc(final_price)) %>% 
  group_by(year, product_id) %>% 
  summarise(sales = sum(final_price)) %>% 
  slice_max(order_by = sales, n = 1, with_ties = TRUE) %>% 
  left_join(df_products, by = 'product_id') %>% 
  select(year, product_name)


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- A


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 
A <- df_orders %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>% 
  select(year, customer_id, order_id) %>% 
  count(year, customer_id, name = "total_orders") %>% 
  group_by(year) %>% 
  slice_max(order_by = total_orders, n = 1, with_ties = TRUE) %>% 
  count(year, total_orders, name = "how_many")
 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- A


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

A <- df_orders %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>% 
  mutate(domain = str_extract(email, "(?<=@)[^ ]+")) %>% 
  select(year, order_id, domain) %>% 
  group_by(year) %>% 
  count(year, domain, name = 'order_count') %>% 
  slice_max(order_by = order_count, n = 1)
    

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- A


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

A <- df_orders %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  select(customer_id, state) %>% 
  filter(state =='TX' | state == 'CA') %>% 
  distinct() %>% 
  group_by(state) %>% 
  count(state, name = 'number')

B <- df_orders %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  filter(state =='TX' | state == 'CA') %>% 
  mutate(year = format(as.Date(order_date), "%Y")) %>% 
  select(customer_id, order_id, state, year) %>%
  group_by(customer_id, state) %>% 
  summarise(zamowienie_2018 = any(year == 2018)) %>% 
  filter(!zamowienie_2018) %>% 
  group_by(state) %>% 
  count(state, name = 'not_2018') %>% 
  left_join(A, by = 'state')
  
    
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- B


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

A <- df_order_items %>% 
  mutate(price = round(list_price - list_price*discount, 2)) %>% 
  mutate(final_price = quantity*price) %>% 
  group_by(order_id) %>% 
  select(order_id, final_price) %>% 
  summarise(cena = sum(final_price)) %>% 
  left_join(df_orders, by = 'order_id') %>% 
  select('customer_id', cena) %>% 
  arrange(cena)

q5 <- quantile(A$cena, 0.05)
q95 <- quantile(A$cena, 0.95)

B <- A %>% 
  filter(cena<q5 | cena>q95) %>% 
  select(customer_id) %>% 
  unique() %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  select(customer_id, 'first_name', 'last_name')
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- B


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

A <- df_orders %>% 
  select(order_id, order_date) %>% 
  group_by(order_date) %>% 
  count(order_date, name = 'liczba_zamowien') %>% 
  mutate(year = format(as.Date(order_date), '%Y'))

A$quarter <- quarters(as.Date(A$order_date))

max_ <- A %>% 
  select(year, quarter, liczba_zamowien) %>% 
  group_by(year, quarter) %>% 
  slice_max(order_by = liczba_zamowien, n = 1) %>% 
  select(year, quarter, liczba_zamowien) %>% 
  distinct()

min_ <- A %>% 
  select(year, quarter, liczba_zamowien) %>% 
  group_by(year, quarter) %>% 
  slice_min(order_by = liczba_zamowien, n = 1) %>% 
  select(year, quarter, liczba_zamowien) %>% 
  distinct()

median_ <- A %>% 
  select(year, quarter, liczba_zamowien) %>% 
  group_by(year, quarter) %>% 
  summarise(mediana = median(liczba_zamowien))

B <- max_ %>% 
  left_join(min_, by = c('year', 'quarter')) %>% 
  left_join(median_, by = c('year', 'quarter'))

colnames(B) <- c('year', 'quarter', 'max', 'min', 'mediana')


## Odpowiedz quarters()## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- B


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

A <- df_orders %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  select('year', 'order_date', 'shipped_date', 'state') %>% 
  filter(!(shipped_date=='NULL')) %>% 
  mutate(time =as.numeric( as.Date(shipped_date)-as.Date(order_date))) %>% 
  select(year, state, time) %>% 
  group_by(year, state) %>% 
  #summary(srednia = mean(time)) %>% 
  mutate(srednia = mean(time)) %>% 
  select(year, state, srednia) %>% 
  distinct()

B <- A %>% 
  pivot_wider(names_from = state, values_from = srednia)


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- B


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

#zakladam, ze mam wybrac klientow ktorzy robili zamowienia CO ROKU

A <- df_orders %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>% 
  select(year, customer_id) %>% 
  group_by(customer_id) %>% 
  summarise(how_many_years = n_distinct(year)) %>% 
  filter(how_many_years==3) %>% 
  left_join(df_customers) %>% 
  select(last_name) %>% 
  mutate(first_letter = substr(last_name,1,1)) %>% 
  group_by(first_letter) %>% 
  count(first_letter, name = 'frequency')
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- A


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

A <- df_products %>% 
  left_join(df_order_items, by = 'product_id') %>% 
  left_join(df_orders, by = 'order_id') %>% 
  select(product_name, order_id, quantity, customer_id) %>% 
  group_by(customer_id, product_name) %>% 
  summarise(sum(quantity)) %>% 
  pivot_wider(names_from = product_name, values_from = 'sum(quantity)', values_fill = 0)

B <- df_products %>% 
  left_join(df_order_items, by = 'product_id') %>% 
  left_join(df_orders, by = 'order_id') %>% 
  mutate(year = format(as.Date(order_date), '%Y')) %>% 
  select(product_id, model_year, year, customer_id) %>% 
  mutate(same_year = if_else(model_year == year, 1, 0)) %>% 
  group_by(customer_id) %>% 
  summarise(sum(same_year)) %>% 
  left_join(A, by = 'customer_id')


## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- B


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

A <- df_order_items %>% 
  left_join(df_orders, by = 'order_id') %>% 
  select(order_id, product_id, discount, order_date) %>% 
  mutate(znizka = discount*100) %>% 
  mutate(dzien_tyg = weekdays(as.Date(order_date))) %>% 
  group_by(product_id, dzien_tyg) %>% 
  summarise(srednia_znizka = mean(znizka)) %>% 
  arrange(product_id)

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- A


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "HallakElissa.rds")
