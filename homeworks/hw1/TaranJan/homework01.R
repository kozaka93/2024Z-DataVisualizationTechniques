library(dplyr)
library(tidyr)

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


df_ans_task_01 <- 
  df_orders %>% 
  mutate(year_q = strtoi(substr(order_date, 6,7), base = 10) %/% 4) %>% 
  select(order_id, customer_id, order_date, year_q) %>% 
  inner_join(df_customers, join_by(customer_id)) %>%
  select(order_id, customer_id, order_date, year_q, state) %>% 
  inner_join(df_order_items, join_by(order_id)) %>%
  inner_join(df_products, join_by(product_id)) %>%
  select(year_q, state, product_name, model_year, quantity) %>% 
  group_by(year_q, state, product_name) %>% 
  reframe(model_year, total_sold_each_quarter = sum(quantity)) %>% 
  group_by(year_q, state) %>% 
  arrange(desc(total_sold_each_quarter)) %>% 
  slice_max(order_by = total_sold_each_quarter, n=1, with_ties = FALSE) %>% 
  select(state, year_q, product_name, model_year) %>% 
  arrange(state)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_ans_task_01


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 
df_ans_task_02 <- 
  df_orders %>% 
  group_by(miesiac = substr(required_date, 6, 7)) %>% 
  summarise(procent_niezrealizowanych_zamowien = 100*sum(order_status!=4)/n())
  #summarise(procent_niezrealizowanych_zamowien = 100*sum(shipped_date == "NULL")/n())
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_ans_task_02


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
df_ans_task_03 <- 
  df_orders %>% 
  inner_join(df_order_items, join_by(order_id)) %>% 
  mutate(profit = quantity*list_price*(1-discount)) %>% 
  inner_join(df_products, join_by(product_id)) %>% 
  mutate(year = substr(order_date,1,4)) %>% 
  select(year, product_name, profit) %>% 
  group_by(year, product_name) %>% 
  summarise(yearly_profit = sum(profit)) %>% 
  arrange(desc(yearly_profit)) %>% 
  slice_max(order_by = yearly_profit, n=1, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(year, product_name)


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_ans_task_03


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 
df_ans_task_04 <-
  df_orders %>% 
  mutate(year = substr(order_date, 1,4)) %>% 
  group_by(year, customer_id) %>% 
  summarise(orders_count = n()) %>% 
  arrange(desc(orders_count)) %>% 
  slice_max(order_by = orders_count, n=1, with_ties = TRUE) %>% 
  group_by(year) %>% 
  reframe(number_of_clients_with_most_orders = n(), orders_count=first(orders_count))


## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_ans_task_04


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

df_ans_task_05 <-
  df_orders %>% 
  mutate(year = substr(order_date, 1,4)) %>% 
  inner_join(df_customers, join_by(customer_id)) %>% 
  mutate(domain = substring(email, regexpr("@", email)+1)) %>% 
  select(year, domain) %>% 
  group_by(year, domain) %>% 
  summarise(domain_count = n()) %>% 
  arrange(desc(domain_count)) %>% 
  slice_max(order_by = domain_count, n=1, with_ties = TRUE)

  
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_ans_task_05


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

df_ans_task_06 <- df_orders %>% 
  inner_join(df_customers, join_by(customer_id)) %>% 
  select(customer_id, state) %>% 
  filter(state == "CA" | state == "TX") %>% 
  distinct(customer_id) %>% 
  arrange(customer_id)

#nrow(df_ans_task_06)

df_ans_task_06_02 <- df_orders %>% 
  right_join(df_ans_task_06, join_by(customer_id)) %>% 
  filter(substr(order_date,1,4) == "2018") %>% 
  distinct(customer_id) %>% 
  arrange(customer_id)

setdiff(df_ans_task_06, df_ans_task_06_02)
nrow(df_ans_task_06_02)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(nrow(df_ans_task_06),"TAK - różnica w ilości wierszy pomiędzy df_ans_task_06 a df_ans_task_06_02")


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

df_ans_task_07 <-
  df_orders %>% 
  inner_join(df_order_items, join_by(order_id)) %>% 
  mutate(profit = quantity*list_price*(1-discount)) %>% 
  select(customer_id, profit) %>% 
  filter(profit <= quantile(profit, probs = 0.05)[[1]] | 
           profit >= quantile(profit, probs = 0.95)[[1]]) %>% 
  distinct(customer_id) %>% 
  inner_join(df_customers, join_by(customer_id)) %>% 
  select(first_name, last_name)


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_ans_task_07


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

days_vector <- as.data.frame(as.character(seq.Date(from=as.Date("2016-01-01"), to=as.Date("2018-12-31"), by="day")))
colnames(days_vector) <- c("days_date")

df_ans_task_08 <-
  df_orders %>% 
  right_join(days_vector, join_by(order_date == days_date)) %>% 
  group_by(order_date) %>% 
  summarise(count_orders = sum(!is.na(order_id))) %>% 
  mutate(year_q = strtoi(substr(order_date, 6,7), base = 10) %/% 4) %>% 
  group_by(year_q) %>% 
  summarise(max_orders = max(count_orders), min_orders=min(count_orders), median_orders = median(count_orders))

  

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_ans_task_08


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

df_ans_task_09 <-
  df_orders %>% 
  mutate(year = substr(order_date, 1, 4)) %>% 
  inner_join(df_customers, join_by(customer_id)) %>% 
  filter(order_status == 4) %>%
  mutate(order_time = as.integer(difftime(as.Date(shipped_date), as.Date(order_date), units = "days"))) %>% 
  select(year, state, order_time) %>%
  group_by(year, state) %>% 
  summarise(mean_order_time = mean(order_time)) %>% 
  pivot_wider(id_cols = year, names_from = state, values_from = mean_order_time)



## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_ans_task_09


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
  
total_years <- df_orders %>% 
  mutate(year=substr(order_date, 1, 4)) %>%
  summarise(n_year = n_distinct(year)) %>% 
  pull(n_year)
#length(unique(substr(df_orders$order_date, 1, 4)))

df_ans_task_10 <- 
  df_orders %>% 
  mutate(year=substr(order_date, 1, 4)) %>% 
  select(customer_id, year) %>% 
  unique() %>% 
  group_by(customer_id) %>% 
  reframe(orders_count = n()) %>% 
  filter(orders_count == 3) %>% 
  inner_join(df_customers, join_by(customer_id)) %>% 
  select(first_name, last_name) %>% 
  mutate(innitials = substr(last_name, 1, 1))

df_ans_task_10_2 <- tolower(unlist(strsplit(df_ans_task_10$last_name, "")))
letters_occurrences <- table(df_ans_task_10_2)
barplot(letters_occurrences, main = "Occurences of letters in surnames", xlab = "Letters", ylab = "Occurrences")



## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- c(df_ans_task_10, letters_occurrences)


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

df_ans_task_11 <-
  df_orders %>% 
  inner_join(df_order_items, join_by(order_id)) %>% 
  inner_join(df_products, join_by(product_id)) %>% 
  inner_join(df_categories, join_by(category_id)) %>% 
  inner_join(df_customers, join_by(customer_id)) %>% 
  mutate(full_name= paste(first_name, " ", last_name), year=substr(order_date, 1, 4), newest = (year==model_year)) %>% 
  select(order_id, full_name, category_name, newest) %>% 
  group_by(full_name, category_name) %>% 
  summarise(count_bikes = n(), count_newest=sum(newest))

newest_bikes <-
  df_ans_task_11 %>% 
  group_by(full_name) %>% 
  summarise(newest = sum(count_newest)) %>% 
  pull(newest)

df_ans_task_11 <- df_ans_task_11 %>% 
  pivot_wider(id_cols = full_name, names_from = category_name, values_from = count_bikes)
df_ans_task_11[is.na(df_ans_task_11)] <- 0
df_ans_task_11[,"newest_bought"] <- newest_bikes
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_ans_task_11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

df_ans_task_12 <-
  df_orders %>% 
  inner_join(df_order_items, join_by(order_id)) %>% 
  inner_join(df_products, join_by(product_id)) %>% 
  mutate(discount_val = discount) %>% 
  select(order_date, product_name, discount_val) %>% 
  mutate(day_of_the_week = weekdays(as.Date(order_date))) %>% 
  group_by(day_of_the_week, product_name) %>% 
  summarise(avg_discount = mean(discount_val)) %>% 
  pivot_wider(id_cols = product_name, names_from = day_of_the_week, values_from = avg_discount) %>% 
  relocate(product_name, Monday, Wednesday, Tuesday, Thursday, Friday, Saturday, Sunday)

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_ans_task_12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "TaranJan.rds")
