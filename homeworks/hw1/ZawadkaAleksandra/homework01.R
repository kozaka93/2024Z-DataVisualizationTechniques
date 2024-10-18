library(dplyr)
library(tidyr)

df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <-  read.csv('homeworks/hw1/dane/customers.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

orders_quarter <- df_orders %>% 
  mutate(quarter =  ceiling((as.numeric(strftime(order_date, '%m')))/3),
         year = strftime(order_date, '%y')) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  select(year, quarter, order_id, state)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_order_items %>% 
  inner_join(orders_quarter, by = "order_id") %>% 
  group_by(year, quarter,state, product_id) %>% 
  summarise(counter = sum(quantity)) %>% 
  filter(counter == max(counter)) %>% 
  inner_join(df_products, by = "product_id") %>% 
  select(product_name, model_year)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% 
  mutate(year = strftime(order_date, '%Y'),
         month = strftime(order_date, '%m')) %>%
  group_by(year, month) %>% 
  summarise(all_orders_count = n(), completed_orders_count = sum(order_status == 4)) %>% 
  mutate(percentage_incompleted = round((1-completed_orders_count/all_orders_count)*100,2)) %>% 
  select(year, month, percentage_incompleted)


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

##co oni ode mnie chcą????????? rjected? hmm????

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>% 
  inner_join(df_orders, by = 'order_id') %>% 
  inner_join(df_products[c('product_id','product_name')], by = 'product_id') %>% 
  mutate(year = strftime(order_date, '%Y'), price = (1-discount)*list_price*quantity) %>% 
  group_by(year, product_name) %>% 
  summarise(income = sum(price)) %>% 
  filter(income == max(income)) 
  

####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej

ANS_TASK_04 <- df_orders %>% 
  mutate(year = strftime(order_date, '%Y')) %>% 
  group_by(year, customer_id) %>% 
  summarise(order_count = n()) %>% 
  filter(order_count == max(order_count)) %>% 
  group_by(year, order_count) %>% 
  summarise(customer_count = n())


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
  
ANS_TASK_05 <- df_orders %>% 
  inner_join(df_customers, by = 'customer_id') %>% 
  mutate(domain = sub(".*@", "", email), year = strftime(order_date, '%Y')) %>% 
  group_by(year, domain) %>% 
  summarise(order_count = n()) %>% 
  filter(order_count == max(order_count))


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

customers_TXCA <- df_customers %>% 
  filter(state == "CA" | state == "TX") 

inactive_in2018 <- df_orders %>% 
  right_join(customers_TXCA['customer_id'], by = "customer_id") %>% 
  mutate (year = strftime(order_date, '%Y')) %>% 
  filter(year == 2018) %>% 
  right_join(customers_TXCA, by = "customer_id") %>% 
  group_by(state) %>%
  filter(is.na(order_id)) %>% 
  summarise(inactive_in2018 = n())

## Odpowiedz przypisana do zmiennej

ANS_TASK_06 <- df_orders %>% 
  right_join(customers_TXCA, by = "customer_id") %>% 
  filter(!is.na(order_id)) %>% 
  distinct(state, customer_id) %>%  
  group_by(state) %>% 
  summarise(active_count = n()) %>% 
  inner_join(inactive_in2018, by = 'state')


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

q05 <- df_orders %>% 
  inner_join(df_order_items, by = 'order_id') %>% 
  mutate(price = (1-discount)*list_price*quantity) %>% 
  group_by(order_id) %>% 
  summarise(order_price = sum(price)) %>% 
  summarise(quantile(order_price, 0.05)) %>% 
  as.numeric()

q95 <- df_orders %>% 
  inner_join(df_order_items, by = 'order_id') %>% 
  mutate(price = (1-discount)*list_price*quantity) %>% 
  group_by(order_id) %>% 
  summarise(order_price = sum(price)) %>% 
  summarise(quantile(order_price, 0.95)) %>% 
  as.numeric()

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_orders %>% 
  inner_join(df_order_items, by = 'order_id') %>% 
  mutate(price = (1-discount)*list_price*quantity) %>% 
  inner_join(df_customers, by = 'customer_id') %>% 
  group_by(order_id, customer_id, first_name, last_name) %>% 
  summarise(order_price = sum(price))%>% 
  filter(order_price < q05 | order_price > q95) %>% 
  group_by(customer_id) %>% slice(1)%>% 
  select(customer_id,first_name,last_name) 


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
##a co gdy brak zamówień

start_date <- as.Date("2016-01-01")
end_date <- as.Date("2018-12-31")
dates <- seq(start_date, end_date, by = "day")

# Tworzenie ramki danych z kolumnami year, quarter, month, day
df_dates <- data.frame(
  order_date = as.character(dates)
)

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>% 
  right_join(df_dates, by="order_date") %>% 
  mutate(quarter =  ceiling((as.numeric(strftime(order_date, '%m')))/3),
         year = strftime(order_date, '%Y'),
         month = strftime(order_date, '%m'),
         day = strftime(order_date, '%d')) %>% 
  group_by(order_date,year,quarter,month,day) %>% 
  summarise(daily = sum(!is.na(order_id))) %>% 
  group_by(year,quarter) %>% 
  summarise(daily_max = max(daily), daily_min = min(daily), daily_median = median(daily))


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>% 
  mutate(delivery_time = difftime(as.Date(shipped_date), as.Date(order_date)),
         year = strftime(order_date, '%Y')) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  group_by(year,state) %>% 
  summarise(mean_delivery_time = mean(delivery_time, na.rm = TRUE)) %>% 
  pivot_wider(names_from = state, values_from = mean_delivery_time)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

orders_year <- df_orders %>% 
  mutate(year = strftime(order_date, '%Y'))

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_orders %>% 
  mutate(year = strftime(order_date, '%Y')) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  group_by(customer_id, last_name) %>% 
  summarise(years_ordered = n_distinct(year)) %>% 
  filter(years_ordered == n_distinct(orders_year$year)) %>% 
  mutate(first_letter = substr(last_name, 1, 1)) %>% 
  group_by(first_letter) %>% 
  summarise(count = n())


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

latest_product <- df_orders %>% 
  right_join(df_customers, by = "customer_id") %>% 
  left_join(df_order_items, by = "order_id") %>% 
  left_join(df_products, by = "product_id") %>%
  mutate(year = strftime(order_date, '%Y')) %>% 
  filter(year == model_year) %>% 
  group_by(customer_id) %>% 
  summarise(latest_product_count = sum(quantity))

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_orders %>% 
  right_join(df_customers, by = "customer_id") %>% 
  left_join(df_order_items, by = "order_id") %>% 
  left_join(df_products, by = "product_id") %>% 
  left_join(df_categories, by = "category_id") %>%
  group_by(customer_id, category_name) %>% 
  summarise(category_count = sum(quantity)) %>% 
  pivot_wider(names_from = category_name, values_from = category_count, values_fill = 0) %>% 
  left_join(latest_product, by = "customer_id")


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_orders %>% 
  mutate(day_of_the_week = strftime(order_date, '%A')) %>% 
  inner_join(df_order_items, by = 'order_id') %>% 
  group_by(day_of_the_week) %>% 
  summarise(mean_discount = weighted.mean(discount, quantity)) %>% 
  mutate(day_order = case_when(
    day_of_the_week == "poniedziałek" ~ 1,
    day_of_the_week == "wtorek" ~ 2,
    day_of_the_week == "środa" ~ 3,
    day_of_the_week == "czwartek" ~ 4,
    day_of_the_week == "piątek" ~ 5,
    day_of_the_week == "sobota" ~ 6,
    day_of_the_week == "niedziela" ~ 7
  )) %>% 
  arrange(day_order) %>%  
  select(-day_order)



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "ZawadkaAleksandra.rds")
