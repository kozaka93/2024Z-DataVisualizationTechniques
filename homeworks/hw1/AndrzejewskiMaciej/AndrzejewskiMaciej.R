library(dplyr)
library(tidyr)

Sys.setlocale("LC_ALL","English")
df_orders <- read.csv('orders.csv')
df_order_items <- read.csv('order_items.csv')
df_products <- read.csv('products.csv')
df_brands <- read.csv('brands.csv')
df_categories <-  read.csv('categories.csv')
df_customers <- read.csv('customers.csv')
df_stores <- read.csv("stores.csv")


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.
# (przyjęto, że kwartały o tym samym numerze w dwóch różnych latach to dwa różne kwartały)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_orders %>% left_join(df_customers) %>% right_join(df_order_items) %>% 
  select(order_date,product_id,quantity, state) %>% 
  mutate(year = substr(order_date,1,4),quarter=(ceiling(as.numeric(substr(order_date,6,7))/3)),sep=":") %>% 
  group_by(product_id, state, quarter, year) %>%
  summarise(product_sum = sum(quantity), .groups="keep") %>% 
  group_by(state, quarter, year) %>% 
  filter(product_sum == max(product_sum)) %>% 
  left_join(df_products) %>% select(product_name, model_year, state, year, quarter) %>% 
  arrange(year, quarter, state)



####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 
# (przyjęto, że to te o order_status == 3 ["rejected"], a miesiąc o tej samej nazwie
# w dwóch różnych latach potraktowano osobno)

status_3 <- df_orders %>% filter(order_status == 3) %>% 
  mutate(year_month = strftime(order_date, "%Y-%m")) %>% 
  group_by(year_month) %>% summarize(number_rejected = n())
status_any <- df_orders %>% mutate(year_month = strftime(order_date, "%Y-%m")) %>% 
  group_by(year_month) %>% summarize(sum = n())
z2 <- status_any %>% left_join(status_3) %>% 
  mutate(number_rejected = ifelse(is.na(number_rejected),0,number_rejected)) %>% 
  mutate(percentage_of_rejected = paste(format(number_rejected/sum*100,digits=3), "%")) %>% 
  select(year_month, percentage_of_rejected)

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- z2


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>% left_join(df_products) %>% left_join(df_orders) %>% 
  select(product_name, order_date, quantity, list_price) %>% 
  mutate(order_date = strftime(order_date, "%Y")) %>% 
  group_by(product_name, order_date, list_price) %>% 
  summarize(quant_sum = sum(quantity), .groups="keep") %>% 
  mutate(profit = quant_sum*list_price) %>% 
  group_by(order_date) %>% filter(profit == max(profit)) %>% 
  select(product_name, order_date) %>% rename(year = order_date) %>% 
  arrange(year)


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% 
  select(customer_id, order_date) %>% 
  mutate(year = strftime(order_date, "%Y")) %>% 
  group_by(customer_id, year) %>% 
  summarize(num_of_orders_per_customer = n())  %>% 
  group_by(year) %>% 
  filter(num_of_orders_per_customer == max(num_of_orders_per_customer)) %>% 
  group_by(year, num_of_orders_per_customer) %>% 
  summarize(num_of_customers = n()) %>%
  relocate(num_of_customers, .before = num_of_orders_per_customer)


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_orders %>% left_join(df_customers) %>% select(email, order_date) %>%
  mutate(domain = substring(email, regexpr("@", email)+1),
         year = substr(order_date, 1,4)) %>% 
  group_by(domain, year) %>% 
  summarize(count = n(), .groups="keep") %>% 
  group_by(year) %>% 
  filter(count == max(count)) %>% 
  arrange(year) %>% relocate(2,1,3)

####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

active <- df_orders %>% left_join(df_customers) %>% select(customer_id, state) %>% 
  distinct() %>% group_by(state) %>% summarize(num_of_customers = n()) %>% 
  filter(state %in% c("CA", "TX"))
active <- active %>% add_row(state = "any", num_of_customers = sum(active$num_of_customers))

cust_with_ord_in_2018 <- df_orders %>% left_join(df_customers) %>% 
  filter(state %in% c("CA", "TX")) %>% 
  mutate(year = substring(order_date, 1,4)) %>% 
  group_by(customer_id, year) %>% 
  summarize(.groups = "keep") %>% filter(year == 2018) %>% 
  mutate(order_in_2018 = TRUE)

no_2018_order <- df_customers %>% filter(state %in% c("CA", "TX")) %>% 
  left_join(cust_with_ord_in_2018) %>% 
  mutate(order_in_2018 = ifelse(is.na(order_in_2018), FALSE, TRUE)) %>% 
  group_by(order_in_2018) %>% summarize(counts = n())

## Odpowiedz przypisana do zmiennej
#active - liczba aktywnych klientów, no_2018_order - brak zamowienia w 2018 z tych stanow
ANS_TASK_06 <- list(active, no_2018_order)


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu 
## poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

orders_prices <- df_order_items %>% 
  mutate(final_price = list_price*quantity*(1-discount)) %>% 
  left_join(df_orders) %>% select(customer_id, final_price, order_id) %>% 
  group_by(order_id, customer_id) %>% summarize(whole_order_price = sum(final_price))

quantiles <- quantile(orders_prices$whole_order_price, probs = c(0.05, 0.95))

z7 <- orders_prices %>% 
  filter(whole_order_price < quantiles[1] |
           whole_order_price > quantiles[2]) %>% 
  left_join(df_customers) %>% ungroup() %>% 
  select(customer_id, first_name, last_name) %>% distinct()
  
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- z7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
# (przyjęto, że kwartały o tym samym numerze w dwóch różnych latach to dwa różne kwartały)

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>% select(order_id,order_date) %>% 
  mutate(year = substr(order_date,1,4),
         quarter = ceiling(as.numeric(substr(order_date, 6, 7))/3), 
         day = substr(order_date, 6,10)) %>% 
  group_by(quarter, day, year) %>%
  summarize(orders_in_day = n(), .groups = "keep") %>%
  group_by(quarter, year) %>% summarize(min = min(orders_in_day), max = max(orders_in_day), 
                                        median = median(orders_in_day)) %>% 
  relocate(2, .before = 1) %>% arrange(year, quarter)


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>% left_join(df_customers) %>% 
  select(order_date, shipped_date, state) %>% 
  filter(shipped_date != "NULL") %>% 
  mutate(year = substr(order_date, 1,4), 
         order_time = as.integer(as.POSIXct(shipped_date)-as.POSIXct(order_date))) %>% 
  group_by(year,state) %>% summarize(mean = mean(order_time), .groups = "keep") %>% 
  mutate(mean = paste(format(mean,digits=3),"days")) %>% 
  pivot_wider(names_from = state, values_from = mean)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# (zrobili zamowienie w każdym z lat 2016, 2017, 2018)
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.


## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_orders %>% select(customer_id, order_date) %>% 
  mutate(year = paste("y",substr(order_date,1,4),sep=""), check = 1) %>% group_by(customer_id) %>%
  select(!order_date) %>% 
  pivot_wider(values_from = check, names_from = year, values_fn = list) %>% 
  filter(!(y2016 == "NULL" | y2017 == "NULL" | y2018 == "NULL")) %>% 
  left_join(df_customers) %>% ungroup() %>% select(last_name) %>% 
  transmute(first_letter = substring(last_name,1,1)) %>% 
  group_by(first_letter) %>% summarize(count = n()) %>% arrange(-count)


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

z11 <- df_order_items %>% left_join(df_orders) %>% left_join(df_products) %>% 
  select(customer_id,quantity, order_date, model_year, category_id)

newest <- z11 %>% 
  transmute(customer_id = customer_id,
            newest_model = ifelse(substring(order_date, 1, 4) == model_year, 1, 0)) %>% 
  group_by(customer_id) %>% summarize(count_newest = sum(newest_model))
  
z11_2 <- z11 %>% select(!c(order_date, model_year)) %>% 
  group_by(customer_id, category_id) %>%
  summarize(quantity_total = sum(quantity), .groups = "keep") %>%
  left_join(df_categories) %>% ungroup() %>% select(!category_id) %>% 
  pivot_wider(names_from = category_name, values_from = quantity_total, values_fill = 0) %>% 
  left_join(newest)
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- z11_2


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <-  df_order_items %>% left_join(df_orders) %>% 
  select(product_id, order_date, discount) %>% 
  #wzor na podstawie definicji roznicy procentowej, mianownikiem ułamka jest średnia porównywanych liczb
  # 100*abs(A-B)/((A+B)/2), po skróceniu we wzorze do obliczeia wystarczy wartość discount
  mutate(percentage_difference = 2*discount/(2-discount) * 100,
         weekday = strftime(order_date, "%A")) %>% 
  select(!c(order_date,discount)) %>% group_by(weekday, product_id) %>% 
  summarize(mean_percent_diff =
              paste(format(mean(percentage_difference), digits = 4), "%"), .groups = "keep") %>% 
  pivot_wider(names_from = weekday, values_from = mean_percent_diff) %>% 
  relocate(c(1,3,7,8,6,2,4,5))



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "AndrzejewskiMaciej.rds")
