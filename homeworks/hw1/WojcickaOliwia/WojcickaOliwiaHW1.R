library(dplyr)
library(tidyr)


df_orders <- read.csv('/Users/oliwi/Desktop/WojcickaOliwia/hw1/dane/orders.csv')
df_order_items <- read.csv('/Users/oliwi/Desktop/WojcickaOliwia/hw1/dane/order_items.csv')
df_products <- read.csv('/Users/oliwi/Desktop/WojcickaOliwia/hw1/dane/products.csv')
df_brands <- read.csv('/Users/oliwi/Desktop/WojcickaOliwia/hw1/dane/brands.csv')
df_categories <-  read.csv('/Users/oliwi/Desktop/WojcickaOliwia/hw1/dane/categories.csv')
df_customers <-  read.csv('/Users/oliwi/Desktop/WojcickaOliwia/hw1/dane/customers.csv')
df_staffs <-  read.csv('/Users/oliwi/Desktop/WojcickaOliwia/hw1/dane/staffs.csv')
df_stocks <-  read.csv('/Users/oliwi/Desktop/WojcickaOliwia/hw1/dane/stocks.csv')
df_stores <-  read.csv('/Users/oliwi/Desktop/WojcickaOliwia/hw1/dane/stores.csv')


####### Zadanie 1
# Ktory produkt by?? najcz????ciej kupowany w ka??dym kwartale w podziale na stany z ktorych pochodz?? klienci? 
# Podaj nazw?? produktu i rok jego produkcji.

#NAJCZ????CIEJ KUPOWANY - ILE ZOSTA??O ZAKUPIONEGO TEGO PRODUKTU

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_orders %>% 
  inner_join(df_customers, by = 'customer_id') %>% 
  inner_join(df_order_items, by = 'order_id') %>% 
  inner_join(df_products, by = 'product_id') %>% 
  select(order_date, state, product_name, quantity, model_year) %>% 
  mutate(month = substr(order_date, start = 6, stop = 7)) %>% 
  mutate(quarter = case_when((month == '01' | month == '02' | month == '03') ~  '1',
                             (month == '04' | month == '05' | month == '06') ~ '2',
                             (month == '07' | month == '08' | month == '09') ~ '3',
                             TRUE ~ '4')) %>% 
  group_by(quarter, state, product_name) %>% 
  summarise(how_many = sum(quantity), model_year = first(model_year), .groups = 'drop') %>% 
  group_by(quarter, state) %>% 
  filter(how_many == max(how_many)) %>% 
  ungroup() %>% 
  select(product_name, model_year)
 

####### Zadanie 2 
# Jaki procent wszystkich zamowien nie zosta?? zrealizowany w kazdym miesi??cu? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% 
  inner_join(df_order_items, by= 'order_id') %>% 
  mutate(month = substr(order_date, start = 6, stop = 7)) %>% 
  select(month, order_id, order_status) %>% 
  distinct() %>% 
  mutate(if_not_made = ifelse(order_status != '4', 1, 0)) %>% 
  group_by(month) %>% 
  summarise('percent(in%)' = (sum(if_not_made)/n())*100)
  

####### Zadanie 3
# Jaki produkt przynios?? najwi??kszy przychod w kazdym roku? 

# ZAK??ADAM, ??E TRANSAKCJA P??ATNO??CI PRZECHODZI JAK KLIENT ODBIERZE PRODUKT 

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>% 
  mutate(price = list_price - discount*list_price) %>% 
  inner_join(df_products, by = 'product_id') %>% 
  inner_join(df_orders, by = 'order_id') %>% 
  mutate(year = substr(shipped_date, start = 1, stop = 4)) %>% 
  select(product_name, quantity, price, year) %>% 
  filter(year != 'NULL') %>% 
  group_by(product_name, year) %>% 
  summarise(quantity = sum(quantity), price = first(price), .groups = 'drop') %>% 
  mutate(income = quantity*price) %>% 
  group_by(year) %>% 
  filter(income == max(income)) %>% 
  arrange(year) %>% 
  select(product_name, year, income)
  

####### Zadanie 4
## Ile klientow zrobilo najwieksze zakupy (czyli zrobili najwi??cej zamowie??) w kazdym roku i ile to by??o zamowie??? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_customers %>% 
  inner_join(df_orders, by = 'customer_id') %>% 
  mutate(year = substr(order_date, start = 1, stop = 4)) %>% 
  select(customer_id, order_id, year) %>% 
  group_by(year, customer_id) %>% 
  summarise(per_person = n(), .groups = 'drop') %>% 
  group_by(year) %>% 
  filter(per_person == max(per_person)) %>% 
  mutate(people = n()) %>% 
  select(year, people, per_person) %>% 
  distinct()


####### Zadanie 5
# Z jakiej domeny mailowej najcz??sciej robiono zamowienia w ka??dym roku? Ile by??o tych zamowie???

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_customers %>% 
  inner_join(df_orders, by = 'customer_id') %>% 
  inner_join(df_order_items, by = 'order_id') %>% 
  mutate(year = substr(order_date, start = 1, stop = 4)) %>% 
  mutate(domain = sub('.*@', '', email)) %>% 
  select(domain, year, order_id) %>% 
  distinct() %>% 
  group_by(domain, year) %>% 
  summarise(orders_total = n(), .groups = 'drop') %>% 
  group_by(year) %>% 
  filter(orders_total == max(orders_total)) %>% 
  ungroup() %>% 
  arrange(year)


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientow z tych stanow s?? tacy, ktorzy nie zrobili ??adnego zamowienia w 2018?

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- df_customers %>% 
  inner_join(df_orders, by = 'customer_id') %>% #po????czy tylko aktywnych, tych co s?? w bazie orders, czyli co?? zamowili
  mutate(year = substr(order_date, start = 1, stop = 4)) %>% 
  select(customer_id, state, year) %>% 
  filter(state == 'CA' | state == 'TX') %>% 
  group_by(customer_id) %>%
  summarise(person_orders = n(), in_2018 = any(year == "2018")) 

how_many_customers <- data.frame(how_many_customers = nrow(ANS_TASK_06))
how_many_not_in_2018 <- data.frame(how_many_not_in_2018 = ANS_TASK_06 %>% 
  filter(in_2018 == FALSE) %>% 
  nrow())

ANS_TASK_06 <- merge(how_many_customers, how_many_not_in_2018) 

####### Zadanie 7
## Ktorzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamowieniu poni??ej 5 kwantyla lub wi??cej ni?? 95 kwantyl wartosci zamowienia?

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_order_items %>% 
  inner_join(df_orders, by = 'order_id') %>% 
  inner_join(df_customers, by = 'customer_id') %>% 
  select(customer_id, first_name, last_name, order_id, quantity, list_price, discount) %>% 
  group_by(customer_id, first_name, last_name, order_id) %>% 
  summarise(spent = sum((list_price - discount*list_price)*quantity), .groups = 'drop') %>% 
  filter(spent < quantile(spent, 0.05) | spent > quantile(spent, 0.95)) %>% 
  select(customer_id, first_name, last_name)


####### Zadanie 8
# Oblicz jaka by??a maksymalna i minimalna oraz mediana liczby zamowie?? z??o??onych ka??dego dnia w poszczegolnych kwartalach.

# ZAK??ADAM, ??E KA??DY DZIE?? JEST INNY

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>% 
  mutate(month = substr(order_date, start = 6, stop = 7)) %>% 
  mutate(quarter = case_when((month == '01' | month == '02' | month == '03') ~  '1',
                             (month == '04' | month == '05' | month == '06') ~ '2',
                             (month == '07' | month == '08' | month == '09') ~ '3',
                             TRUE ~ '4')) %>% 
  group_by(quarter, order_date) %>% 
  summarise(per_day = n(), .groups = 'drop') %>% 
  group_by(quarter) %>% 
  summarise(max = max(per_day), min = min(per_day), median = median(per_day))


####### Zadanie 9 
# Jaki by?? ??redni czas dostarczania zamowienia w zale??no??ci od roku i  stanu w ktorym mieszka?? klient. 
# Jako rozwi??zanie przygotuj szeroka postac tabeli, ktora b??dzie mia??a informacj?? o ka??dym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>% 
  inner_join(df_customers, by = 'customer_id') %>% 
  mutate(year = substr(order_date, start = 1, stop = 4)) %>% 
  select(order_id, order_date, shipped_date, year, customer_id, state) %>% 
  mutate(delivery_time = difftime(as.Date(shipped_date), as.Date(order_date), units = 'days')) %>% 
  group_by(year, state) %>% 
  summarise(average_time = mean(delivery_time, na.rm = TRUE), .groups = 'drop') %>% 
  pivot_wider(names_from = state, values_from = average_time)
  

####### Zadanie 10
# Od jakich liter zaczynaj?? si?? nazwiska klientow, ktorzy robili zamowienia co roku. 
# Przeanalizuj jak cz??sto wyst??puje ka??da litera w??rod tych nazwisk. 

# BIOR?? TE LITERY, KTORE S?? PIERWSZYMI NAZWISKA, A NAST??PNIE PATRZ?? ILE RAZY KA??DA Z NICH WYST??PUJE WE WSZYTKICH MOICH NAZWISKACH
# ZAK??ADAM, ZE a TO TA SAMA LITERA CO A
# NA KONIEC LICZ?? PROCENT WYST??POWANIA TYCH LITER WE WSZYSTKICH MO??LIWYCH U??YTYCH W MOICH NAZWISKACH

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_customers %>% 
  inner_join(df_orders, by = 'customer_id') %>% 
  mutate(year = substr(order_date, start = 1, stop = 4)) %>% 
  select(customer_id, last_name, year) %>% 
  group_by(customer_id) %>% 
  filter(any(year == '2016') & any(year == '2017') & any(year == '2018')) %>% 
  ungroup() %>% 
  select(last_name) %>% 
  distinct() %>% 
  mutate(first_letter = substr(last_name, start = 1, stop = 1), last_name = toupper(last_name)) %>% 
  mutate(letters = strsplit(last_name, "")) %>%
  unnest(letters) %>% 
  mutate(all_letters_surname = n()) %>% 
  filter(letters %in% first_letter) %>% 
  group_by(letters) %>% 
  summarise(how_many = n(), all_letters_surname = first(all_letters_surname)) %>% 
  mutate('percent(in%)' = (how_many/all_letters_surname)*100) %>% 
  select(letters, how_many, 'percent(in%)')


####### Zadanie 11
# Zrob zestawienie (szeroka posta?? tabeli) ile razy ka??dy klient kupi?? rower ka??dej kategorii. 
# Je??li nie kupowa?? takiego roweru zaraportuj warto??c"0"
# Dodaj do zestawienia informacj?? ile razy klient kupowa?? najnowszy produkt (rower zosta?? wyprodukowany w tym roku, kiedy z??o??ono zamowienie)

# ROZUMIEM TO TAK, ??E NALE??Y UWZGL??DNI?? LICZB?? ZAKUPIONYCH ROWEROW
# CZYLI JAK W JEDNYM ZAMOWIENIU KUPI?? 2 ROWERY, A W DRUGIM 3, TO KUPI?? 5 RAZY A NIE 2

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_order_items %>% 
  inner_join(df_products, by = 'product_id') %>% 
  inner_join(df_orders, by = 'order_id') %>% 
  inner_join(df_customers, by = 'customer_id') %>% 
  inner_join(df_categories, by = 'category_id') %>% 
  mutate(year = substr(order_date, start = 1, stop = 4)) %>% 
  select(customer_id, category_name, quantity, year, model_year) %>% 
  mutate(is_new = ifelse(year == model_year, quantity, 0)) %>% 
  group_by(customer_id, category_name) %>% 
  summarise(how_many = sum(quantity), is_new = sum(is_new), .groups = 'drop') %>% 
  pivot_wider(names_from = category_name, values_from = c(how_many, is_new), values_fill = 0)  
  

### Zadanie 12
# Jaki by?? ??redni rabat udzielony na ka??dy produkt w ka??dym dniu tygodnia?
# Jako ??redni rabat rozumiemy ro??nic?? procentow?? miedzy przychodem wynikaj??cym z ceny katalogowej a przychodem faktycznym uwzgl??dniaj??cym udzielony rabat

# RABAT ZOSTANIE NALICZONY W DNIU Z??O??ENIA ZAMOWIENIA

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_products %>% 
  inner_join(df_order_items, by = 'product_id') %>% 
  inner_join(df_orders, by = 'order_id') %>% 
  select(product_id, order_date, list_price.x, discount) %>% 
  mutate(week = weekdays(as.Date(order_date)), discount2 = ((list_price.x - (list_price.x - list_price.x*discount))/list_price.x)*100) %>% 
  group_by(product_id, week) %>% 
  summarise(average_discount = mean(discount2), .groups = 'drop')


### Zapisanie rozwi??za?? do pliku .RDS

### Prosz?? nic nie zmienia?? 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Prosz?? zmieni?? tylko nazw?? pliku w komendzie saveRDS na swoje Nazwisko i Imi?? (bez polskich znakow)
saveRDS(solutions, file = "WojcickaOliwia.rds")