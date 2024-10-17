library(dplyr)
library(tidyr)
#
df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers  <-  read.csv('homeworks/hw1/dane/customers.csv')
df_stores  <-  read.csv('homeworks/hw1/dane/stores.csv')
####### Zadanie 1


# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.
wynik <- df_orders%>%  mutate(
  month = as.numeric(substr(order_date, 6, 7)), # Wyodrębnienie miesiąca jako liczby
  kwartal = case_when(
    month %in% 1:3 ~ 1,
    month %in% 4:6 ~ 2,
    month %in% 7:9 ~ 3,
    month %in% 10:12 ~ 4
  ),
  rok = as.numeric(substr(order_date,1,4))
)%>%
  select(-month)
wynik <- wynik %>% left_join(df_customers %>% select("customer_id", "state"), by=("customer_id"))
wynik <- df_order_items %>% left_join(wynik %>% select("order_id", "state", "kwartal","rok"), by=("order_id"))
wynik <- wynik %>%
  group_by(product_id,rok, kwartal, state) %>%
  summarize(total_quantity = sum(quantity), .groups = "drop") %>%
  group_by(state,rok, kwartal) %>%   
  filter(total_quantity == max(total_quantity)) %>%
  ungroup()

wynik <- wynik %>% left_join(df_products %>% select(product_id, product_name,model_year), by =("product_id"))
## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- select(wynik, product_name, model_year, rok, kwartal,state)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 
wynik2 <- df_orders%>%  mutate( rok = as.numeric(substr(order_date,1,4)),
  month = as.numeric(substr(order_date, 6, 7))) 
wynik2 <- wynik2 %>% mutate (zrealizowany = ifelse(shipped_date == "NULL", 0, 1)) %>% group_by(rok,month) %>%
  summarize(Tak = sum(zrealizowany==1), Nie = sum(zrealizowany==0),.groups = "drop") %>% mutate(Procent = (1-Tak/(Tak+Nie)) *100 )

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- select(wynik2, Procent, month, rok)


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
wynik3 <- df_orders%>%  mutate(rok = as.numeric(substr(order_date,1,4)))
wynik3 <- df_order_items %>% left_join(wynik3 %>% select(rok, order_id), by = ("order_id")) 
wynik3 <- wynik3 %>% mutate(zarobek = list_price * quantity * (1- discount))
wynik3 <-wynik3 %>%
  group_by(rok, product_id) %>%
  summarise(laczny_zarobek = sum(zarobek), .groups = "drop") %>%
  group_by(rok) %>%
  filter(laczny_zarobek == max(laczny_zarobek))
wynik3 <- wynik3 %>% left_join(df_products %>% select(product_id, product_name), by = ("product_id"))
 

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <-  wynik3 %>% select(rok,product_name)


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 
wynik4 <- df_orders%>%  mutate(rok = as.numeric(substr(order_date,1,4)))
wynik4 <- wynik4 %>%
  group_by(rok, customer_id) %>%
  summarise(liczba_zamowien = n(), .groups = "drop") %>%
  group_by(rok) %>%
  filter(liczba_zamowien == max(liczba_zamowien)) %>%
  mutate(maks_zamowienia = max(liczba_zamowien)) %>%
  summarise(liczba_klientow = n(), maks_zamowienia = first(maks_zamowienia), .groups = "drop")
 
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- wynik4


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
wynik5 <- df_orders%>%  mutate(rok = as.numeric(substr(order_date,1,4)))
wynik5 <- wynik5 %>% left_join(df_customers %>% select(customer_id, email), by=("customer_id")) %>% 
  mutate(domain = sub(".*@", "", email))    %>% 
  group_by(rok, domain) %>% summarise(licizba_mailii = n(), .groups = "drop") %>% group_by(rok) %>% filter(licizba_mailii == max(licizba_mailii))
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- wynik5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?
tmp1 <- unique(df_orders$customer_id)

wynik6 <- df_customers %>%
  filter(state %in% c("TX", "CA")) %>%
  mutate(is_active = case_when(customer_id %in% tmp1 ~ 1,TRUE ~ 0)) %>%
  group_by(state) %>%
  summarise(Liczba = sum(is_active), LiczbaNieaktywnych =  n() - sum(Liczba), .groups = "drop") 
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <-wynik6


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
wynik7 <- df_orders %>% left_join(df_order_items %>% select(order_id, list_price, discount, quantity), by = "order_id") %>% left_join(df_customers %>% select(first_name, last_name, customer_id), by = "customer_id") %>%
  mutate(Wartość = quantity * (list_price*(1-discount))) %>% group_by(order_id) %>% summarise(Całość = sum(Wartość))
quantiles <- wynik7 %>%
  summarise(q5 = quantile(Całość, 0.05), q95 = quantile(Całość, 0.95))
wynik7 <- wynik7 %>% filter(Całość < quantiles$q5 | Całość > quantiles$q95)
wynik7 <- wynik7 %>% left_join(df_orders %>% select("order_id", "customer_id"), by = "order_id") %>%
  left_join(df_customers %>% select(customer_id,first_name, last_name), by = "customer_id") %>% distinct(customer_id, first_name, last_name)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- wynik7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
wynik8 <- df_orders%>%  mutate(
  month = as.numeric(substr(order_date, 6, 7)), 
  kwartal = case_when(
    month %in% 1:3 ~ 1,
    month %in% 4:6 ~ 2,
    month %in% 7:9 ~ 3,
    month %in% 10:12 ~ 4
  ),
  rok = as.numeric(substr(order_date,1,4)), dzień = as.numeric(substr(order_date,9,10))) %>% group_by(rok, kwartal, dzień) %>% summarise(liczbaZ = n(), .groups = "drop") %>% group_by(kwartal,rok) %>%
  summarise(mediana = median(liczbaZ), maks = max(liczbaZ), minimum = min(liczbaZ), .groups = "drop")
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- wynik8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
wynik9 <- df_orders %>% left_join(df_customers %>% select(state, customer_id), by = ("customer_id")) %>% 
  mutate(rok = as.numeric(substr(order_date,1,4))) %>% filter(shipped_date != "NULL")%>%   mutate(
    order_date = as.Date(order_date, format = "%Y-%m-%d"),
    shipped_date = as.Date(shipped_date, format = "%Y-%m-%d"),
    days_between = as.numeric(shipped_date - order_date)) %>% group_by(rok, state) %>% 
 summarise(średniCzas = mean(days_between), .groups = "drop") %>% select(rok, state, średniCzas) %>% pivot_wider(names_from = state, values_from = średniCzas)

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- wynik9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
wynik10 <- df_orders %>% left_join(select(df_customers, c(customer_id, last_name)), by = "customer_id") %>%  mutate(rok = as.numeric(substr(order_date,1,4))) %>%
  group_by(last_name) %>% summarize(years_count = n_distinct(rok)) %>% filter(as.data.frame(years_count == 3)) %>% mutate(litera = substr(last_name,1,1)) %>% 
  group_by(litera) %>% summarise(Liczność = n(),.groups = "drop")

                                                                                                  
## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- wynik10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)
wynik11 <- df_orders %>% left_join(df_order_items %>% select(product_id, order_id), by="order_id")%>% left_join(df_products %>% select(product_id,category_id),by = "product_id") %>% 
  left_join(df_categories, by= "category_id") %>%
  left_join(df_products%>%  select(product_id, model_year), by = "product_id")%>% left_join(df_customers %>% select(first_name,last_name, customer_id), by = "customer_id") %>% 
  mutate(rok = as.numeric(substr(order_date,1,4)), nowy = ifelse(model_year==rok,1,0)) %>% group_by(first_name,last_name, category_id) %>%
  summarise(Liczba = n(), LiczbaNowych = sum(nowy), .groups = "drop") %>% group_by(first_name,last_name) %>% summarise(LiczbaNowych = sum(LiczbaNowych), Liczba = sum(Liczba), category_id=category_id, .groups = "drop") %>%
  pivot_wider(names_from = category_id, values_from = Liczba, values_fill = 0) 

## Odpowiedz przypisana do zmiennej## Odpowiedz przypisana do zmiennejLiczba
ANS_TASK_11 <- wynik11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
tmp12 <- df_order_items$quantity * df_order_items$list_price

wynik12 <- df_order_items %>%
  left_join(select(df_orders, order_id, order_date), by = "order_id") %>%
  mutate(DzienTygodnia = strftime(as.Date(order_date), format = "%a"),
         discount = tmp1 - tmp1 * (1 - discount)) %>%
  group_by(product_id, DzienTygodnia) %>%
  summarise(Srednia = mean(discount), .groups = "drop") %>%
  left_join(select(df_products, product_id, product_name), by="product_id") %>%
  select(product_name, DzienTygodnia, Srednia)
## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- wynik12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "SyrkiewiczMichal.rds")