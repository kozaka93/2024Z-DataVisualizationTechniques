library(dplyr)
library(tidyr)

df_brands <- read.csv("dane/brands.csv")
df_categories <- read.csv("dane/categories.csv")
df_customers <- read.csv("dane/customers.csv")
df_order_items <- read.csv("dane/order_items.csv")
df_orders <- read.csv("dane/orders.csv")
df_products <- read.csv("dane/products.csv")
df_staffs <- read.csv("dane/staffs.csv")
df_stocks <- read.csv("dane/stocks.csv")
df_stores <- read.csv("dane/stores.csv")



####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.
wszystko <- df_order_items %>% 
  inner_join(df_orders, by="order_id") %>% 
  inner_join(df_customers, by = "customer_id") %>%
  inner_join(df_products, by = "product_id") %>% 
  mutate(quater=ceiling(as.integer(substr(order_date, 6,7))/3))

wynik1 <- wszystko %>% 
  group_by(quater, state, product_name, model_year) %>% 
  summarise(ilosc = sum(quantity), .groups = 'drop') %>% 
  group_by(quater, state) %>% 
  slice_max(ilosc, n = 1, with_ties = FALSE) 

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- wynik1


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

zamowienia <- df_orders %>% 
  mutate(
    order_date = as.Date(order_date),  
    rok = as.integer(format(order_date, "%Y")),
    miesiac = as.integer(format(order_date, "%m"))
  )
wynik2 <- zamowienia %>% 
  group_by(rok, miesiac) %>% 
  summarise(wszystkie_zamowienia = n(), 
            niespelnione_zamowienia = sum(order_status != 4),
            procent = (niespelnione_zamowienia/wszystkie_zamowienia)*100)


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- wynik2


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
przychod <- df_order_items %>%
  inner_join(df_products, by="product_id", suffix = c("","2")) %>% 
  mutate(przychody = list_price*(1-discount)) %>%
  inner_join(df_orders, by="order_id") %>% 
  mutate(data_zamowienia = as.Date(order_date)) %>%
  mutate(rok = format(data_zamowienia, "%Y")) %>%
  group_by(rok, product_name) %>%
  summarise(caly_dochod = sum(przychody, na.rm = TRUE)) %>%
  group_by(rok) %>%
  slice_max(caly_dochod, n=1, with_ties = FALSE)
  



## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- przychod


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

klienci <- df_orders %>% 
  mutate(data_zamowienia = as.Date(order_date)) %>% 
  mutate(rok = format(data_zamowienia, "%Y")) %>% 
  group_by(customer_id,rok) %>% 
  summarise(liczba_zamowien = n()) %>% 
  group_by(rok) %>% 
  slice_max(liczba_zamowien, n=1, with_ties = FALSE) %>% 
  arrange(rok)




## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- klienci


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
domeny <- df_orders %>% 
  inner_join(df_customers, by="customer_id") %>% 
  mutate(data = as.Date(order_date), rok = format(data, "%Y")) %>% 
  mutate(domena = sub(".*@", "", email)) %>% 
  group_by(rok, domena) %>% 
  summarise(zamowienia = n()) %>% 
  slice_max(zamowienia, n=1, with_ties = FALSE)
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- domeny


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

aktywni_klienci <- df_orders %>%
  inner_join(df_customers, by = "customer_id") %>%
  filter(state %in% c("CA", "TX")) %>%
  group_by(customer_id, state) %>%
  summarise(liczba_zamowien = n()) %>% 
  group_by(state) %>% 
  summarise(liczba_aktywnych = n())

klienci <- df_customers %>% 
  filter(state %in% c("TX", "CA")) %>% 
  left_join(df_orders, by="customer_id") %>% 
  mutate(data= as.Date(order_date)) %>% 
  group_by(customer_id, state) %>% 
  summarise(zamowienia_2018 = sum(format(data, "%Y")== 2018)) %>% 
  filter(zamowienia_2018 == 0)
  




## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(aktywni_klienci, klienci)


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

zad7 <- df_order_items %>% 
  mutate(pieniadze = list_price*(1-discount)*quantity) %>% 
  group_by(order_id) %>% 
  summarise(wszystko = sum(pieniadze, na.rm = TRUE))

kwartyl5 <- quantile(zad7$wszystko, 0.05)
kwartyl95 <- quantile(zad7$wszystko, 0.95)

wynik7 <- zad7 %>% 
  filter(wszystko<kwartyl5 | wszystko>kwartyl95) %>% 
  inner_join(df_orders, by="order_id") %>% 
  inner_join(df_customers, by="customer_id") %>% 
  select(customer_id, first_name, last_name, order_id, wszystko)
  


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- wynik7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
kwa_dzien <- df_orders %>% 
  mutate(data = as.Date(order_date), kwartal=ceiling(as.numeric(format(data, "%m"))/3)) %>% 
  group_by(data, kwartal) %>% 
  summarise(zamowienia = n()) %>% 
  group_by(kwartal) %>% 
  summarise(max_zamowien = max(zamowienia), min_zamowienia= min(zamowienia), mediana_zamowienia = median(zamowienia))


## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- kwa_dzien


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

czas_stan <- df_orders %>% 
  mutate(data_z = as.Date(order_date), data_d = as.Date(shipped_date), czas = as.numeric(data_d-data_z)) %>% 
  inner_join(df_customers, by="customer_id") %>% 
  mutate(rok = format(data_z,"%Y")) %>% 
  group_by(rok, state) %>% 
  summarise(avg_czas = mean(czas, na.rm = TRUE))

zad9 <- czas_stan %>% 
  pivot_wider(names_from = state, values_from = avg_czas)

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- zad9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

co_roku <- df_orders %>%
  mutate(rok = format(as.Date(order_date), "%Y")) %>%
  group_by(customer_id, rok) %>% 
  summarise(ilosc_lat = n_distinct(rok)) %>% 
  summarise(x = sum(ilosc_lat)) %>% 
  filter(x == 3)

zad10 <- co_roku %>% 
  inner_join(df_customers, by="customer_id") %>% 
  mutate(pierwsza_nazwisko = substr(last_name,1,1)) %>% 
  group_by(pierwsza_nazwisko) %>% 
  summarise(zliczanie = n())
  



## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- zad10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)


zad11 <- df_order_items %>%
  inner_join(df_products, by = "product_id") %>%
  inner_join(df_orders, by = "order_id") %>%
  inner_join(df_categories, by = "category_id") %>%
  inner_join(df_customers, by = "customer_id") %>%
  mutate(rok_zamowienia = format(as.Date(order_date), "%Y"), najnowszy = ifelse(rok_zamowienia == model_year, 1, 0))

zad11_1 <- zad11 %>% 
  group_by(customer_id, category_name) %>% 
  summarise(zakupy = sum(quantity)) %>% 
  pivot_wider(names_from = category_name, values_from = zakupy, values_fill = 0)

zad11_2 <- zad11 %>% 
  group_by(customer_id) %>% 
  summarise(ile_najnowyszch = sum(najnowszy))




## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- zad11_1 %>%  left_join(zad11_2, by="customer_id")
  


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat


zad12w <- df_order_items %>%
  inner_join(df_products, by = "product_id") %>%
  inner_join(df_orders, by = "order_id") %>%
  mutate(data_zamowienia = as.Date(order_date),dzien_tygodnia = weekdays(data_zamowienia)) %>% 
  mutate(rabat_procentowy = (list_price.x - list_price.x * (1 - discount)) / list_price.x * 100) %>% 
  group_by(product_name, dzien_tygodnia) %>%
  summarise(sredni_rabat = mean(rabat_procentowy))

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- zad12w



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "BaraniakWojciech.rds")
