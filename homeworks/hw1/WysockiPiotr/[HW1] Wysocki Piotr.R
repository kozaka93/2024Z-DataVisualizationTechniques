library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

df_orders <- read.csv("C:/Users/Admin/Downloads/orders.csv")
df_order_items <- read.csv("C:/Users/Admin/Downloads/order_items.csv")
df_products <- read.csv("C:/Users/Admin/Downloads/products.csv")
df_brands <- read.csv("C:/Users/Admin/Downloads/brands.csv")
df_categories <-  read.csv("C:/Users/Admin/Downloads/categories.csv")
df_customers <- read.csv("C:/Users/Admin/Downloads/customers.csv")

head(df_customers)
####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.
df_orders %>% mutate(data=strftime(order_date,'%Y-%m')) %>% group_by(data)
pom <- df_order_items %>% left_join(df_orders) %>% left_join(df_customers)
ANS_TASK_01<- pom %>% group_by(Rok=strftime(order_date,"%Y"),kwartal=ceiling(as.integer(strftime(order_date,"%m"))/3),state,product_id) %>% 
summarise(ilosc = sum(quantity)) %>%  
group_by(Rok,kwartal,state) %>% filter(ilosc==max(ilosc)) %>% 
left_join(df_products) %>% arrange(desc(ilosc)) %>% select(model_year,product_name,Rok,kwartal,state)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 


## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% mutate(czy = case_when(required_date == shipped_date ~0, .default = 1)) %>% 
  group_by(strftime(required_date, "%m")) %>% 
  summarize(procent =  sum(czy)*100/n())


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
p <- df_order_items %>% left_join(df_products) %>% left_join(df_orders) %>%
group_by(product_id, Rok = strftime(order_date,"%Y")) %>% 
transmute(product_id = product_id, product_name = product_name, income = quantity * (list_price-discount*list_price), Rok=Rok) %>% 
summarise(suma_przychod = sum(income),Rok = Rok, product_name = product_name) %>% distinct()
p <- p %>% group_by(Rok) %>% filter(suma_przychod == max(suma_przychod)) %>% select(product_name,Rok)
## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- p


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% group_by(customer_id, Rok = strftime(order_date,"%Y")) %>% 
  summarise(ilosc = n()) %>% group_by(Rok) %>% filter(ilosc == max(ilosc)) %>% 
  group_by(Rok,ilosc) %>% summarise(ile_klientow = n()) %>% distinct()


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_orders %>% left_join(df_customers) %>%
  mutate(domena = str_extract(email,"(?<=@)[\\w\\.]+")) %>% 
  group_by(Rok = strftime(order_date,"%Y"), domena) %>% summarise(ile_domen = n()) %>% 
  filter(ile_domen == max(ile_domen))


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

df_customers %>% filter(state == "CA" | state == "TX") %>% summarise(n())

df_costumers %>% left_join(df_orders) %>%
filter(state == "CA" | state == "TX") %>% 
transmute(czy_bez = case_when(is.null(order_date) ~1, .default = 0)) %>% 
summarise(sum(czy_bez))

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- c(426,"nie")


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_order_items %>% left_join(df_orders) %>%
  mutate(pom=list_price*(1-discount), cal_wartosc_zam = pom*quantity) %>% 
  group_by(order_id) %>% mutate(cal_wartosc_zam = sum(cal_wartosc_zam)) %>%
  mutate(czy_ekstr = case_when(pom<0.05*cal_wartosc_zam | pom>0.95*cal_wartosc_zam ~1, .default = 0)) %>% 
  filter(sum(czy_ekstr)>1) %>% select(customer_id) %>% distinct() %>% pull(customer_id)


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
df_orders %>% group_by(order_date) %>% mutate(ile = n()) %>%
distinct(order_date,ile) %>% ungroup() %>% 
group_by(kwartal = ceiling(as.numeric(strftime(order_date, "%m"))/3)) %>% 
summarise(max = max(ile),min = min(ile), mediana = median(ile))
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>% group_by(order_date) %>% mutate(ile = n()) %>%
  distinct(order_date,ile) %>% ungroup() %>% 
  group_by(kwartal = ceiling(as.numeric(strftime(order_date, "%m"))/3)) %>% 
  summarise(max = max(ile),min = min(ile), mediana = median(ile))

?pivot_wider
####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
pom2 <- df_orders %>% left_join(df_customers) %>% 
group_by(Rok = strftime(order_date,"%Y"),state) %>% 
summarise(sredni_czas = mean(difftime(as.Date(shipped_date),as.Date(order_date)),na.rm = TRUE))

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- pivot_wider(pom2,names_from = state,values_from = sredni_czas)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
pom3 <- df_customers %>% left_join(df_orders) %>% group_by(customer_id) %>% 
mutate(lata = n_distinct(strftime(order_date, "%Y"))) %>% filter(lata == 3) %>%
transmute(litera = substring(last_name,1,1)) %>% distinct()

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- table(pom3[2])

####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)
pom4<-df_orders %>% left_join(df_order_items) %>% 
  full_join(df_products, by = join_by(product_id)) %>% 
  group_by(customer_id,product_id) %>% 
summarise(ilosc = n())
pom4 <- pivot_wider(pom4,names_from=product_id,values_from = ilosc,values_fill = 0)
najnowszy<-as.vector(df_orders %>% left_join(df_order_items) %>% 
  inner_join(df_products, by = join_by(product_id)) %>% 
  group_by(customer_id,product_id) %>% 
  mutate(najnowszy = case_when(as.integer(strftime(order_date,"%Y")) == model_year ~1, .default = 0)) %>% 
  ungroup() %>% group_by(customer_id) %>% summarise(najnowszy = sum(najnowszy)) %>% 
  arrange(customer_id) %>% select(najnowszy))
typeof(najnowszy$najnowszy)
## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <-pom4 %>% filter(!is.na(customer_id)) %>% mutate(najnowszy)

### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_orders %>% left_join(df_order_items) %>% mutate(week = wday(order_date,week_start =1)) %>% 
  group_by(week, product_id) %>% transmute(sredni_rabat = mean(discount)) %>% 
  arrange(product_id,week) %>% distinct()


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "WysockiPiotr.rds")
