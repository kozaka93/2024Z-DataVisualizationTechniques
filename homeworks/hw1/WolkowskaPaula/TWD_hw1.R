library(dplyr)
library(tidyr)

df_orders <- read.csv('dane/orders.csv')
df_order_items <- read.csv('dane/order_items.csv')
df_products <- read.csv('dane/products.csv')
df_brands <- read.csv('dane/brands.csv')
df_categories <-  read.csv('dane/categories.csv')
df_customers <- read.csv('dane/customers.csv')
df_staffs <- read.csv('dane/staffs.csv')
df_stocks <- read.csv('dane/stocks.csv')
df_stores <- read.csv('dane/stores.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_orders %>% 
  mutate(order_month = as.numeric(substr(order_date, 6, 7)),
         order_qrt = case_when( # dodajemy informację o kwartale, w którym zostało wykonane zamówienie
           order_month %in% 1:3 ~1,
           order_month %in% 4:6 ~2,
           order_month %in% 7:9 ~3,
           order_month %in% 10:12 ~4
         )) %>% 
  inner_join(df_customers, by='customer_id') %>% 
  inner_join(df_order_items, by='order_id') %>% 
  inner_join(df_products, by='product_id') %>% 
  group_by(order_qrt, state, product_id, product_name, model_year) %>% 
  summarise(cnt=sum(quantity), .groups='drop') %>% # sumuje liczbę sprzedanych sztuk dla każdego produktu 
  group_by(order_qrt, state) %>% # grupujemy po kwartale zamówienia i stanie
  filter(cnt==max(cnt)) %>% # wybieramy produkt, który w każdej grupie był najczęściej kupowany
  select(order_qrt, state, product_name, model_year)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% 
  mutate(order_month = as.numeric(substr(order_date, 6, 7)),
         order_year = as.numeric(substr(order_date,1,4))) %>% 
  group_by(order_year, order_month) %>% 
  summarise(pct_not_processed_orders = sum(order_status!=4)*100/n()) # dla każdej grupy wyliczamy procent zamówień, które nie zostały zrealizowane


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>% 
  inner_join(df_products, by='product_id') %>% #łączymy df_order_items z df_products
  inner_join(df_orders, by='order_id') %>% #łączymy df_order_items z df_orders
  mutate(income=quantity*list_price.x*(1-discount), # income - przychód uzyskany ze sprzedaży danego produktu w danym zamówieniu
         order_year = as.numeric(substr(order_date,1,4))) %>%
  group_by(order_year, product_id, product_name) %>% 
  summarise(total_income=sum(income), .groups='drop')%>% 
  group_by(order_year) %>% 
  filter(total_income==max(total_income)) %>% 
  select(order_year, total_income, product_name)


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% 
  mutate(order_year=as.numeric(substr(order_date,1,4))) %>% 
  group_by(customer_id,order_year) %>% 
  summarise(cnt=n(), .groups='drop') %>% 
  group_by(order_year) %>% 
  filter(cnt==max(cnt)) %>% 
  summarise(num_of_clients=n(), max_order_count=max(cnt))


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_orders %>% 
  inner_join(df_customers, by='customer_id') %>% 
  mutate(domain=sub("^(.*?)@(.*?)\\.com$", "\\2", email),
         order_year=as.numeric(substr(order_date,1,4))) %>% 
  group_by(order_year, domain) %>% 
  summarise(cnt=n(), .groups='drop') %>% 
  group_by(order_year) %>% 
  filter(cnt==max(cnt))


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- df_orders %>% 
  inner_join(df_customers, by="customer_id") %>% 
  mutate(order_year=as.numeric(substr(order_date,1,4))) %>% 
  filter(state=='CA'|state=="TX") %>% 
  group_by(customer_id, state) %>% 
  summarise(total_order_count = n(),
            order_count_2018 = sum(order_year==2018),
            .groups='drop') %>% 
  group_by(state) %>% 
  summarise(num_of_customers = n(),
            customers_no_orders_2018 = sum(order_count_2018==0))


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_order_items %>% 
  inner_join(df_orders, by='order_id') %>% 
  inner_join(df_customers, by='customer_id') %>% 
  mutate(final_price=list_price*(1-discount)) %>% 
  group_by(order_id, customer_id, first_name, last_name) %>% 
  summarise(order_value=sum(final_price*quantity), .groups='drop') %>% 
  filter(order_value < quantile(order_value, 0.05) | order_value > quantile(order_value, 0.95)) %>% 
  select(customer_id, first_name, last_name) %>% 
  arrange(customer_id)


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>% 
  mutate(order_day = as.numeric(substr(order_date, 9, 10)),
         order_month = as.numeric(substr(order_date, 6, 7)),
         order_year = as.numeric(substr(order_date, 1, 4)),
         order_qrt = case_when(
           order_month %in% 1:3 ~1,
           order_month %in% 4:6 ~2,
           order_month %in% 7:9 ~3,
           order_month %in% 10:12 ~4)) %>% 
  group_by(order_year, order_month, order_day, order_qrt) %>% 
  summarise(cnt=n(), .groups='drop')%>% 
  group_by(order_year, order_qrt) %>% 
  summarise(min=min(cnt),
            max=max(cnt),
            median=median(cnt))


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>% 
  inner_join(df_customers, by='customer_id') %>% 
  mutate(order_year=as.numeric(substr(order_date,1,4)),
         order_date=as.Date(order_date, format="%Y-%m-%d"),
         shipped_date=as.Date(shipped_date, format="%Y-%m-%d"),
         delivery_time_days=as.numeric(shipped_date-order_date)) %>% 
  group_by(order_year, state) %>% 
  summarise(avg_delivery_time=mean(delivery_time_days, na.rm=TRUE), .groups='drop') %>% 
  pivot_wider(names_from=state, values_from=avg_delivery_time)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
tmp1 <-

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_orders %>% 
  inner_join(df_customers, by='customer_id') %>% 
  mutate(order_year=as.numeric(substr(order_date,1,4)),
         last_name_first_letter=substr(last_name, 1,1)) %>% 
  group_by(customer_id,last_name, last_name_first_letter) %>% 
  summarise(years_count=n_distinct(order_year), .groups='drop') %>% 
  filter(years_count==3) %>% 
  group_by(last_name_first_letter) %>% 
  summarise(num_of_clients=n())


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

# Obliczamy ile razy każdy klient kupił rower każdej kategorii
temp1 <- df_orders %>% 
  inner_join(df_order_items, by='order_id') %>%
  inner_join(df_products, by='product_id') %>% 
  group_by(customer_id, category_id) %>% 
  summarise(cnt=sum(quantity), .groups='drop') %>% 
  pivot_wider(names_from=category_id, values_from=cnt, values_fill=0) %>% 
  select('customer_id', '1','2','3','4','5','6','7')

#Obliczanie ile razy klient kupił najnowszy produkt
temp2<-df_orders %>%
  inner_join(df_order_items, by = 'order_id') %>%
  inner_join(df_products, by = 'product_id') %>%
  mutate(order_year = as.numeric(substr(order_date, 1, 4)),
         is_latest_product = if_else(order_year == model_year, 1, 0)) %>%
  group_by(customer_id) %>%
  summarise(latest_product_count = sum(is_latest_product * quantity), .groups = 'drop')

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- temp1 %>% 
  inner_join(temp2, by='customer_id')
 

### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_orders %>% 
  inner_join(df_order_items, by='order_id') %>%
  mutate(order_day=strftime(order_date, "%A"),
         expected_income=list_price*quantity,
         real_income=list_price*quantity*(1-discount)
         ) %>% 
  group_by(order_day, product_id) %>% 
  summarise(avg_discount=(1-sum(real_income)/sum(expected_income)))


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "WolkowskaPaula.rds")

