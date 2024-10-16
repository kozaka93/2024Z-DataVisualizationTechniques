library(dplyr)
library(tidyr)


df_orders <- read.csv('orders.csv')
df_order_items <- read.csv('order_items.csv')
df_products <- read.csv('products.csv')
df_brands <- read.csv('brands.csv')
df_categories <-  read.csv('categories.csv')
df_staffs <-  read.csv('staffs.csv')
df_stocks <-  read.csv('stocks.csv')
df_stores <-  read.csv('stores.csv')
df_customers <- read.csv('customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

tab <- df_orders %>%
  left_join(df_order_items, by = "order_id") %>%
  left_join(df_products, by = "product_id") %>%
  left_join(df_customers, by = "customer_id") %>%
  mutate(year =as.numeric(substr(order_date, 1, 4)), month=as.numeric(substr(order_date, 6, 7))) %>%
  mutate(quarter = case_when(
    month %in% 1:3 ~ 1,
    month %in% 4:6 ~ 2,
    month %in% 7:9 ~ 3,
    month %in% 10:12 ~ 4
  )) %>%
  group_by(state, year, quarter, product_id, product_name, model_year) %>%
  summarise(amount = sum(quantity))


## Odpowiedz przypisana do zmiennej

ANS_TASK_01 <- tab %>%
  group_by(state, year, quarter) %>%
  top_n(1, amount) %>%
  select(year, state, quarter, product_name, amount)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

tab22 <- df_orders %>%
  select(order_date, shipped_date) %>%
  mutate(year = substr(order_date, 1, 4), month = substr(order_date, 6, 7), delivered = ifelse(shipped_date == "NULL", 'no', 'yes')
  ) %>%
  group_by(year, month) %>%
  summarise(total_orders = n(), undelivered_orders = sum(delivered == "no")) %>%
  group_by(year, month) %>%
  mutate(percentage=undelivered_orders/total_orders)

## Odpowiedz przypisana do zmiennej

ANS_TASK_02 <- tab22 %>%
select(year, month, percentage)

####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

tab3<-df_order_items %>%
  left_join(df_orders, by='order_id') %>%
  mutate(year = substr(order_date, 1, 4), income_from_sale=(1-discount)*list_price*quantity) %>%
  left_join(df_products, by='product_id') %>%
  select(year, product_name, product_id, income_from_sale) %>%
  group_by(year, product_id, product_name) %>%
  summarise(total_revenue = sum(income_from_sale), .groups = 'drop') %>%
  slice_max(total_revenue, by = year, n = 1)


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- tab3

####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

tab4 <- df_orders %>%
  left_join(df_customers, by ='customer_id') %>%
  mutate(year = substr(order_date, 1, 4)) %>% 
  select(order_id, customer_id, year) %>%
  group_by(customer_id, year) %>%
  summarise(order_count=n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(max_orders = max(order_count)) %>%  
  filter(order_count == max_orders) %>%
  group_by(year, max_orders) %>%
  summarise(customers_with_max_orders = n())



## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- tab4


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

tab5<- df_orders %>%
  left_join(df_customers, by = 'customer_id') %>%
  mutate(year = substr(order_date, 1, 4), domain=sub('.*@','',email)) %>% 
  select(customer_id, year, domain, order_id) %>%
  group_by(year, customer_id, domain) %>%
  summarise(usage_counter=n()) %>%
  select(year, domain, usage_counter) %>%
  ungroup() %>%
  group_by(year, domain) %>%
  summarise(all_orders=sum(usage_counter)) %>%
  group_by(year) %>%
  mutate(max_orders = max(all_orders)) %>%  
  filter(all_orders == max_orders) %>%
  select(year, domain, all_orders)


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- tab5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 

tab61<- df_orders %>%
  left_join(df_customers, by = 'customer_id') %>%
  filter(state=='CA' | state=='TX') %>%
  select(customer_id, order_id) %>%
  group_by(customer_id) %>%
  summarise(order_count=n()) %>%
  ungroup() %>%
  summarise(active_count=n())


# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

tab62<- df_orders %>%
  left_join(df_customers, by = 'customer_id') %>%
  filter((state=='CA' | state=='TX')) %>%
  mutate(year = substr(order_date, 1, 4)) %>%
  select(customer_id, order_id, year) %>%
  group_by(customer_id, year) %>%
  summarise(order_count=n()) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(active_count2=n()) %>%
  filter(year=='2018') %>%
  select(active_count2)

wynik6<-cbind(tab61, tab62) %>%
  mutate(not_active_2018=active_count-active_count2) %>%
  select(active_count, not_active_2018)


## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- wynik6


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

tab7 <- df_order_items %>% 
  inner_join(df_orders, by='order_id') %>%
  inner_join(df_customers, by='customer_id') %>%
  group_by(order_id, customer_id, first_name, last_name) %>%
  summarize(order_value=sum(quantity*list_price*(1-discount)), .groups = "drop") %>% 
  filter(order_value < quantile(order_value, 0.05) | order_value > quantile(order_value, 0.95)) %>% 
  select(customer_id, first_name, last_name) %>%
  distinct %>%
  arrange(customer_id)

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- tab7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

tab8<-df_orders %>%
  mutate(day_of_the_month=as.numeric(substr(order_date,9,10)), month=as.numeric(substr(order_date, 6, 7)), year=as.numeric(substr(order_date, 1, 4))) %>%
  mutate(quarter = case_when(
    month %in% 1:3 ~ 1,
    month %in% 4:6 ~ 2,
    month %in% 7:9 ~ 3,
    month %in% 10:12 ~ 4
  )) %>%
  group_by(day_of_the_month, month, year, quarter) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  select(day_of_the_month, month, year, count) %>%
  group_by(month, day_of_the_month) %>%
  mutate(max_orders = max(count), min_orders=min(count), median_orders=median(count)) %>%
  #filter(max_orders==count() | min_orders==count() | median_orders==count()) %>%
  select(max_orders, min_orders, median_orders, day_of_the_month, month)


## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- tab8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

tab9<-df_orders%>%
  left_join(df_customers,by="customer_id")%>%
  mutate(year=substr(order_date,1,4), date_diff=as.numeric(as.Date(shipped_date)-as.Date(order_date)))%>%
  group_by(state,year)%>%
  summarise(avg=mean(date_diff,na.rm=TRUE))%>%
  pivot_wider(names_from=state,values_from = avg)


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- tab9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.


tab10<-df_orders%>%
  left_join(df_customers,by="customer_id")%>%
  mutate(year=substr(order_date,1,4)) %>%
  select(customer_id, first_name, last_name, year) %>%
  group_by(customer_id, first_name, last_name) %>%
  summarise(year_counter=n()) %>%
  ungroup() %>%
  filter(year_counter==3) %>%
  mutate(first_letter=substr(last_name,1,1))

pom101 <- tab10 %>%
  select(first_letter)

pom102 <- tab10 %>%
  select(first_letter, last_name) %>%
  transmute(first_letter=tolower(first_letter), last_name=tolower(last_name)) %>%
  rowwise() %>%
  mutate(letter_count = sum(strsplit(last_name, "")[[1]] == first_letter)) %>%
  ungroup() %>%
  group_by(first_letter) %>%
  summarise(total_count = sum(letter_count)) 




## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- pom102


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

tab111<-df_orders%>%
  left_join(df_order_items,by="order_id")%>%
  left_join(df_customers,by="customer_id")%>%
  left_join(df_products,by="product_id") %>%
  group_by(customer_id, first_name, last_name, category_id) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  pivot_wider(names_from=category_id, values_from=count, values_fill=0) %>% 
  select('customer_id', 'first_name', 'last_name', '1','2','3','4','5','6','7')

tab112<-df_orders%>%
  left_join(df_order_items,by="order_id")%>%
  left_join(df_customers,by="customer_id")%>%
  left_join(df_products,by="product_id") %>%
  mutate(year=as.numeric(substr(order_date,1,4))) %>%
  group_by(customer_id) %>%
  summarise(latest_model_counter=sum(year==model_year))

tab111 <- tab111 %>%
  left_join(tab112, by='customer_id')


## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- tab111


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

tab12 <- df_orders %>%
  left_join(df_order_items, by='order_id') %>%
  mutate(order_day=strftime(order_date, "%A"),
         list_income=list_price*quantity,
         sale_income=list_price*quantity*(1-discount)
  ) %>% 
  group_by(order_day, product_id) %>% 
  summarise(avg_discount=(1-sum(sale_income)/sum(list_income)))

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- tab12

### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "BrandtAleksander.rds")