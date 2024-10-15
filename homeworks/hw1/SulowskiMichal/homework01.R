library(dplyr)
library(tidyr)
library(lubridate)

df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale 
# w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.
df_orders %>% 
  mutate(quarter=quarters(as.Date(order_date)),year=substring(order_date,1,4)) %>% 
  right_join(df_order_items,by='order_id')  %>%
  select(product_id,quantity,customer_id,quarter,year) %>% 
  left_join(df_customers,by='customer_id') %>% 
  select(product_id,quantity,quarter,year,state) %>% 
  group_by(year,quarter,state,product_id) %>% 
  summarize(counts=sum(quantity)) %>%
  group_by(year,quarter,state) %>% 
  filter(counts==max(counts)) %>% 
  left_join(df_products,by='product_id') %>% 
  select(year,quarter,state,product_name,model_year) -> answer1

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- answer1


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 
df_orders %>% 
  mutate(notcompleted=ifelse(order_status!=4,TRUE,FALSE),month=substring(order_date,6,7)) %>% 
  group_by(month) %>%
  summarise(total_orders = n(),
            uncompleted_orders = sum(order_status != 4),
            percent_uncompleted = (uncompleted_orders / total_orders) * 100) %>% 
  select(month,percent_uncompleted)->answer2
## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- answer2

####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
df_order_items %>% 
  mutate(price_after_discount=list_price*(1-discount),income=price_after_discount*quantity) %>% 
  left_join(df_orders,by='order_id') %>% 
  mutate(year=substring(order_date,1,4)) %>% 
  select(product_id,income,year) %>% 
  group_by(product_id, year) %>%
  summarise(total_income = sum(income, na.rm = TRUE)) %>% 
  group_by(year) %>%
  filter(total_income == max(total_income)) %>% 
  left_join(df_products,by='product_id') %>% 
  select(year,product_name) -> answer3
## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- answer3


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 
df_orders %>%
  mutate(order_year=substring(order_date,1,4)) %>% 
  group_by(customer_id,order_year) %>% 
  summarize(order_count=n(), .groups = 'drop') %>% 
  group_by(order_year) %>%                                      
  summarize(
    max_orders = max(order_count),                        
    customers_with_max = sum(order_count == max_orders) 
  ) ->answer4
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <-answer4

####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
df_orders %>%
  left_join(df_customers,by='customer_id') %>% 
  select(order_id,email,order_date) %>% 
  mutate(year=substring(order_date,1,4),domain = sub(".*@", "",email)) %>% 
  group_by(domain,year) %>% 
  summarize(orders_with_domain_count=n(),.groups = 'drop') %>% 
  group_by(year) %>% 
  filter(orders_with_domain_count == max(orders_with_domain_count)) %>%
  select(year, domain, orders_with_domain_count) %>%
  arrange(year)->answer5
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- answer5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

#### UWAGA:zakladam ze aktywnym klientem jest klient ktory zlozyl kiedykolwiek zamowienie###
df_orders %>% 
  left_join(df_customers,by='customer_id') %>% 
  select(customer_id,state) %>% 
  filter(state=='CA'|state=='TX') %>% 
  unique() %>% 
  group_by() %>% 
  summarize(active_customers_count_ca_tx = n())->answer6a

df_customers %>%
  filter(state %in% c("CA", "TX")) %>% 
  left_join(df_orders, by = "customer_id") %>% 
  group_by(customer_id, state) %>% 
  summarize(orders_in_2018 = sum(substring(order_date, 1, 4) == "2018", na.rm = TRUE), .groups = "drop") %>%
  filter(orders_in_2018 == 0) %>% 
  group_by() %>% 
  summarize(customers_with_no_order_in_2018_ca_tx = n())->answer6b
answer6<-cbind(answer6a,answer6b)
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- answer6


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
df_order_items %>% 
  mutate(final_price=(1-discount)*list_price*quantity) %>% 
  group_by(order_id) %>% 
  summarise(order_price=sum(final_price)) %>% 
  left_join(df_orders,by='order_id') %>% 
  select(order_id,customer_id,order_price)->df_with_order_price
quantiles<-df_with_order_price %>% 
  summarize(
    lower_quantile=quantile(order_price,0.05),
    upper_quantile=quantile(order_price,0.95)
  )
extreme_customers<-df_with_order_price %>% 
  filter(order_price<quantiles$lower_quantile | order_price>quantiles$upper_quantile) %>% 
  select(customer_id) %>% 
  unique() %>% 
  left_join(df_customers,by='customer_id') %>% 
  select(customer_id,first_name,last_name)

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- extreme_customers


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
df_orders %>% 
  mutate(order_date=as.Date(order_date),year=substring(order_date,1,4),quarter=quarters(as.Date(order_date))) %>% 
  group_by(order_date,quarter,year) %>% 
  summarise(orders_count=n(),.groups = 'drop') %>% 
  complete(order_date=seq(as.Date('2016-01-01'),as.Date('2018-12-31'),by='day'),
           fill=list(orders_count=0)) %>% 
  mutate(year=substring(order_date,1,4),quarter=quarters(as.Date(order_date))) %>% 
  group_by(year,quarter) %>% 
  summarize(max=max(orders_count),min=min(orders_count),median=median(orders_count)) %>% 
  select(year,quarter,max,min,median)->answer8
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- answer8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
df_orders %>% 
  left_join(df_customers,by='customer_id') %>%
  select(order_id,order_status,order_date,shipped_date,state) %>% 
  mutate(year=substring(order_date,1,4)) %>% 
  filter(order_status==4) %>% 
  mutate(days=as.numeric(as.Date(shipped_date)-as.Date(order_date))) %>% 
  select(year,state,days) %>% 
  group_by(year,state) %>% 
  summarise(mean_shipping_days=mean(days),.groups='drop') %>% 
  pivot_wider(names_from = 'state',values_from = mean_shipping_days)->answer9

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- answer9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
df_orders %>% 
  select(customer_id,order_date) %>% 
  mutate(year=substring(order_date,1,4)) %>% 
  select(customer_id,year) %>% 
  unique() %>% 
  group_by(customer_id) %>% 
  summarise(count=n()) %>% 
  filter(count==3) %>% 
  left_join(df_customers,by='customer_id') %>%
  select(last_name)->df_last_names

df_last_names %>% 
  transmute(first_letter=substring(last_name,1,1))->first_letter_of_special_last_names

df_last_names %>%
  mutate(all_letters = gsub("[^A-Za-z]", "", toupper(last_name))) %>%  
  pull(all_letters) %>%                                               
  paste(collapse = "") %>%                                             
  strsplit("") %>%                                                     
  unlist() %>%                                                         
  table() %>%                                                         
  as.data.frame()->how_often_every_letter
colnames(how_often_every_letter)=c("Letter","Count")
how_often_every_letter %>% 
  mutate(letter_of_customer_with_every_year_order=Letter %in% first_letter_of_special_last_names$first_letter)->answer10

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <-answer10

####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)
zestawienie <- df_orders %>%
  left_join(df_order_items, by = "order_id") %>%
  left_join(df_products, by = "product_id") %>%
  left_join(df_categories, by = "category_id") %>%
  left_join(df_customers, by = "customer_id")

zestawienie_szerokie <- zestawienie %>%
  group_by(customer_id, first_name, last_name, category_name) %>%
  summarise(liczba_zakupow = sum(quantity), .groups = "drop") %>%
  pivot_wider(names_from = category_name, values_from = liczba_zakupow, values_fill = list(liczba_zakupow = 0))

najnowszy_licznik <- zestawienie %>%
  filter(model_year == year(order_date)) %>%
  group_by(customer_id) %>%
  summarise(najnowszy_produkt = sum(quantity), .groups = "drop")

answer11 <- zestawienie_szerokie %>%
  left_join(najnowszy_licznik, by = "customer_id") %>%
  replace_na(list(najnowszy_licznik= 0))


## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- answer11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
df_order_items %>% 
  mutate(rabat=discount*100) %>% 
  left_join(df_orders,by='order_id') %>% 
  select(order_id,rabat,order_date) %>% 
  mutate(weekday=weekdays(as.Date(order_date))) %>% 
  select(rabat,weekday) %>% 
  group_by(weekday) %>% 
  summarise(average_discount=mean(rabat)) ->answer12
## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- answer12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "SulowskiMichal.rds")
