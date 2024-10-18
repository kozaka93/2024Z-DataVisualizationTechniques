library(dplyr)
library(tidyr)
library(lubridate)





# df_orders <- read.csv('C:/Users/Stefan/Desktop/homeworks/hw1/dane/orders.csv')
# df_order_items <- read.csv('C:/Users/Stefan/Desktop/homeworks/hw1/dane/order_items.csv')
# df_products <- read.csv('C:/Users/Stefan/Desktop/homeworks/hw1/dane/products.csv')
# df_brands <- read.csv('C:/Users/Stefan/Desktop/homeworks/hw1/dane/brands.csv')
# df_categories <-  read.csv('C:/Users/Stefan/Desktop/homeworks/hw1/dane/categories.csv')
# df_customers <- read.csv('C:/Users/Stefan/Desktop/homeworks/hw1/dane/customers.csv')
# df_products <- read.csv('C:/Users/Stefan/Desktop/homeworks/hw1/dane/products.csv')
# df_stocks <- read.csv('C:/Users/Stefan/Desktop/homeworks/hw1/dane/stocks.csv')
# df_stores <- read.csv('C:/Users/Stefan/Desktop/homeworks/hw1/dane/stores.csv')

df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_stocks <- read.csv('homeworks/hw1/dane/stocks.csv')
df_stores <- read.csv('homeworks/hw1/dane/stores.csv')




####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
ANS_TASK_01<-df_order_items %>% 
  left_join(df_orders,by="order_id") %>% 
  left_join(df_customers,by="customer_id") %>% 
  select(c("order_id","product_id","quantity","order_date","state")) %>%
  mutate(quarter=quarters(as.Date(order_date))) %>% 
  mutate(year=year(as.Date(order_date))) %>% 
  group_by(quarter,state,product_id,year) %>% 
  summarize(total_quantity = sum(quantity), .groups = 'drop') %>%
  group_by(quarter, state,year) %>% 
  slice_max(order_by = total_quantity, n = 1, with_ties = FALSE) %>% 
  left_join(df_products, by = "product_id") %>%
  select(quarter,year, state,product_name, model_year)  
  
  


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% 
  mutate(month=month(as.Date(order_date))) %>% 
  group_by(month) %>% 
  summarize(
    not_completed_percent = (sum(order_status != 4) / n()) * 100
  )
  
  


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>%
  left_join(df_orders,by="order_id") %>% 
  mutate(year=year(as.Date(order_date))) %>% 
  group_by(year,product_id) %>% 
  summarize(final_sum=sum(quantity*list_price*(1-discount))) %>% 
  slice_max(order_by = final_sum, n = 1, with_ties = FALSE) %>% 
  left_join(df_products, by = "product_id") %>%
  select(year,product_name)  


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% 
  mutate(year=year(as.Date(order_date))) %>% 
  group_by(year,customer_id) %>% 
  summarize(orders_number=n(),.groups = 'drop') %>% 
  group_by(year) %>% 
  summarize(max_orders_number=max(orders_number),how_many_clients=sum(orders_number==max_orders_number))


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_orders %>% 
  mutate(year=year(as.Date(order_date))) %>% 
  left_join(df_customers,by="customer_id") %>% 
  mutate(domain = sub(".*@", "", email)) %>% 
  group_by(year,domain) %>% 
  summarize(orders_number=n()) %>% 
  filter(orders_number == max(orders_number))
  

####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej

###Najpierw liczę ilu jest aktywnych klientów którzy zrobili zamówienia w 2018
last_year_customers<-df_orders %>% 
  left_join(df_customers,by="customer_id") %>% 
  mutate(year=year(as.Date(order_date))) %>% 
  filter(year==2018) %>% 
  filter(state=="CA" | state=="TX") %>% 
  summarize(unique_customers=n_distinct(customer_id)) %>% 
  pull(unique_customers)

ANS_TASK_06 <- df_orders %>% 
  left_join(df_customers,by="customer_id") %>% 
  filter(state=="CA" | state=="TX") %>% 
  summarize(unique_customers=n_distinct(customer_id)) %>% 
  mutate(not_ordering_in_2018=unique_customers-last_year_customers)

####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_orders %>% 
  left_join(df_order_items,by="order_id") %>% 
  group_by(order_id,customer_id) %>% 
  summarize(money_spent=sum(quantity*list_price*(1-discount))) %>% 
  ungroup() %>%
  mutate(
    lower_quantile = quantile(money_spent, 0.05),
    upper_quantile = quantile(money_spent, 0.95),
    is_extreme_customer = money_spent < lower_quantile | money_spent > upper_quantile
  ) %>%
  select(customer_id, is_extreme_customer) %>% 
  filter(is_extreme_customer==TRUE) %>% 
  arrange(customer_id)
####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_orders %>% 
  mutate(order_date = as.Date(order_date, format = "%Y-%m-%d")) %>%
  group_by(order_date) %>% 
  mutate(orders_number=n()) %>% 
  ungroup() %>% 
  complete(order_date = seq.Date(min(order_date), max(order_date), by = "day"),
           fill = list(orders_number = 0)) %>%
  mutate(week_day=wday(order_date),
         quarter=quarters(order_date),
         year=year(order_date)
  ) %>% 
  group_by(year,quarter,week_day) %>% 
  summarize(orders_number_max=max(orders_number),
            orders_number_min=min(orders_number),
            orders_number_med=median(orders_number))
  

####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_orders %>% 
  left_join(df_customers,by="customer_id") %>% 
  mutate(year=year(as.Date(order_date)),
         shipping_time_in_days=as.numeric(as.Date(shipped_date)-as.Date(order_date))
         ) %>% 
  group_by(year,state) %>% 
  ###Obliczamy średni czas dostarczania zamówienia - nie wliczamy paczek które nie zostały dostarczone
  summarize(mean_shipping_time=mean(shipping_time_in_days,na.rm=TRUE)) %>% 
  spread(state,mean_shipping_time)
  

####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_orders %>% 
  left_join(df_customers,by="customer_id") %>% 
  mutate(year=year(as.Date(order_date))) %>% 
  mutate(earliest_date = min(year),        
         latest_date = max(year),  
         every_year = latest_date-earliest_date+1
  ) %>% 
  select(-earliest_date,-latest_date) %>% 
  group_by(customer_id,last_name,every_year) %>% 
  summarize(years_with_orders = sapply(list(unique(year)),length), .groups = 'drop',
  ) %>% 
  filter(years_with_orders==every_year) %>% 
  mutate(first_letter=substring(last_name,1,1)) %>% 
  group_by(first_letter) %>% 
  summarize(how_often=n())
  

  


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

#Najpierw stworzymy tabelę z dodatkową informacją dla każdego klienta
how_often_newest<-df_order_items %>% 
  left_join(df_orders,by='order_id') %>% 
  left_join(df_customers, by='customer_id') %>% 
  left_join(df_products, by='product_id') %>% 
  group_by(customer_id) %>% 
  summarize(how_often_newest=0+sum(year(as.Date(order_date))==model_year))
## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_order_items %>% 
  left_join(df_orders,by='order_id') %>% 
  left_join(df_customers, by='customer_id') %>% 
  left_join(df_products, by='product_id') %>% 
  group_by(customer_id,category_id) %>% 
  summarize(how_often_this_category=n(),.groups='drop') %>% 
  complete(customer_id, category_id, fill = list(how_often_this_category = 0)) %>% 
  spread(category_id,how_often_this_category) %>% 
  right_join(how_often_newest,by='customer_id')
  

### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_orders %>% 
  left_join(df_order_items,by="order_id") %>% 
  mutate(week_day=wday(as.Date(order_date))) %>% 
  group_by(week_day,product_id) %>% 
  mutate(money_really_spent=quantity*list_price*(1-discount),
         money_catalog=quantity*list_price) %>% 
  summarize(mean_discount=mean((money_catalog-money_really_spent)/money_catalog*100))
  



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "GajdaStefan.rds")






