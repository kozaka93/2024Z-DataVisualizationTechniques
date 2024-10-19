library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
df_merged <- left_join(df_order_items, df_orders  , by="order_id" )%>%
  select(-c(list_price, discount, order_status, required_date,
            shipped_date, )) %>% 
  left_join(df_customers, by ="customer_id")%>%
  select(-c(order_id, store_id, staff_id, first_name, last_name, phone, email,
            street, city, zip_code,item_id,customer_id))%>%
  mutate(order_date = as.Date(order_date))%>%
  mutate(year = format(order_date,"%Y"), quarter = quarters(order_date))%>%
  select(-c(order_date))%>%
  group_by(year,quarter, state, product_id)%>%
  summarise(sum_of_ordered_products = sum(quantity))

temp <- df_merged %>% group_by(year, quarter, state) %>% 
  summarise(max_sales =max(sum_of_ordered_products))

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_merged %>% left_join(temp, by=c("year","quarter","state")) %>%
  filter(sum_of_ordered_products == max_sales)%>%
  group_by(year, quarter, state)%>% filter(row_number()==1)%>%
  left_join(df_products,by="product_id")%>%
  select(-c(product_id, sum_of_ordered_products, max_sales, brand_id,
            category_id, list_price))


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

df_orders_with_month_year <- df_orders%>%
  mutate(order_date = as.Date(order_date))%>%
  mutate(order_month = format(order_date,"%m"),
         order_year = format(order_date,"%Y"))
         
df_sum_orders<- df_orders_with_month_year %>% 
  group_by(order_year, order_month) %>%
  summarise(number_of_orders = n())

df_not_fufilled_orders<- df_orders_with_month_year%>%
  filter(shipped_date=="NULL")%>%
  group_by(order_year, order_month)%>%
  summarise(number_of_orders_unfulfilled = n()) 

df_final_zadanie_2 <- df_sum_orders %>% 
  left_join(df_not_fufilled_orders, by = c("order_year", "order_month"))
df_final_zadanie_2$number_of_orders_unfulfilled[is.na(df_final_zadanie_2$number_of_orders_unfulfilled)]=0



## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_final_zadanie_2 %>%
  mutate(percentage_of_unfulfilled_orders = 
         paste(round(number_of_orders_unfulfilled * 100/ number_of_orders,2),"%",sep=""))%>%
  select(-c(number_of_orders, number_of_orders_unfulfilled))%>%
  rename(Year=order_year, Month = order_month, 
         unfulfillment_percentage = percentage_of_unfulfilled_orders)


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?



## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <-  df_order_items %>% left_join(df_orders, by="order_id") %>%
  mutate(order_year = format(as.Date(order_date), "%Y"), 
         paid_price = round((1-discount) * list_price*quantity, digits=2))%>%
  group_by(order_year, product_id) %>% 
  summarise(revenue = sum(paid_price))%>%
  group_by(order_year)%>%
  mutate(highest_rev_in_year=max(revenue))%>%
  filter(revenue == highest_rev_in_year)%>%
  select(-revenue)%>%
  rename(highest_revenue = highest_rev_in_year)


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% 
  mutate(order_year = format(as.Date(order_date),"%Y"))%>%
  group_by(order_year, customer_id) %>% 
  summarise(number_of_orders = n())%>%
  group_by(order_year) %>%
  arrange(desc(number_of_orders),.by_group = TRUE)%>%
  mutate(max_number = max(number_of_orders)) %>%
  filter(number_of_orders ==max_number)%>% 
  mutate(number_of_customers = n())%>%
  filter(row_number()==1)%>%
  select(-c(customer_id,max_number))


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
df_domain <- df_orders %>% left_join(df_customers,by="customer_id")%>%
  mutate(domain = strsplit(email,split = "@"))
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_domain%>% 
  mutate(domain = sapply(df_domain$domain,'[',2))%>%
  mutate(order_year = format(as.Date(order_date),"%Y"))%>%
  group_by(order_year, domain) %>% 
  summarise(number_of_orders = n())%>% 
  group_by(order_year)%>%
  arrange(desc(number_of_orders),.by_group=TRUE )%>%
  filter(row_number()==1)


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?
df_6 <- df_orders %>% left_join(df_customers, by='customer_id')%>%
  filter(state=="CA" | state=="TX") %>%
  group_by(customer_id) %>% 
  filter(row_number()==1) %>%
  group_by(state)%>%
  summarise(number_of_active_customers = n())
  
df_6_2018 <- df_orders %>% left_join(df_customers, by='customer_id')%>%
  filter(state=="CA" | state == "TX") %>% 
  mutate(order_year = format(as.Date(order_date),"%Y"))%>%
  mutate(if_ordered_in_2018 = case_when(order_year=="2018" ~ TRUE,
                                        .default = FALSE
                                        ))%>%
  group_by(customer_id,state)%>%
  summarise(if_ordered_in_2018 = any(if_ordered_in_2018)) %>%
  group_by(state)%>%
  summarise(if_not_ordered_in_2018 = any(!if_ordered_in_2018))
  


## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- inner_join(df_6,df_6_2018,by='state')


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
df_7 <- df_order_items %>% left_join(df_orders, by='order_id') %>%
  left_join(df_customers,by='customer_id')%>%
  mutate(price_paid = round((1-discount)*list_price*quantity,digits=2))%>%
  group_by(order_id,customer_id, first_name, last_name)%>%
  summarise(price_of_order = sum(price_paid))
  
  quantile_5 = unname(quantile(df_7$price_of_order,0.05))
  quantile_95= unname(quantile(df_7$price_of_order,0.95))

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_7%>% 
  filter(price_of_order < quantile_5 | price_of_order > quantile_95)%>%
  ungroup()%>% select(-order_id,-price_of_order)


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
df_8_number_of_orders_daily <- df_orders %>% group_by(order_date) %>%
  summarise(number_of_orders=n())%>% 
  mutate(order_date=as.Date(order_date))
  
min_date = min(df_8_number_of_orders_daily$order_date)
max_date = max(df_8_number_of_orders_daily$order_date)
df_8_all_dates <- data.frame(order_date = seq(min_date, max_date,by="days"))

df_8_temp <- left_join(df_8_all_dates,df_8_number_of_orders_daily,
                       by='order_date')%>%
  mutate(Year = format(order_date,"%Y"), quarter = quarters(order_date))

df_8_temp$number_of_orders[is.na(df_8_temp$number_of_orders)] = 0
  
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_8_temp %>% group_by(Year, quarter)%>%
  summarise(max_orders_daily = max(number_of_orders),
            min_orders_daily = min(number_of_orders),
            median_orders_daily = median(number_of_orders)) 
  

####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
df_9 <- df_orders %>% left_join(df_customers, by = 'customer_id') %>%
  mutate(order_year = format(as.Date(order_date),"%Y") ) %>%
  filter(shipped_date!="NULL")%>%
  mutate(delivery_time = as.Date(shipped_date) - as.Date(order_date))%>%
  group_by(order_year, state)%>%
  summarise(mean_delivery_time = mean(delivery_time))%>%
  rename(year = order_year)

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_9 %>% 
  pivot_wider(names_from = state, values_from = mean_delivery_time)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
df_10<- df_orders %>% left_join(df_customers, by='customer_id')%>%
  mutate(order_year = format(as.Date(order_date),"%Y"))%>%
  group_by(customer_id,last_name)%>% 
  summarise(if_ordered_every_year = all(c( any(order_year=="2016"),
                                           any(order_year=="2017"),
                                           any(order_year=="2018") )) )%>%
  filter(if_ordered_every_year)%>%
  mutate(first_letter_Last_Name = substring(last_name,1,1))

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- data.frame(table(df_10$first_letter_Last_Name))%>%
  rename(First_letter_of_last_name = Var1, Number_of_last_names = Freq)


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)
df_11 <- df_orders %>% left_join(df_order_items, by = "order_id") %>%
  left_join(df_products, by ="product_id") %>% 
  group_by(customer_id, category_id) %>% 
  summarise(number_of_purchased_bikes = sum(quantity))%>%
  left_join(df_categories, by='category_id')%>%
  ungroup%>%
  select(customer_id, category_name, number_of_purchased_bikes)%>%
  pivot_wider(names_from = customer_id, values_from = number_of_purchased_bikes)
  
df_11_2 <- df_orders %>% left_join(df_order_items, by = "order_id") %>%
  left_join(df_products, by ="product_id") %>%
  mutate(order_year = strtoi( format(as.Date(order_date),"%Y") )) %>%
  filter(order_year==model_year)%>%
  group_by(customer_id)%>%
  summarise(number_of_newest_purchased_bikes = sum(quantity))%>%
  right_join(df_customers,by='customer_id')

df_11_2$number_of_newest_purchased_bikes[is.na(df_11_2$number_of_newest_purchased_bikes)]=0

temp <- df_11_2 %>% select(customer_id,number_of_newest_purchased_bikes)%>%
  mutate(category_name = "new bikes")%>%
  pivot_wider(names_from = customer_id, values_from = number_of_newest_purchased_bikes)

df_11<-df_11 %>% add_row(temp)

df_11[is.na(df_11)]=0

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
df_12 <- df_orders %>% left_join(df_order_items, by='order_id')%>%
  mutate(order_weekday = weekdays(as.Date(order_date)))%>%
  mutate(discount = discount*100)%>%
  group_by(product_id, order_weekday)%>%
  summarise(mean_discount = mean(discount))%>%
  left_join(df_products,by='product_id')%>%
  ungroup()%>%
  select(product_name, order_weekday, mean_discount)%>%
  mutate(mean_discount = paste(round(mean_discount,2), "%", sep = ""))
  
    
## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "NazwiskoImie.rds")
