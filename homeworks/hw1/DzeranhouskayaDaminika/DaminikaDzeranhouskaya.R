
library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')
df_stores <- read.csv('homeworks/hw1/dane/stores.csv')


####### Zadanie 1
# Ktory produkt byl najczesciej kupowany w kazdym kwartale w podziale na stany z ktorych pochodza klienci? 
# Podaj nazwe produktu i rok jego produkcji.


  
df_1 <- df_orders %>% 
  mutate(month=as.integer(substr(order_date, 6,7))) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  inner_join(df_order_items, by = "order_id") %>% 
  select(customer_id, month, state, product_id) %>% 
  mutate(quarter=case_when(
    month<=3~1,
    month<=6~2,
    month<=9~3,
    TRUE~4
  )) %>% 
  select(customer_id, quarter, state, product_id) %>% 
  group_by(quarter, state, product_id) %>% 
  summarise(count = n()) %>% 
  group_by(quarter, state) %>% 
  filter(count == max(count)) %>% 
  inner_join(df_products, by = "product_id") %>% 
  select(quarter,state, product_name, model_year)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_1


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie zostal zrealizowany w kazdym miesiacu? 

df_2 <- df_orders %>% 
  mutate(month=as.integer(substr(order_date, 6,7)), 
         not_shipped = ifelse(shipped_date=="NULL",1,0)) %>% 
  group_by(month) %>% 
  summarise(count=n(),not_shipped_count=sum(not_shipped)) %>% 
  mutate(not_delieverd_pers = paste(round(not_shipped_count/count * 100,2), "%", sep="")) %>% 
  select(month,not_delieverd_pers)
  
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_2


####### Zadanie 3
# Jaki produkt przyniosl najwiekszy przychod w kazdym roku?


df_3 <- df_orders %>% 
  inner_join(df_order_items, by="order_id") %>% 
  mutate(year = as.integer(substr(order_date, 1,4)),
         revenue=list_price*(1-discount)
         ) %>% 
  select(year, product_id, revenue) %>% 
  group_by(year, product_id) %>% 
  summarise(total_revenue = sum(revenue)) %>% 
  group_by(year) %>% 
  filter(total_revenue==max(total_revenue)) %>% 
  inner_join(df_products, by="product_id") %>% 
  select(year, product_name)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_3


####### Zadanie 4
## Ile klientow zrobilo najwieksze zakupy (czyli zrobili najwiecej zamowien) w kazdym roku i ile to bylo zamowien? 

df_4 <- df_orders %>% 
  mutate(year = as.integer(substr(order_date, 1,4))) %>% 
  group_by(year, customer_id) %>% 
  summarise(order_count = n()) %>% 
  group_by(year) %>% 
  filter(order_count==max(order_count)) %>% 
  group_by(year, order_count) %>% 
  summarise(customer_count = n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_4


####### Zadanie 5
# Z jakiej domeny mailowej najczesciej robiono zamowienia w kazdym roku? Ile bylo tych zamowien?

df_5 <- df_orders %>% 
  inner_join(df_customers) %>% 
  mutate(domain=sub(".*@(.*)\\.com", "\\1", email)) %>% 
           mutate(year = as.integer(substr(order_date, 1,4))) %>%
  group_by(year, domain) %>% 
  summarise(count=n()) %>% 
  select(year, domain, count) %>% 
  group_by(year) %>% 
  filter(count==max(count))

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientow z tych stanow sa tacy, ktorzy nie zrobili zadnego zamowienia w 2018?

df_6 <- df_orders %>% 
  inner_join(df_customers) %>% 
  filter(state=="CA" | state=="TX") %>% 
  mutate(year = substr(order_date, 1,4),
         didordered_2018=ifelse(year=="2018",1,0)) %>%
  group_by(customer_id) %>% 
  summarise(did_not_ordered_2018=sum(didordered_2018)) %>% 
  filter(did_not_ordered_2018==0)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- df_6


####### Zadanie 7
## Ktorzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamowieniu ponizej 5 kwantyla lub wiecej niz 95 kwantyl wartosci zamowienia?
df77 <- df_orders %>% 
  inner_join(df_order_items) %>% 
  mutate(money_spent=list_price*(1-discount))
  
  
quantile_5 <- quantile(df77$money_spent, 0.05)
quantile_95 <- quantile(df77$money_spent, 0.95)

df_7 <- df77 %>% 
  filter(money_spent<quantile_5 | money_spent>quantile_95) %>% 
  select(customer_id)

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_7


####### Zadanie 8
# Oblicz jaka byla maksymalna i minimalna oraz mediana liczby zamowien zlozonych kazdego dnia w poszczegolnych kwartalach.

df_8 <-  df_orders %>% 
  mutate(order_month=as.integer(substr(order_date, 6,7)),
         order_day=as.integer(substr(order_date, 9,10))) %>% 
  mutate(order_quarter=case_when(
    order_month<=3~1,
    order_month<=6~2,
    order_month<=9~3,
    TRUE~4
  )) %>%
  group_by(order_quarter, order_month, order_day) %>% 
  summarise(order_count=n()) %>% 
  group_by(order_quarter) %>% 
  summarise(
    min = min(order_count),
    max = max(order_count),
    mediana = median(order_count)
  )
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_8



####### Zadanie 9 
# Jaki byl sredni czas dostarczania zamowienia w zaleznosci od roku i  stanu w ktorym mieszkal klient. 
# Jako rozwiazanie przygotuj szeroka postac tabeli, ktora bedzie miala informacje o kazdym stanie w innej kolumnie

df_9 <- df_orders %>% 
  mutate(delivery_time = as.Date(shipped_date)-as.Date(order_date)) %>% 
  mutate(year = as.integer(substr(order_date, 1,4))) %>% 
  inner_join(df_customers) %>% 
  group_by(state, year) %>% 
  summarise(average_del_time=mean(delivery_time, na.rm = TRUE)) %>% 
  pivot_wider(names_from = state, values_from = average_del_time)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_9


####### Zadanie 10
# Od jakich liter zaczynaja sie nazwiska klientow, ktorzy robili zamowienia co roku. 
# Przeanalizuj jak czesto wystepuje kazda litera wsrod tych nazwisk.

df_10 <- df_orders %>% 
  mutate(year = as.integer(substr(order_date, 1,4))) %>% 
  group_by(customer_id,year) %>% 
  select(customer_id, year) %>% 
  group_by(customer_id) %>% 
  summarise(num_of_years_active = n()) %>% 
  filter(num_of_years_active==3) %>% 
  inner_join(df_customers) %>% 
  mutate(first_letter=substr(last_name,1,1)) %>% 
  group_by(first_letter) %>% 
  summarise(letter_count=n())
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_10


####### Zadanie 11
# Zrob zestawienie (szeroka postac tabeli) ile razy kazdy klient kupil rower kazdej kategorii. 
# Jesli nie kupowal takiego roweru zaraportuj wartosc"0"
# Dodaj do zestawienia informacje ile razy klient kupowal najnowszy produkt (rower zostal wyprodukowany w tym roku, kiedy zlozono zamowienie)

df_11 <- df_orders %>% 
  mutate(order_year = substr(order_date, 1,4)) %>% 
  inner_join(df_order_items) %>% 
  inner_join(df_products) %>% 
  mutate(is_new=ifelse(order_year==model_year,1,0)) %>% 
  inner_join(df_categories) %>% 
  select(is_new, category_name, customer_id)

df_p <- df_11 %>% 
  group_by(customer_id, category_name) %>% 
  summarise(category_count=n()) %>% 
  pivot_wider(names_from = category_name, values_from = category_count, values_fill = 0) 
  
df_is_nw <- df_11 %>% 
  group_by(customer_id) %>% 
  summarise(new=sum(is_new))

df11 <- df_is_nw %>% 
  inner_join(df_p, by="customer_id")

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df11


### Zadanie 12
# Jaki byl sredni rabat udzielony na kazdy produkt w kazdym dniu tygodnia?
# Jako sredni rabat rozumiemy roznice procentowa miedzy przychodem wynikajacym z ceny katalogowej a przychodem faktycznym uwzgledniajacym udzielony rabat

df_12 <- df_orders %>% 
  inner_join(df_order_items) %>% 
  mutate(day_of_week = weekdays(as.Date(order_date))) %>% 
  inner_join(df_products) %>% 
  group_by(product_name, day_of_week) %>% 
  summarise(average_discount=mean(discount)) %>% 
  mutate(average_discount = paste(round(average_discount * 100,2), "%", sep=""))
  
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_12



### Zapisanie rozwiazan do pliku .RDS

### Prosze nic nie zmieniac 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Prosze zmienic tylko nazwe pliku w komendzie saveRDS na swoje Nazwisko i Imie (bez polskich znakow)
saveRDS(solutions, file = "DzeranhouskayaDaminika.rds")
