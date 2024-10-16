library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers<-read.csv('homeworks/hw1/dane/customers.csv')
df_staff<-read.csv('homeworks/hw1/dane/staffs.csv')
df_stocks<-read.csv('homeworks/hw1/dane/stocks.csv')
df_stores<-read.csv('homeworks/hw1/dane/stores.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci?

# Podaj nazwę produktu i rok jego produkcji.

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- inner_join(select(df_orders,order_id,customer_id,order_date),select(df_customers,customer_id,state),by="customer_id") %>% 
  inner_join(select(df_order_items,order_id,product_id,quantity),by="order_id") %>% 
  inner_join(select(df_products,product_id,product_name,model_year),by="product_id") %>% 
  mutate(quarter=case_when(as.double(strftime(order_date,"%m"))<=3~"1",(as.double(strftime(order_date,"%m")))<=6~"2",(as.double(strftime(order_date,"%m")))<=9~"3",TRUE~"4")) %>% 
  group_by(year=strftime(order_date,"%Y"),quarter,state,product_id,product_name,model_year) %>% summarise(count=sum(quantity)) %>% 
  ungroup() %>% 
  group_by(year,quarter,state,) %>% 
  filter(count==max(count)) %>% 
  ungroup() %>% 
  select(year,quarter,state,product_name,model_year)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% mutate(is_completed=ifelse(order_status==4,"Yes","No")) %>% 
  group_by(year=strftime(order_date,"%Y"),month=strftime(order_date,"%m")) %>% 
  summarise(perc_uncompleted=length(which(is_completed=="No"))/length(is_completed)*100)


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <-inner_join(inner_join(select(df_orders,order_id,order_date),df_order_items,by="order_id"),select(df_products,product_id,product_name),by="product_id" ) %>% 
  mutate(cost=quantity*list_price*(1-discount)) %>% 
  group_by(year=strftime(order_date,"%Y"),product_name) %>%
  summarise(total_income=sum(cost)) %>%
  filter(total_income==max(total_income)) %>% ungroup()


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_orders %>% 
  group_by(year=strftime(order_date,"%Y"),customer_id) %>%
  summarise(number_of_orders=n()) %>% 
  filter(number_of_orders==max(number_of_orders)) %>% 
  select(year,number_of_orders) %>% 
  group_by(year,most_orders=number_of_orders) %>% 
  summarise(number_of_customers=n())


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- inner_join(select(df_orders,order_id,order_date,customer_id),select(df_customers,customer_id,email),by="customer_id") %>% 
  mutate(domain=sub(".*@", "", email)) %>% 
  group_by(year=strftime(order_date,"%Y"),domain) %>% 
  summarise(number_of_orders=n()) %>% 
  filter(number_of_orders==max(number_of_orders)) %>% 
  ungroup()


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- left_join(select(df_orders,customer_id,order_date),select(df_customers,customer_id,state),by="customer_id") %>% 
  filter(state=="CA"|state=="TX") %>% 
  group_by(state,customer_id) %>% 
  summarise(count=n(),count_in_2018=length(which(strftime(order_date,"%Y")==2018))) %>% 
  group_by(state) %>% 
  summarise(number_of_cutomers=n(),no_orders_in_2018=sum(count_in_2018==0))


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- inner_join(select(df_orders,order_id,customer_id),select(df_order_items,order_id,quantity,list_price,discount),by="order_id") %>% 
  inner_join(select(df_customers,customer_id,first_name,last_name),by="customer_id") %>% 
  group_by(customer_id,first_name,last_name,order_id) %>% summarise(cost=sum(quantity*list_price*(1-discount))) %>%
  ungroup() %>% 
  filter(cost< quantile(cost,0.05)|cost>quantile(cost,0.95)) %>% 
  select(customer_id,first_name,last_name) %>% 
  distinct()


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- select(df_orders,order_id,order_date) %>% 
  mutate(quarter=case_when(as.double(strftime(order_date,"%m"))<=3~"1",(as.double(strftime(order_date,"%m")))<=6~"2",(as.double(strftime(order_date,"%m")))<=9~"3",TRUE~"4")) %>% 
  group_by(order_date,quarter) %>% 
  summarise(number_of_orders=n()) %>% 
  group_by(year=strftime(order_date,"%Y"),quarter) %>% 
  summarise(max=max(number_of_orders),min=min(number_of_orders),median=median(number_of_orders))


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- inner_join(select(df_orders,customer_id,order_date,shipped_date),select(df_customers,customer_id,state),by="customer_id") %>% 
  mutate(delivery_time=as.Date(shipped_date)-as.Date(order_date)) %>% 
  transform(year=strftime(order_date,"%Y")) %>% 
  select(year,state,delivery_time,) %>% 
  filter(!delivery_time=="NA days") %>% 
  group_by(year,state) %>% 
  summarise(mean_time=mean(delivery_time)) %>% 
  pivot_wider(names_from = state,values_from=mean_time)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- inner_join(select(df_orders,order_id,customer_id,order_date),select(df_customers,customer_id,last_name),by="customer_id") %>% 
  group_by(customer_id,last_name,year=strftime(order_date,"%Y")) %>% 
  summarise(n=n()) %>% 
  group_by(customer_id,last_name) %>% 
  summarise(n=n()) %>% 
  filter(n==3) %>% 
  mutate(letter=substring(last_name,1,1)) %>% 
  group_by(letter) %>% 
  summarise(count=n())


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)
tmp1<-inner_join(select(df_orders,order_id,customer_id,order_date),select(df_order_items,order_id,product_id,quantity),by="order_id") %>%
  inner_join(select(df_products,product_id,category_id,model_year),by="product_id") %>% 
  inner_join(select(df_categories,category_id,category_name),by="category_id") %>% 
  select(customer_id,order_date,model_year,category_name,quantity)
tmp2<-tmp1 %>% 
  group_by(customer_id) %>% 
  mutate(year=strftime(order_date,"%Y")) %>% 
  summarise(number_of_newest=sum((model_year==year)*quantity))
tmp1<-tmp1 %>% 
  group_by(category_name,customer_id) %>% 
  summarise(count=sum(quantity)) %>% 
  ungroup() %>% 
  group_by(customer_id) %>% 
  pivot_wider(names_from = category_name,values_from =count,values_fill = 0)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- inner_join(tmp1,tmp2,by="customer_id")
### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- inner_join(select(df_orders,order_id,order_date),select(df_order_items,order_id,product_id,list_price,quantity,discount),by="order_id") %>% 
  mutate(day=strftime(order_date,"%A"),expected_income=list_price*quantity,real_income=list_price*quantity*(1-discount)) %>% 
  group_by(product_id,day) %>% 
  summarise(average_discount=(1-sum(real_income)/sum(expected_income)))



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "ZalewskaZuzanna.rds")
