library(dplyr)
library(tidyr)
library(stringr)

df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')
# 
# df_orders <- read.csv('../orders.csv')
# df_order_items <- read.csv('../order_items.csv')
# df_products <- read.csv('../products.csv')
# df_brands <- read.csv('../brands.csv')
# df_categories <-  read.csv('../categories.csv')
# df_customers <- read.csv('../customers.csv')
# df_staffs <- read.csv('../staffs.csv')
# df_stocks <- read.csv('../stocks.csv')
# df_stores <- read.csv('../stores.csv')
####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.


df1<- inner_join(df_orders,df_customers, by="customer_id") %>% 
  inner_join(df_order_items) %>% 
  inner_join(df_products,by=c("product_id")) %>% 
  mutate(year=format(as.Date.character(order_date),"%Y"),quarter=ceiling(as.numeric(format(as.Date.character(order_date),"%m"))/3)) %>% 
  select(state,year,quarter,product_name,model_year) %>%
  group_by(quarter,year,state,product_name,model_year)%>% 
  summarise(count=n()) %>% 
  group_by(year,quarter,state) %>% 
  summarise(product_name,model_year,count,max_count=max(count)) %>% 
  filter(max_count==count) %>% 
  select(-max_count)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df1


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 
new_orders<-df_orders %>% 
  mutate(year=format(as.Date.character(order_date),"%Y"),month=as.numeric(format(as.Date.character(order_date),"%m"))) %>% 
  group_by(year,month) %>% 
  summarise(all=n())
unrealized<-df_orders %>% 
  filter(order_status!=4) %>% 
  mutate(year=format(as.Date.character(order_date),"%Y"),month=as.numeric(format(as.Date.character(order_date),"%m"))) %>% 
  group_by(year,month) %>% 
  summarise(unrealized=n())
df2<-full_join(new_orders,unrealized,by=c("year","month")) %>% 
  mutate(percent_of_unrealized=unrealized/all*100) %>% 
  select(year,month,percent_of_unrealized)
df2$percent_of_unrealized[is.na(df2$percent_of_unrealized)]<-0
## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df2


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
df3<-df_orders %>% 
  inner_join(df_order_items) %>% 
  inner_join(df_products) %>% 
  mutate(year=format(as.Date.character(order_date),"%Y"),list_price=list_price*(1-discount)) %>% 
  group_by(year,product_name,list_price) %>% 
  summarise(count=n()) %>% 
  mutate(income=count*list_price) %>% 
  group_by(year) %>% 
  summarise(year,product_name,income,highiest_income=max(income)) %>% 
  filter(highiest_income==income) %>% 
  select(year,product_name,income)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df3


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 
df4<-df_orders %>% 
  mutate(year=format(as.Date.character(order_date),"%Y")) %>% 
  group_by(year,customer_id) %>% 
  summarize(number_of_orders=n()) %>% 
  summarise(customer_id,number_of_orders, highiest_number_of_orders=max(number_of_orders)) %>% 
  filter(number_of_orders==highiest_number_of_orders) %>% 
  group_by(year) %>% 
  summarise(number_of_biggest_orders=n(),highiest_number_of_orders=unique(highiest_number_of_orders))
  
  
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df4


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
get_domain <- function(email) {
   sub(".*@", "", email)
}

df5<- df_orders %>% 
  inner_join(df_customers) %>% 
  mutate(domain=get_domain(email),year=format(as.Date.character(order_date),"%Y")) %>% 
  group_by(year,domain) %>% 
  summarise(number_of_orders=n()) %>% 
  summarise(domain,number_of_orders, highiest_number_of_orders=max(number_of_orders)) %>% 
  filter(number_of_orders==highiest_number_of_orders)
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?
df6_1<- df_order_items %>% 
  inner_join(df_products) %>% 
  inner_join(df_brands) %>% 
  inner_join(df_orders) %>% 
  inner_join(df_customers) %>% 
  group_by(brand_name,state) %>% 
  summarise(number_of_customers=length(unique(customer_id))) %>% 
  filter(state == "CA" | state=="TX")

#odpowiedź dla poszczególnych firm według stanów, TRUE- jacyś klienci nie wykonali zamówienia w 2018, False - wszyscy klienci danej firmy wykonali zamówienie w 2018
df6_2<- df_order_items %>% 
  inner_join(df_products) %>% 
  inner_join(df_brands) %>% 
  inner_join(df_orders) %>% 
  inner_join(df_customers) %>%
  mutate(year=format(as.Date.character(order_date),"%Y")) %>% 
  group_by(brand_name,state,year) %>%
  summarise(number_of_customers_2018=length(unique(customer_id))) %>% 
  filter(year==2018,state == "CA" | state=="TX") %>% 
  inner_join(df6_1,by=c("brand_name","state")) %>% 
  mutate(any_customer_did_not_order_anything_in_2018=(number_of_customers_2018<number_of_customers)) %>% 
  select(brand_name,state,any_customer_did_not_order_anything_in_2018)
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- list(df6_1,df6_2)


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
df7<- df_order_items %>% 
  inner_join(df_products) %>% 
  inner_join(df_brands) %>% 
  inner_join(df_orders) %>% 
  mutate(list_price=list_price*(1-discount)) %>% 
  group_by(order_id,customer_id) %>% 
  summarise(total_order_cost=sum(quantity*list_price))
df7<- df7 %>% 
  filter(total_order_cost<=quantile(df7$total_order_cost,c(0.05)) | total_order_cost>=quantile(df7$total_order_cost,c(0.95))) %>% 
  inner_join(df_customers) %>% 
  ungroup() %>% 
  select(customer_id,first_name,last_name) %>% 
  distinct()
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
df8<-df_orders %>% 
  mutate(year=format(as.Date.character(order_date),"%Y"),quarter=ceiling(as.numeric(format(as.Date.character(order_date),"%m"))/3)) %>% 
  group_by(year,quarter,order_date) %>% 
  summarise(orders_count=n()) %>% 
  group_by(year,quarter) %>% 
  summarise(max_of_orders_daily=max(orders_count),min_of_orders_daily=min(orders_count),median_of_orders_daily=median(orders_count))
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
df9<-df_orders %>% 
  inner_join(df_customers) %>% 
  mutate(year=format(as.Date.character(order_date),"%Y"),time_difference_days=as.numeric(as.Date(shipped_date)-as.Date(order_date))) %>% 
  filter(!is.na(time_difference_days)) %>% 
  group_by(year,state) %>% 
  summarise(mean_time_difference_days=mean(time_difference_days)) %>% 
  pivot_wider(names_from = state,values_from = mean_time_difference_days)

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
df10<- df_orders %>% 
  mutate(year=format(as.Date.character(order_date),"%Y")) %>% 
  inner_join(df_customers) %>% 
  group_by(customer_id,last_name) %>%
  summarise(tmp=unique(year)) %>% 
  summarise(tmp=n()) %>% 
  filter(tmp==3) %>% 
  mutate(first_letters=substring(last_name,1,1)) %>% 
  group_by(first_letters) %>% 
  summarise(count=n())
  
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)
df11<-df_customers %>% 
  full_join(df_orders) %>% 
  full_join(df_order_items) %>% 
  full_join(df_products) %>% 
  mutate(year=format(as.Date.character(order_date),"%Y"),newest_quantity=case_when(
    year==model_year ~ quantity,
    year != model_year ~ 0)) %>% 
  select(customer_id,category_id,quantity,newest_quantity) %>% 
  inner_join(df_categories) %>% 
  group_by(customer_id,category_name) %>% 
  summarise(count=sum(quantity),newest=sum(newest_quantity))
newest<-select(df11,customer_id,newest) %>% 
  group_by(customer_id) %>% 
  summarise(newest=sum(newest))
df11<-pivot_wider(df11,names_from =category_name, id_cols =customer_id,values_from = count)
df11[is.na(df11)]<-0
df11<-inner_join(df11,newest)
## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df11

  
  

### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
df12<-df_orders %>% 
  inner_join(df_order_items) %>% 
  mutate(weekday=weekdays(as.Date(order_date))) %>% 
  group_by(product_id,weekday) %>% 
  summarise(mean_discount_percent=100*(sum(quantity*discount)/sum(quantity))) # %>% 
  #pivot_wider(df12,names_from=weekday,id_cols=product_id,values_from=mean_discount_percent) %>% 
  #select(product_id,
         # poniedziałek,
         # wtorek,
         # środa,
         # czwartek,
         # piątek,
         # sobota,
         # niedziela)
         # 
## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "KacprzakKarol.rds")

