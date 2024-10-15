library(dplyr)
library(tidyr)


df_orders <- read.csv('orders.csv')
df_order_items <- read.csv('order_items.csv')
df_products <- read.csv('products.csv')
df_brands <- read.csv('brands.csv')
df_categories <-  read.csv('categories.csv')
df_customers<-read.csv("customers.csv")


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.


orders_1<-df_orders%>%
  inner_join(df_order_items, by="order_id")%>%
  inner_join(df_customers,by="customer_id")%>%
  inner_join(df_products,by="product_id")%>%
  mutate(month=substr(order_date,6,7),
         year=substr(order_date,1,4),
         quarter=case_when(month%in%c("01","02","03")~1,
                           month%in%c("04","05","06")~2,
                           month%in%c("07","08","09")~3,
                            TRUE~4))%>%
  group_by(state,quarter,year,product_id)%>%
  mutate(quantity_sum=sum(quantity))%>%
  distinct(state,quantity_sum,year,quarter,product_name,model_year)


## Odpowiedz przypisana do zmiennej

ANS_TASK_01 <-orders_1%>%
  group_by(state,year,quarter)%>%
  filter(quantity_sum==max(quantity_sum))%>%
  distinct(state,quarter,product_name,model_year)









####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 




## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <-df_orders%>%
  mutate(year=substr(order_date,1,4),month=substr(order_date,6,7),shipped_delivery=
           case_when(order_status==3~0,
                     TRUE~1),total=1)%>%
  group_by(year,month)%>%
  summarise(pct_not_realised=100*(1-(sum(shipped_delivery)/sum(total))))


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?


revenueYearly<-df_orders%>%
  inner_join(df_order_items, by="order_id")%>%
  inner_join(df_products,by="product_id")%>%
  select(order_id,product_name,quantity,list_price.x,discount,order_date)%>%
  mutate(year=substr(order_date,1,4))%>%
  group_by(year,product_name)%>%
  mutate(total=sum(quantity*list_price.x*(1-discount)))


## Odpowiedz przypisana do zmiennej
ANS_TASK_03<-revenueYearly%>%
  group_by(year)%>%
  mutate(max=max(total))%>%
  filter(max==total)%>%
  distinct(product_name,year)






####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 


counted_orders<-df_orders%>%
  mutate(year=substr(order_date,1,4))%>%
  group_by(year,customer_id)%>%
  summarise(Orders_count=n())

counted_orders_year<-counted_orders%>%
  group_by(year)%>%
  filter(Orders_count==max(Orders_count))


## Odpowiedz przypisana do zmiennej
ANS_TASK_04<-counted_orders_year%>%
  group_by(year,Orders_count)%>%
  summarise(How_many_customers=n())
 







####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?


## Odpowiedz przypisana do zmiennej

orders_5<-df_customers%>%
  inner_join(df_orders,by="customer_id")%>%
  mutate(year=substr(order_date,1,4),send_count=1)%>%
  group_by(email,year)%>%
  mutate(count=sum(send_count))%>%
  distinct(customer_id,year,count)
  

ANS_TASK_05<-orders_5%>%
  mutate(domain=sub(".*@","",email))%>%
  group_by(year,domain)%>%
  mutate(sum_domain=sum(count))%>%
  distinct(year,domain,sum_domain)%>%
  group_by(year)%>%
  filter(sum_domain==max(sum_domain))


 




####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?



result_6a<-df_orders%>%
  inner_join(df_customers,by="customer_id")%>%
  filter(state%in%c("CA","TX"))%>%
  distinct(customer_id)%>%
  summarise(active=n())


customer_2018<-df_orders%>%
  inner_join(df_customers,by="customer_id")%>%
  mutate(year=substr(order_date,1,4))%>%
  filter(year==2018)%>%
  distinct(customer_id)%>%
  mutate(order_in_2018=TRUE)

result_6b<-df_orders%>%
  inner_join(df_customers,by="customer_id")%>%
  left_join(customer_2018,by="customer_id")%>%
  filter(is.na(order_in_2018))%>%
  distinct(customer_id,first_name,last_name)



#Ludzie, ktorzy nie byli aktywni w 2018 - result_6b


## Odpowiedz przypisana do zmiennej
ANS_TASK_06<-c(result_6a,TRUE)


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

orders_7<-df_orders%>%
  inner_join(df_order_items,by="order_id")%>%
  group_by(order_id)%>%
  mutate(total=sum(quantity*list_price*(1-discount)))%>%
  distinct(order_id,customer_id,total)%>%
  ungroup()


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <-orders_7%>%
  left_join(df_customers,by="customer_id")%>%
  filter(total<quantile(orders_7$total,0.05) | total>quantile(orders_7$total,0.95))%>%
  distinct(customer_id,first_name,last_name)





####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

orders_8<-df_orders%>%
  mutate(day=as.numeric(format(as.Date(order_date), "%d")),
         month=substr(order_date,6,7),
         year=substr(order_date,1,4),
         quarter=case_when(month%in%c("01","02","03")~1,
                           month%in%c("04","05","06")~2,
                           month%in%c("07","08","09")~3,
                           TRUE~4))%>%
  group_by(year,quarter,month,day)%>%
  summarise(count=n())

## Odpowiedz przypisana do zmiennej

ANS_TASK_08<-orders_8%>%
  group_by(year,quarter)%>%
  summarise(max=max(count),min=min(count),median=median(count))



####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

## Odpowiedz przypisana do zmiennej

ANS_TASK_09<-df_orders%>%
  inner_join(df_customers,by="customer_id")%>%
  mutate(year=substr(order_date,1,4),
         date_diff=as.numeric(as.Date(shipped_date)-as.Date(order_date)))%>%
  group_by(state,year)%>%
  summarise(avg=mean(date_diff,na.rm=TRUE))%>%
  pivot_wider(names_from=state,values_from = avg)






####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.


orders_10<-df_orders%>%
  inner_join(df_customers,by="customer_id")%>%
  mutate(year=substr(order_date,1,4))%>%
  select(customer_id,last_name,year)

distinctyears<-df_orders%>%
  mutate(year=substr(order_date,1,4))%>%
  summarise(min=min(as.numeric(year)),max=max(as.numeric(year)))


customer_years <- orders_10 %>%
  group_by(customer_id) %>%
  summarise(distinct_years = n_distinct(year))


yearstotal<-distinctyears[1,2]-distinctyears[1,1]+1


letters<-orders_10%>%
  inner_join(customer_years,by="customer_id")%>%
  filter(yearstotal==distinct_years)%>%
  mutate(first_letter=substr(last_name,1,1))%>%
  distinct(customer_id,first_letter)%>%
  select(first_letter)
## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <-letters%>%
  group_by(first_letter)%>%
  summarise(count=n())%>%
  arrange(desc(count))







####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

productsBrand<-df_products%>%
  inner_join(df_categories,by="category_id")


orders_11<-df_orders%>%
  inner_join(df_order_items, by="order_id")%>%
  inner_join(productsBrand,by="product_id")%>%
  group_by(customer_id,category_name)%>%
  summarise(count=n())%>%
  arrange(-desc(customer_id))%>%
  pivot_wider(names_from = category_name,values_from = count,values_fill = 0)

neworders<-df_orders%>%
  inner_join(df_order_items, by="order_id")%>%
  inner_join(df_products,by="product_id")%>%
  mutate(shippment_year=substr(order_date,1,4))%>%
  mutate(newstprod=case_when(shippment_year==model_year~1,
                   TRUE~0))%>%
  group_by(customer_id)%>%
  summarise(count=sum(newstprod))%>%
  arrange(-desc(customer_id))%>%
  mutate(category_name="Newest_product")%>%
  pivot_wider(names_from = category_name,values_from = count)%>%
  select(Newest_product)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_11<-cbind(orders_11,neworders)









### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat


## Odpowiedz przypisana do zmiennej

orders_12<-df_orders%>%
  inner_join(df_order_items,by="order_id")%>%
  mutate(day=as.numeric(format(as.Date(order_date), "%d"))%%7)%>%
  group_by(order_id)%>%
  mutate(theory_price=sum(quantity*list_price),
         discount_price=sum((1-discount)*quantity*list_price),  
         discount_whole=2*(theory_price-discount_price)/(theory_price+discount_price))

ANS_TASK_12<-orders_12%>%
  group_by(product_id,day)%>%
  summarise(avg=mean(discount_whole,na.rm = TRUE))%>%
  pivot_wider(values_from = avg,names_from = day)

colnames(ANS_TASK_12)<-c("prodcut_id","Friday","Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday")













### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "KotlowskiPiotr.rds")