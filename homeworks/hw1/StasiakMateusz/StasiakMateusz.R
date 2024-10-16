library(dplyr)
library(tidyr)


df_orders <- read.csv("homeworks/hw1/dane/orders.csv")
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.


df_1<-df_orders %>% 
    mutate(order_month=as.integer(format(as.Date(order_date,format="%Y-%m-%d"),"%m"))) %>% 
    inner_join(df_customers,by="customer_id") %>% 
    inner_join(df_order_items,by="order_id") %>% 
    mutate(order_quater=case_when(
        order_month<=3~1,
        order_month<=6~2,
        order_month<=9~3,
        TRUE~4
    )) %>% 
    select(customer_id,order_quater,state,product_id) %>% 
    group_by(order_quater,state,product_id) %>% 
    summarise(count=n()) %>% 
    group_by(order_quater,state) %>% 
    filter(count==max(count)) %>% 
    inner_join(df_products,by="product_id") %>% 
    select(order_quater,state,product_name,model_year)
    

#View(df_1)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- df_1


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

df_2<-df_orders %>% 
    mutate(
        order_month=as.integer(format(as.Date(order_date,format="%Y-%m-%d"),"%m")),
        not_shipped=ifelse(shipped_date=="NULL",1,0)
           ) %>% 
    group_by(order_month) %>% 
    summarise(count=n(),not_shipped_count=sum(not_shipped)) %>% 
    mutate(not_delivered_percentage=paste(round(10000*not_shipped_count/count)*0.01,"%",sep="")) %>% 
    select(order_month,not_delivered_percentage)

#View(df_2)
    
## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_2


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?


df_3<-df_orders %>% 
    inner_join(df_order_items,by="order_id") %>% 
    mutate(
        year=format(as.Date(order_date,format="%Y-%m-%d"),"%Y"),
        revenue=list_price*(1-discount)
           ) %>% 
    select(year,product_id,revenue) %>% 
    group_by(year,product_id) %>% 
    summarise(total_revenue=sum(revenue)) %>% 
    group_by(year) %>% 
    filter(total_revenue==max(total_revenue)) %>% 
    inner_join(df_products,by="product_id") %>% 
    select(year,product_name)

#View(df_3)
    
## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_3


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

df_4<-df_orders %>% 
    mutate(year=format(as.Date(order_date,format="%Y-%m-%d"),"%Y")) %>% 
    group_by(year,customer_id) %>% 
    summarise(order_count=n()) %>% 
    group_by(year) %>% 
    filter(order_count==max(order_count)) %>% 
    group_by(year,order_count) %>% 
    summarise(customer_count=n())

#View(df_4)
    

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- df_4


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

df_5<-df_orders %>% 
    inner_join(df_customers) %>% 
    mutate(domain=sub(".*@(.*)\\.com", "\\1", email),
           year=format(as.Date(order_date,format="%Y-%m-%d"),"%Y")) %>% 
    group_by(year,domain) %>% 
    summarise(count=n()) %>% 
    select(year,domain,count) %>% 
    group_by(year) %>% 
    filter(count==max(count))
    
#View(df_5)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

df_6<-df_orders %>% 
    inner_join(df_customers) %>% 
    filter(state=="CA" | state=="TX") %>% 
    mutate(year=format(as.Date(order_date,format="%Y-%m-%d"),"%Y")) %>% 
    mutate(ordered_in_2018=ifelse(year=="2018",1,0)) %>% 
    group_by(customer_id) %>% 
    summarise(pom=sum(ordered_in_2018)) %>% 
    filter(pom==0)

#tak sa tacy

#View(df_6)
    

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- df_6


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

df_7<-df_orders %>% 
    inner_join(df_order_items) %>% 
    mutate(money_spent=list_price*(1-discount))

quantile_5<-quantile(df_7$money_spent,0.05)
quantile_95<-quantile(df_7$money_spent,0.95)

df_7<-df_7 %>% 
    filter(money_spent>quantile_95|money_spent<quantile_5) %>% 
    select(customer_id,money_spent)

#print(quantile_5)
#print(quantile_95)
#View(df_7)
    


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- df_7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

df_8<-df_orders %>% 
    mutate(
        order_month=as.integer(format(as.Date(order_date,format="%Y-%m-%d"),"%m")),
        order_day=as.integer(format(as.Date(order_date,format="%Y-%m-%d"),"%d"))
        ) %>% 
    mutate(order_quater=case_when(
        order_month<=3~1,
        order_month<=6~2,
        order_month<=9~3,
        TRUE~4
    )) %>% 
    group_by(order_quater,order_month,order_day) %>% 
    summarise(order_count=n()) %>% 
    group_by(order_quater) %>% 
    summarise(
        min=min(order_count),
        max=max(order_count),
        median=median(order_count)
    )

#View(df_8)
    
## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- df_8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

df_9<-df_orders %>% 
    mutate(
        shipped_date=as.Date(shipped_date),
        order_date=as.Date(order_date),
        year=format(as.Date(order_date,format="%Y-%m-%d"),"%Y")
        ) %>% 
    mutate(delivery_time=shipped_date-order_date) %>% 
    inner_join(df_customers) %>% 
    group_by(state,year) %>% 
    summarise(average_delivery_time=mean(delivery_time,na.rm=TRUE)) %>% 
    pivot_wider(names_from=state,values_from = average_delivery_time) 

#View(df_9)
    

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- df_9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

df_10<-df_orders %>% 
    mutate(year=format(as.Date(order_date,format="%Y-%m-%d"),"%Y")) %>% 
    group_by(customer_id,year) %>% 
    select(customer_id,year) %>% 
    group_by(customer_id) %>% 
    summarise(number_of_years_active=n()) %>% 
    filter(number_of_years_active==3) %>% 
    inner_join(df_customers) %>% 
    mutate(first_letter=substr(last_name,1,1)) %>% 
    group_by(first_letter) %>% 
    summarise(letter_count=n())

#View(df_10)

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- df_10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

df_11<-df_orders %>% 
    mutate(order_year=format(as.Date(order_date,format="%Y-%m-%d"),"%Y")) %>% 
    inner_join(df_order_items) %>% 
    inner_join(df_products) %>% 
    mutate(is_new=ifelse(order_year==model_year,1,0)) %>% 
    inner_join(df_categories) %>% 
    select(customer_id,category_name,is_new)

df_categories<-df_11%>% 
    group_by(customer_id,category_name) %>% 
    summarise(category_count=n()) %>% 
    pivot_wider(names_from = category_name,values_from = category_count,values_fill = 0)
    
df_is_new<-df_11 %>% 
    group_by(customer_id) %>% 
    summarise(new_count=sum(is_new))

df_11<-df_is_new %>% 
    inner_join(df_categories,by="customer_id")

#View(df_11)


## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- df_11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

df_12<-df_orders %>% 
    inner_join(df_order_items) %>% 
    mutate(week_day=weekdays(as.Date(order_date))) %>% 
    inner_join(df_products) %>% 
    group_by(product_name,week_day) %>% 
    summarise(average_discount=mean(discount)) %>% 
    mutate(average_discount=paste(round(10000*average_discount)*0.01,"%",sep="")) 

#View(df_12)

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- df_12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "StasiakMateusz.rds")