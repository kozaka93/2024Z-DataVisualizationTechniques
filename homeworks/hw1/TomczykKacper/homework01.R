library(dplyr)
library(tidyr)
library(lubridate)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers<-  read.csv('homeworks/hw1/dane/customers.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

# 4 kwartały i sporo stanow - sporo różnych produktów i lat produkcji



# dokleic do order_items treść kolumn (order_date(MOnth)) z orders i state z customers
#teraz znaleźć, który model pojawiał się najczęściej (może... n() +top.n(1)???)
#po pogrupowaniu na kwartały i store_id
#quarter = ceiling((as.integer(format(as.Date(order_date), "%m")))/3) <- wyznaczanie kwartałów
#



ANS_TASK_01<-df_order_items %>% 
  left_join(df_orders[c('order_id','order_date','customer_id')], by = 'order_id') %>% 
  left_join(df_customers[c('customer_id','state')],by = 'customer_id') %>% 
  mutate(quarter = quarter(order_date,with_year = TRUE)) %>% 
  group_by(state,quarter,product_id) %>%
  summarise(cnt = sum(quantity)) %>% 
  group_by(state, quarter) %>%
  arrange(desc(cnt)) %>%
  slice(1) %>%
  left_join(df_products[c('product_id','product_name','model_year')],by = 'product_id') %>%
  select(state,quarter, product_name, model_year)



####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 


ANS_TASK_02 <- df_orders %>% 
  mutate(month_year = format(as.Date(order_date),'%Y-%m')) %>% 
  group_by(month_year) %>% 
  summarise(cnt = n(),
            unrealized = sum(order_status != 4),
            ) %>% 
  mutate(procent = (unrealized/cnt)*100) %>% 
  select(month_year,procent)
  






####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

#trzeba zebrac zamówienia z każdego roku
#a raczej po ile i czego zamówiono każdego roku+
#
#zatem biore order items, doklejam order date i order status(po order id)
#po order status biore tylko te, które mają 4 (dotarły - zrealizowane)
#pogrupować po roku i zrobić tak:
# musze policzyc, ile który produkt zarobił (to, ile go bylo w zamówieniu razy cena minus discount)
#potem zsumowac przychod produktow z tym samym id
#
#wypisac topke



ANS_TASK_03 <- df_order_items %>% 
  left_join(df_orders[c('order_id','order_date','order_status')], by='order_id') %>% 
  filter(order_status == 4) %>% 
  mutate(year = format(as.Date(order_date),'%Y'))%>% 
  group_by(year,product_id) %>%
  summarise(income = sum((quantity*list_price)*(1-discount))) %>% 
  group_by(year) %>% 
  arrange(desc(income)) %>% 
  top_n(1) %>% 
  ungroup %>% 
  left_join(df_products[c('product_id','product_name')], by='product_id') %>% 
  select(-c('product_id'))

  








####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 


ANS_TASK_04 <- df_orders %>% 
  mutate(year = format(as.Date(order_date),'%Y')) %>% 
  group_by(year, customer_id) %>% 
  summarize(count_of_orders = n()) %>% 
  group_by(year) %>% 
  summarise(top_value = max(count_of_orders),
            count_of_clients =sum(count_of_orders == top_value)) 
  
  
  






####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?


ANS_TASK_05 <- df_orders %>% 
  left_join(df_customers[c('customer_id','email')], by = 'customer_id') %>% 
  mutate(domains = sub(".*@","",email)) %>% 
  mutate(year = format(as.Date(order_date),'%Y')) %>% 
  group_by(year,domains) %>% 
  summarize(count = n()) %>% 
  group_by(year) %>% 
  arrange(desc(count)) %>% 
  top_n(1) %>% 
  ungroup
  
           





####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?


ANS_TASK_06 <- df_orders %>% 
  left_join(df_customers,by = 'customer_id') %>% 
  mutate(year = format(as.Date(order_date),'%Y')) %>% 
  group_by(state) %>% 
  summarise(no_customers__from_2018 = ifelse(any(year==2018),'no','yes')) %>% 
  mutate(count(df_customers,state)) %>% 
  filter(state == 'CA' | state=='TX')
  
  
  
  
  



####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?


ANS_TASK_07 <- df_order_items %>% 
  group_by(order_id) %>% 
  summarise(value = sum(quantity*list_price*(1-discount))) %>% 
  filter(value <quantile(value, 0.05) |value >quantile(value, 0.95)) %>% 
  left_join(df_orders[c('order_id','customer_id')],by='order_id') %>% 
  left_join(df_customers,by='customer_id') %>% 
  select(first_name, last_name)
  





####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna 
#oraz mediana liczby zamówień złożonych każdego dnia (?miesiąca?) w poszczególnych kwartalach.

ANS_TASK_08 <- df_orders %>% 
  mutate(quarter = quarter(order_date,with_year = TRUE)) %>% 
  mutate(month_day = format(as.Date(order_date),'%m-%d')) %>% 
  group_by(quarter,month_day) %>% 
  summarise(cnt=n()) %>% 
  group_by(quarter) %>% 
  summarise(max = max(cnt),
            min = min(cnt),
            med = median(cnt))







####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie


ANS_TASK_09 <- df_orders %>% 
  left_join(df_customers[c('customer_id','state')],by='customer_id') %>% 
  mutate(year = format(as.Date(order_date),'%Y')) %>% 
  mutate(delta_time = as.Date(shipped_date)-as.Date(order_date)) %>% 
  group_by(year,state) %>% 
  summarise(mean_time = mean(delta_time, na.rm = TRUE)) %>% 
  pivot_wider(names_from = 'state',values_from = 'mean_time')
  


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.


ANS_TASK_10 <- df_orders %>% 
  mutate(year = format(as.Date(order_date),'%Y')) %>%
  group_by(customer_id) %>% 
  summarise(every_year_customer = (any(year == 2016) && any(year == 2017)&&any(year == 2018))) %>%
  filter(every_year_customer == TRUE) %>% 
  left_join(df_customers[c('customer_id','last_name')],by = 'customer_id') %>% 
  select('last_name') %>% 
  mutate(litery = strsplit(as.character(last_name), "")) %>%
  unnest(litery) %>%
  mutate(czy_pierwsza = grepl("[A-Z]", litery)) %>%
  #mutate(litery = toupper(litery)) %>%
  count(litery,czy_pierwsza) 
  
  
  





####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt 
#(rower został wyprodukowany w tym roku, kiedy złożono zamówienie)


newest_product_count<-df_order_items %>% 
  left_join(df_orders[c('order_id','customer_id','order_date')],by = 'order_id') %>% 
  left_join(df_products[c('product_id','model_year')], by = 'product_id') %>% 
  mutate(year = format(as.Date(order_date),'%Y')) %>% 
  group_by(customer_id) %>% 
  summarise(cnt = sum(model_year == year)) %>% 
  pivot_wider(names_from = customer_id, values_from = cnt)

ANS_TASK_11 <- df_order_items %>% 
  left_join(df_orders[c('order_id','customer_id')],by = 'order_id') %>% 
  left_join(df_products[c('product_id','category_id')], by = 'product_id') %>% 
  group_by(customer_id,category_id) %>%
  summarise(count = sum(quantity)) %>% 
  pivot_wider(names_from = customer_id,values_from = count) %>% 
  mutate_all(~ replace(., is.na(.), 0)) %>% 
  bind_rows(newest_product_count) 
  




### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej 
#a przychodem faktycznym uwzględniającym udzielony rabat

  
ANS_TASK_12<-df_order_items %>% 
  left_join(df_orders[c('order_id','order_date')],by = 'order_id') %>% 
  mutate(weekday = weekdays(as.Date(order_date))) %>% 
  mutate(price_after_discount = (list_price*(1-discount))) %>% 
  group_by(weekday) %>% 
  summarise(mean_discount =(sum(list_price)-sum(price_after_discount))/sum(list_price)*100)
  


## Odpowiedz przypisana do zmiennej




### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "TomczykKacper.rds")
