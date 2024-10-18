library(dplyr)
library(tidyr)
library(stringi)
library(lubridate)


df_orders <- read.csv('hw1/orders.csv')
df_order_items <- read.csv('hw1/order_items.csv')
df_products <- read.csv('hw1/products.csv')
df_brands <- read.csv('hw1/brands.csv')
df_categories <-  read.csv('hw1/categories.csv')
df_customers <- read.csv('hw1/customers.csv')
df_brands <- read.csv('hw1/brands.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

convert_to_quarter_format <- function(date_col) {

  year <- format(date_col, "%Y")
  month <- as.numeric(format(date_col, "%m"))
  quarter <- case_when(
    month < 4 ~ 1,
    month < 7 ~ 2,
    month < 10 ~ 3,
    TRUE ~ 4
  )
  result <- paste0(year, "Q", quarter)
  
  result
}




ans <- df_order_items %>%
  left_join(df_orders, by = 'order_id') %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  mutate(quarter_date = convert_to_quarter_format(as.Date(order_date))) %>% 
  group_by(quarter_date, state, product_id) %>% 
  summarise(num_of_purchases = sum(quantity)) %>%
  mutate(max_num_of_purchases = max(num_of_purchases)) %>% 
  filter(num_of_purchases == max_num_of_purchases) %>% 
  left_join(df_products, by = "product_id") %>% 
  select(c("quarter_date", "state", "product_name", "model_year"))
  







## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- ans


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 


ans_2 <- df_orders %>% 
  mutate(date_Ym = format(as.Date(order_date), "%Y-%m")) %>% 
  group_by(date_Ym) %>% 
  mutate(all_orders_month_sum = n()) %>% 
  mutate(failed_orders_month_sum = sum(order_status == 3)) %>% 
  mutate(percent_of_failed_month = failed_orders_month_sum/all_orders_month_sum) %>% 
  mutate(percent_of_failed_month = percent_of_failed_month*100) %>% 
  select("date_Ym", "percent_of_failed_month")
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- ans_2


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

ans_3 <- df_order_items %>% 
  left_join(df_orders, by='order_id') %>% 
  mutate(Year = format(as.Date(order_date), "%Y")) %>% 
  group_by(Year, product_id) %>% 
  mutate(item_revenue = quantity*list_price*(1-discount)) %>% 
  summarise(revenue = sum(item_revenue)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  mutate(max_revenue_by_year = max(revenue)) %>% 
  filter(revenue == max_revenue_by_year) %>% 
  left_join(df_products, by="product_id") %>% 
  select(Year, product_name)
  



## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- ans_3


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

ans_4 <- df_orders %>% 
  left_join(df_customers, by='customer_id') %>% 
  mutate(Year = format(as.Date(order_date), "%Y")) %>%
  group_by(Year, customer_id) %>% 
  mutate(orders_by_year = n()) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  mutate(max_orders_by_year = max(orders_by_year)) %>% 
  filter(orders_by_year == max_orders_by_year) %>% 
  select(Year, customer_id, first_name, last_name, orders_by_year)
  





## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <-ans_4


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

domain_extractor <- function(email_adress){
  domain_start <- stri_locate_first_fixed(email_adress, "@")[,1]
  stri_sub(email_adress, domain_start - stri_length(email_adress), -1)
}





ans_5 <- df_orders %>% 
  left_join(df_customers, by='customer_id') %>% 
  mutate(Year = format(as.Date(order_date), "%Y")) %>% 
  mutate(domain = domain_extractor(email)) %>% 
  group_by(Year, domain) %>% 
  mutate(domain_num_of_orders = n()) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  mutate(max_domain_num_of_orders = max(domain_num_of_orders)) %>% 
  filter(domain_num_of_orders == max_domain_num_of_orders) %>% 
  select(Year, domain, domain_num_of_orders) %>% 
  group_by(Year, domain, domain_num_of_orders) %>% 
  slice(1)






## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- ans_5


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

ans_6_1 <- length(df_customers$state[df_customers$state %in% c('TX', 'CA')])

modified_df_orders <- df_orders %>% 
  mutate(Year = format(as.Date(order_date), "%Y")) %>% 
  filter(Year == "2018")

ans_6_2 <- df_customers %>% 
  filter(state %in% c('TX', 'CA')) %>% 
  left_join(modified_df_orders, by="customer_id") %>% 
  filter(is.na(order_id))
  










## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- list(ans_6_1, length(ans_6_2$customer_id) > 0)


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

ans_7 <- df_order_items %>%
  left_join(df_orders, by = 'order_id') %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  group_by(order_id) %>% 
  mutate(order_price = sum(quantity*list_price*(1-discount))) %>% 
  select(order_id, order_price, customer_id, first_name, last_name) %>% 
  ungroup() %>% 
  group_by(order_id, order_price, customer_id, first_name, last_name) %>% 
  slice(1) %>% 
  ungroup() %>% 
  bind_cols(data.frame(t(quantile(.$order_price, probs = (1:100)/100)))) %>% 
  filter(order_price < X5. | order_price > X95.) %>% 
  select(customer_id, first_name, last_name)
  








## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- ans_7


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

ans_8 <- df_orders %>% 
  mutate(quarter_date = convert_to_quarter_format(as.Date(order_date))) %>% 
  group_by(order_date) %>% 
  mutate(num_of_orders_day = n()) %>% 
  ungroup() %>% 
  group_by(quarter_date) %>% 
  mutate(quarter_min = min(num_of_orders_day)) %>% 
  mutate(quarter_median = median(num_of_orders_day)) %>% 
  mutate(quarter_max = max(num_of_orders_day)) %>% 
  select(quarter_date, quarter_min, quarter_median, quarter_max) %>% 
  ungroup() %>% 
  group_by(quarter_date, quarter_min, quarter_median, quarter_max) %>% 
  slice(1)





## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- ans_8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

ans_9 <- df_orders %>% 
  left_join(df_customers, by="customer_id") %>% 
  filter(order_status == 4) %>% 
  mutate(Year = format(as.Date(order_date), "%Y")) %>% 
  mutate(order_travel_time = difftime(as.Date(shipped_date), as.Date(order_date), units = "days")) %>% 
  group_by(Year, state) %>% 
  summarise(average_shipping_time = mean(order_travel_time)) %>% 
  pivot_wider(names_from = Year, values_from = average_shipping_time)
  
















## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- ans_9


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

ans_10 <- df_orders %>% 
  left_join(df_customers, by='customer_id') %>% 
  mutate(Year = format(as.Date(order_date), "%Y")) %>% 
  select(Year, customer_id, last_name) %>% 
  group_by(Year, customer_id, last_name) %>% 
  mutate(num_of_years_active = n()) %>% 
  filter(num_of_years_active >= 3) %>% 
  slice(1) %>% 
  mutate(first_letter = stri_sub(last_name, 1, 1)) %>% 
  ungroup() %>% 
  select(first_letter) %>% 
  group_by(first_letter) %>% 
  mutate(num_of_appearances = n())

  
  
  








## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- ans_10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

sum_newest_purchased <- df_order_items %>%
  left_join(df_orders, by = 'order_id') %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  left_join(df_products, by="product_id") %>% 
  mutate(Year = format(as.Date(order_date), "%Y")) %>% 
  group_by(customer_id, product_id) %>% 
  mutate(sum_purchased = sum(quantity)) %>% 
  mutate(newest_purchased = Year == model_year) %>% 
  ungroup() %>% 
  group_by(customer_id) %>% 
  mutate(sum_newest_purchased = sum(quantity)*newest_purchased) %>% 
  ungroup() %>% 
  select(customer_id, product_id, sum_purchased, sum_newest_purchased) %>% 
  group_by(customer_id, sum_newest_purchased) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(customer_id, sum_newest_purchased) %>% 
  group_by(customer_id) %>% 
  mutate(sum_newest_purchased = sum(sum_newest_purchased)) %>% 
  slice(1) %>% 
  select(sum_newest_purchased)

sum_newest_purchased <- append(c("sum_newest_purchased"), sum_newest_purchased$sum_newest_purchased)


ans_11 <- df_order_items %>%
  left_join(df_orders, by = 'order_id') %>% 
  left_join(df_customers, by = 'customer_id') %>% 
  left_join(df_products, by="product_id") %>% 
  mutate(Year = format(as.Date(order_date), "%Y")) %>% 
  group_by(customer_id, product_id) %>% 
  mutate(sum_purchased = sum(quantity)) %>% 
  mutate(newest_purchased = Year == model_year) %>% 
  ungroup() %>% 
  group_by(customer_id) %>% 
  mutate(sum_newest_purchased = sum(quantity)*newest_purchased) %>% 
  ungroup() %>% 
  select(customer_id, product_id, sum_purchased) %>% 
  group_by(customer_id, product_id, sum_purchased) %>% 
  slice(1) %>% 
  ungroup() %>% 
  pivot_wider(names_from = customer_id, values_from = sum_purchased, values_fill = 0)
  
  
  
  
  
  





## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- rbind(ans_11, sum_newest_purchased)


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

ans_12 <- df_order_items %>% 
  left_join(df_orders, by="order_id") %>% 
  mutate(day_of_week =  wday(order_date, week_start = 1)) %>% 
  group_by(day_of_week, product_id) %>% 
  summarise(mean_discount = mean(discount))





## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- ans_12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "DuboisJozef.rds")

