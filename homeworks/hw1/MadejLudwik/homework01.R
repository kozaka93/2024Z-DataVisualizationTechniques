library(dplyr)
library(tidyr)
library(stringr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.
odp1 <-df_orders %>% 
  inner_join(df_customers, by = "customer_id") %>%
  inner_join(df_order_items, by = "order_id") %>% 
  mutate(
    kwartal=(as.numeric(str_sub(order_date, 6, 7)) - 1) %/% 3 + 1,
    ) %>% 
  select(product_id, state, kwartal, order_id, quantity) %>% 
  group_by(kwartal, state, product_id) %>% 
  summarise(count=sum(quantity)) %>% 
  group_by(kwartal, state) %>% 
  filter(count == max(count)) %>% 
  ungroup() %>% 
  inner_join(df_products, by="product_id") %>% 
  select(kwartal, state, product_name, model_year)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- odp1 #moje rozwiązanie nie zakłada rozróżnialności kwartałów pośród lat


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu?
odp2 <- df_orders %>% 
  mutate(order_status=(order_status==3), #za niezrealizowane uznaje te pod nazwa "REJECTED"
         month=str_sub(order_date,6,7)) %>% 
  mutate(month=as.numeric(month)) %>% 
  group_by(month) %>% 
  summarise(rejected_percentage=mean(order_status)*100)

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- odp2 

####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
odp3 <- df_orders %>% 
  mutate(year=str_sub(order_date, 1,4)) %>% 
  inner_join(df_order_items %>% 
               mutate(price=list_price*(1-discount)*quantity) %>% 
               select(order_id, product_id, price),
             by = "order_id") %>% 
  select(year, product_id, price) %>%
  group_by(year, product_id) %>% 
  summarise(total_spent=sum(price)) %>% 
  group_by(year) %>% 
  filter(total_spent == max(total_spent)) %>% 
  ungroup() %>% 
  inner_join(df_products %>% 
               select(product_id, product_name),
             by = "product_id") %>% 
  select(year, product_name)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- odp3 


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 
odp4 <- df_orders %>% 
  mutate(year=str_sub(order_date, 1, 4)) %>% 
  group_by(year, customer_id) %>% 
  summarise(total_orders = n()) %>% 
  group_by(year) %>% 
  filter(total_orders == max(total_orders)) %>% 
  summarise(
    customers_with_max=n(),
    max_orders=max(total_orders)
    )
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- odp4 


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
odp5 <- df_orders %>% 
  inner_join(df_customers %>% 
               mutate(domena=sapply(str_split(email, "@"), "[" , 2)) %>% 
               select(customer_id, domena),
             by = "customer_id") %>% 
  mutate(year=str_sub(order_date, 1,4)) %>% 
  select(year, domena) %>% 
  group_by(year, domena) %>% 
  summarise(count_per_domena=n()) %>% 
  group_by(year) %>% 
  filter(count_per_domena == max(count_per_domena)) %>% 
  ungroup()
## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- odp5 


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018? NIE
highlighted_customers <- df_customers %>% 
  filter((zip_code>90000 & zip_code < 96162+1) | (zip_code > 75001-1 & zip_code < 88595+1)) #klienci z wybranych stanow

odp6 <- df_orders %>% 
  group_by(customer_id) %>% 
  summarise(orders_made = n()) %>% 
  right_join(highlighted_customers, by="customer_id") %>% 
  select(customer_id, orders_made) %>% 
  filter(orders_made!=0) %>% 
  summarise(active_customers_from_tx_ca=n())

zmiennaPomocnicza <- df_orders %>% 
  inner_join(df_customers, by="customer_id") %>%
  filter(state %in% c("CA", "TX")) %>% 
  group_by(customer_id) %>% 
  summarise(orders_made_in_2018=sum(str_sub(order_date,1,4)=="2018", na.rm=TRUE)) %>%
  select(customer_id, orders_made_in_2018) %>% 
  filter(orders_made_in_2018 == 0) %>% 
  summarise(how_many_customers_with_zero_orders_in_2018 = n()) 

odp6 <- cbind(odp6, zmiennaPomocnicza)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- odp6 

####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
odp7 <- df_orders %>% 
  inner_join(df_order_items %>% 
               mutate(price=list_price*(1-discount)*quantity) %>% 
               select(order_id, price),
             by="order_id") %>% 
  select(customer_id, price, order_id) %>% 
  group_by(customer_id, order_id) %>% 
  summarise(total_spent = sum(price)) %>%
  mutate(pomocnicza=1) %>% 
  group_by(pomocnicza) %>% 
  filter((total_spent <  quantile(total_spent, 0.05)) | (total_spent > quantile(total_spent, 0.95))) %>% 
  ungroup() %>% 
  distinct(customer_id) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  select(first_name, last_name, customer_id)

ANS_TASK_07 <- odp7 

####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
odp8 <- df_orders %>% 
  filter(!is.null(order_date)) %>% 
  mutate(
    month=as.numeric(str_sub(order_date, 6, 7)),
    day=as.numeric(str_sub(order_date, 9, 10))
    ) %>% 
  mutate(kwartal=(month-1) %/% 3 + 1) %>%
  group_by(kwartal, day) %>%
  summarise(count=n()) %>% 
  group_by(kwartal) %>% 
  summarise(
    min_count=min(count),
    max_count=max(count),
    median_count=median(count)
    )

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- odp8 #Moje rozwiązanie nie uwzględnia rozróżnialności kwartałów względem lat


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
calculate_time <- function(y_0, m_0, d_0, y_1, m_1, d_1){
  if ((y_0 %% 4 == 0) & d_0 %% 100 != 0) {
    miesiace <- c(31, 29, 31,29,31,30,31,30,31,31,30,31,30,31)
  } else {
    miesiace <- c(31, 28, 31,28,31,30,31,30,31,31,30,31,30,31)
  }
  ilosc_dni = 0
  dni_w_miesiacu <- miesiace[[m_0]]
  while (y_0 != y_1 | m_0 != m_1 | d_0 != d_1) {
    ilosc_dni <- ilosc_dni+1
    d_0 <- d_0 + 1
    if (d_0 > dni_w_miesiacu){
      d_0 = d_0 %% dni_w_miesiacu
      m_0 <- m_0 + 1
      if (m_0 > 12) {
        m_0 <- 1
        y_0 <- y_0+1
        if (y_0 %% 4 == 0 & y_0 %% 100 != 0) {
          miesiace <- c(31, 29, 31,29,31,30,31,30,31,31,30,31,30,31)
        } else {
          miesiace <- c(31, 28, 31,28,31,30,31,30,31,31,30,31,30,31)
        }
      dni_w_miesiacu <- dni_w_miesiacu <- miesiace[[m0]]
      }
    }
  }
  ilosc_dni
}


odp09 <- df_orders %>% 
  filter(!is.null(order_date) & !is.null(shipped_date)) %>% 
  mutate(
    order_date = as.Date(order_date),
    shipped_date = as.Date(shipped_date),
    year=as.numeric(str_sub(order_date,1,4))
    # month_ordered=as.numeric(str_sub(order_date,6,7)),
    # day_ordered=as.numeric(str_sub(order_date,9,10)),
    # year_shipped=as.numeric(str_sub(shipped_date,1,4)),
    # month_shipped=as.numeric(str_sub(shipped_date,6,7)),
    # day_shipped=as.numeric(str_sub(shipped_date,9,10))
  ) %>%
  mutate(delivery_time = as.numeric(shipped_date - order_date)) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  group_by(year, state) %>% 
  summarise(average_delivery_time = mean(delivery_time, na.rm = TRUE)) %>% 
  pivot_wider(names_from = state, values_from = average_delivery_time)

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- odp09 


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
literki <- df_orders %>% 
  mutate(year=as.numeric(str_sub(order_date,1,4))) %>% 
  group_by(year, customer_id) %>% 
  summarise(total_count=n()) %>% 
  filter(total_count > 0) %>% 
  group_by(customer_id) %>% 
  summarise(year_sum = sum(year)) %>% 
  filter(year_sum==2017*3) %>% 
  inner_join(df_customers) %>% 
  mutate(first_letter = str_sub(last_name,1,1)) %>% 
  select(first_letter) 

zliczenie_literek <- table(literki)

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- zliczenie_literek


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

zliczenie_nowych <- df_orders %>% 
  mutate(year=as.numeric(str_sub(order_date,1,4))) %>% 
  select(order_id, customer_id, year) %>% 
  
  inner_join(df_order_items %>% 
               select(product_id, order_id, quantity), 
             by = "order_id") %>% 
  
  inner_join(df_products %>% 
               select(product_id, 
                      product_name, 
                      model_year, 
                      category_id),
             by="product_id") %>%
  
  inner_join(
    df_categories,
    by="category_id") %>% 
  mutate(newest_one = (model_year==year)*quantity) %>% 
  inner_join(df_customers %>%
               select(customer_id, first_name, last_name),
             by = "customer_id") %>%
  rowwise() %>% 
  #mutate(full_name = paste(first_name, last_name, collapse = " ")) %>% 
  group_by(customer_id) %>%
  summarise(newest_ones = sum(newest_one))

odp11 <- df_orders %>% 
  mutate(year=as.numeric(str_sub(order_date,1,4))) %>% 
  select(order_id, customer_id, year) %>% 
  
  inner_join(df_order_items %>% 
               select(product_id, order_id, quantity), 
             by = "order_id") %>% 
  
  inner_join(df_products %>% 
               select(product_id, 
                      product_name, 
                      model_year, 
                      category_id),
             by="product_id") %>%
  
  right_join(
    df_categories,
    by="category_id") %>% 
  
  mutate(newest_one = (model_year==year)) %>% 
  group_by(customer_id, category_name) %>% 
  summarise(count=sum(quantity)) %>% 
  select(customer_id, count, category_name) %>%
  
  inner_join(zliczenie_nowych, by = "customer_id") %>% 
  
  inner_join(df_customers %>%
               select(customer_id, first_name, last_name),
             by = "customer_id") %>%
  rowwise() %>% 
  mutate(full_name = paste(first_name, last_name, collapse = " ")) %>% 
  select(customer_id, full_name, count, category_name, newest_ones) %>% 
  
  pivot_wider(
    names_from = category_name, 
    values_from = count,
    values_fill = 0
    ) 

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- odp11

### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
odp12 <- df_orders %>% 
  select(order_id, order_date) %>% 
  inner_join(df_order_items %>% 
               mutate(rabat=discount*100) %>% 
               select(order_id, rabat),
             by="order_id") %>% 
  mutate(order_date=as.Date(order_date)) %>% 
  mutate(day_of_week=weekdays(order_date)) %>% 
  group_by(day_of_week) %>% 
  summarise(sredni_rabat=mean(rabat))

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- odp12 

### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "MadejLudwik.rds")
