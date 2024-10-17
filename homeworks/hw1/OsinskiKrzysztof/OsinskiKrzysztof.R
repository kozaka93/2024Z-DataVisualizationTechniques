library(dplyr)
library(tidyr)


df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')



####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

# Założyłem, że w zadaniu rozróżniamy kwartały w różnych latach:
zad1 <- df_orders %>% 
  inner_join(df_customers, by = "customer_id") %>%
  inner_join(df_order_items, by = "order_id") %>% 
  mutate(year=as.numeric(substr(order_date,1 ,4 )), 
         quarter=(as.numeric(substr(order_date, 6, 7)) - 1) %/% 3 + 1,) %>%
  select(product_id, state, year, quarter, order_id, quantity) %>% 
  group_by(product_id, state, quarter, year) %>%
  summarise(product_count = sum(quantity)) %>% 
  group_by(state, quarter, year) %>% 
  filter(product_count == max(product_count)) %>% 
  inner_join(df_products, by="product_id") %>% 
  select(year, quarter, state, product_name, model_year) %>%
  arrange(year, quarter)

# Alternatywna wersja, w której nie rozróżniamy danego kwartału w różnych latach.
zad1_inaczej <- df_orders %>% 
  inner_join(df_customers, by="customer_id") %>%
  inner_join(df_order_items, by="order_id") %>% 
  mutate(quarter=(as.numeric(substr(order_date, 6, 7)) - 1) %/% 3 + 1,) %>% 
  select(product_id, state, quarter, order_id, quantity) %>% 
  group_by(quarter, state, product_id) %>% 
  summarise(product_count=sum(quantity)) %>% 
  group_by(quarter, state) %>% 
  filter(product_count == max(product_count)) %>% 
  ungroup() %>% 
  inner_join(df_products, by="product_id") %>% 
  select(quarter, state, product_name, model_year)
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- zad1



####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

# W tym zadaniu uznałem za zamówienia niezrealizowane te oznaczone jako "Rejected".
zad2 <- df_orders %>% 
  mutate(order_status=(order_status==3), month=as.numeric(substr(order_date,6,7))) %>% 
  group_by(month) %>% 
  summarise(rejected_orders_percentage=mean(order_status)*100) %>%
  mutate(month = month.name[month])   # Dla czytelności zmieniam numery na nazwy miesięcy.

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- zad2



####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

zad3 <- df_orders %>% 
  mutate(year=substr(order_date, 1, 4)) %>% 
  inner_join(df_order_items %>% 
               mutate(real_price=list_price*(1-discount)*quantity) %>% 
               select(order_id, product_id, real_price), by = "order_id") %>% 
  select(year, product_id, real_price) %>%
  group_by(year, product_id) %>% 
  summarise(total_income=sum(real_price)) %>% 
  group_by(year) %>% 
  filter(total_income == max(total_income)) %>% 
  ungroup() %>% 
  inner_join(df_products %>% 
               select(product_id, product_name), by = "product_id") %>% 
  select(year, product_name)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- zad3



####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

zad4 <- df_orders %>% 
  mutate(year=substr(order_date, 1, 4)) %>% 
  group_by(year, customer_id) %>% 
  summarise(customer_orders = n()) %>% 
  group_by(year) %>% 
  filter(customer_orders == max(customer_orders)) %>% 
  summarise(best_customers=n(), number_of_orders=max(customer_orders))
  
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- zad4



####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

zad5 <-df_customers %>%
  inner_join(df_orders,by="customer_id") %>%
  mutate(year=substr(order_date, 1, 4), domain=sub(".*@","",email)) %>%
  select(year, domain) %>% 
  group_by(year, domain) %>% 
  summarise(domain_count=n()) %>% 
  group_by(year) %>% 
  filter(domain_count == max(domain_count)) %>% 
  ungroup()

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- zad5



####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

zad6a <- df_orders %>% inner_join(df_customers, by="customer_id") %>% 
  select(customer_id, state) %>% 
  distinct() %>% 
  group_by(state) %>% 
  filter(state %in% c("CA", "TX")) %>%
  summarize(number_of_active_customers = n()) %>%
  add_row(state = "both CA and TX", 
          number_of_active_customers = sum(zad6a$number_of_active_customers))
# Dwa pierwsze wiersze zawierają liczbę aktywnych klientów w poszczególnych stanach,
# a ostatni wiersz zawiera ich sumę.

# Pomocnicza ramka z identyfikatorami aktywnych klientów w stanach Kalifornia i Teksas,
# którzy dokonali zamówienia w 2018 roku.
customers_with_order_in_2018 <- df_orders %>% inner_join(df_customers, by="customer_id") %>% 
  mutate(year = substr(order_date, 1, 4)) %>%
  filter(state %in% c("CA", "TX"), year == 2018) %>% 
  group_by(customer_id, year) %>% 
  summarize(.groups = "keep") %>%
  mutate(has_order_in_2018 = TRUE)

zad6b <- df_customers %>%
  filter(state %in% c("CA", "TX")) %>% 
  left_join(customers_with_order_in_2018) %>%
  mutate(has_order_in_2018 = ifelse(is.na(has_order_in_2018), FALSE, TRUE)) %>% 
  group_by(has_order_in_2018) %>% 
  summarize(number_of_customers = n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- list(zad6a, zad6b)



####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

zad7 <- df_orders %>% 
  inner_join(df_order_items %>% 
             mutate(real_price=list_price*(1-discount)*quantity),
             by="order_id") %>% 
  select(customer_id, real_price, order_id) %>% 
  group_by(customer_id, order_id) %>% 
  summarise(total_order_cost = sum(real_price)) %>%
  ungroup() %>%
  filter((total_order_cost <  quantile(total_order_cost, 0.05)) | 
          (total_order_cost > quantile(total_order_cost, 0.95))) %>%
  distinct(customer_id) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  select(customer_id, first_name, last_name) 

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- zad7



####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

# Założyłem, że w zadaniu rozróżniamy kwartały w różnych latach:
zad8 <- df_orders %>%
  filter(!is.null(order_date)) %>% 
  mutate(year = as.numeric(substr(order_date, 1, 4)),
         month =  as.numeric(substr(order_date, 6, 7)),
         day = as.numeric(substr(order_date, 9, 10))) %>% 
  mutate(quarter=(month-1) %/% 3 + 1) %>%
  group_by(year, quarter, day) %>%
  summarize(order_count = n(), .groups = "keep") %>%
  group_by(year, quarter) %>% 
  summarize(min_num_of_orders = min(order_count), 
            max_num_of_orders = max(order_count), 
            median_num_of_orders = median(order_count)) %>% 
  ungroup()

# Alternatywna wersja, w której nie rozróżniamy danego kwartału w różnych latach.
zad8_inaczej <- df_orders %>% 
  filter(!is.null(order_date)) %>% 
  mutate(month = as.numeric(substr(order_date, 6, 7)),
         day = as.numeric(substr(order_date, 9, 10))) %>% 
  mutate(quarter = (month-1) %/% 3 + 1) %>%
  group_by(quarter, day) %>%
  summarise(order_count = n()) %>% 
  group_by(quarter) %>% 
  summarise(min_num_of_orders = min(order_count),
            max_num_of_orders = max(order_count),
            median_num_of_orders = median(order_count))

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- zad8


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

zad9 <-df_orders %>%
  inner_join(df_customers,by="customer_id") %>%
  mutate(year = substr(order_date, 1, 4),
         time = as.numeric(as.Date(shipped_date)-as.Date(order_date))) %>%
  group_by(state, year) %>%
  summarise(mean_time = paste(format(mean(time, na.rm=TRUE), digits=3), "days"))%>%
  pivot_wider(names_from = state, values_from = mean_time)

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- zad9



####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

zad10 <- df_orders %>% 
  mutate(year=as.numeric(substr(order_date, 1, 4))) %>% 
  group_by(year, customer_id) %>% 
  summarise(customer_orders_each_year = n()) %>% 
  filter(customer_orders_each_year > 0) %>% 
  group_by(customer_id) %>% 
  summarise(customer_year_sum = sum(year)) %>% 
  filter(customer_year_sum == 2016+2017+2018) %>% 
  inner_join(df_customers, by="customer_id") %>% 
  mutate(first_letter = substr(last_name, 1, 1)) %>% 
  group_by(first_letter) %>% 
  summarize(last_names_count = n()) %>% 
  arrange(-last_names_count)

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- zad10



####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

zad11a <- df_orders %>% 
  select(order_id, order_date, customer_id) %>%
  inner_join(df_order_items %>%
               select(order_id, product_id, quantity), by = "order_id") %>%
  inner_join(df_products %>%
               select(product_id, category_id, model_year), by = "product_id") %>%
  inner_join(df_categories, by = "category_id") 
zad11 <- inner_join(zad11a %>% 
                    group_by(customer_id, category_name) %>% 
                    summarise(total_quantity = sum(quantity)) %>%
    pivot_wider(names_from=category_name, values_from=total_quantity, values_fill=0),
  zad11a %>% group_by(customer_id) %>%
    summarise(new_product_purchase= sum(quantity * ifelse(substr(order_date, 1, 4) == model_year,1,0))),
  by = "customer_id") %>%
  inner_join(df_customers %>%
               select(customer_id, first_name, last_name),
             by = "customer_id") %>%
  rowwise() %>% 
  mutate(full_name = paste(first_name, last_name, collapse = " ")) %>% 
  select(-c(first_name, last_name)) %>%
  relocate(full_name, .after = customer_id) %>%
  relocate(new_product_purchase, .after = full_name)

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- zad11



### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

zad12 <- df_orders %>% 
  select(order_id, order_date) %>% 
  inner_join(df_order_items %>% 
               mutate(discount = discount*100) %>% 
               select(order_id, discount),
             by="order_id") %>% 
  mutate(order_date=as.Date(order_date)) %>% 
  mutate(weekday=weekdays(order_date)) %>% 
  group_by(weekday) %>% 
  summarise(mean_discount = paste(format(mean(discount), digits = 4), "%"))
  zad12$weekday <- factor(zad12$weekday, 
                       levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))
  zad12 <- zad12 %>%
    arrange(weekday)

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- zad12



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "OsinskiKrzysztof.rds")
