library(dplyr)
library(tidyr)


df_orders <- read.csv("C:\\Users\\kubam\\OneDrive\\Pulpit\\dane\\orders.csv")
df_order_items <- read.csv("C:\\Users\\kubam\\OneDrive\\Pulpit\\dane\\order_items.csv")
df_products <- read.csv("C:\\Users\\kubam\\OneDrive\\Pulpit\\dane\\products.csv")
df_brands <- read.csv("C:\\Users\\kubam\\OneDrive\\Pulpit\\dane\\brands.csv")
df_categories <-  read.csv("C:\\Users\\kubam\\OneDrive\\Pulpit\\dane\\categories.csv")
df_customers <- read.csv("C:\\Users\\kubam\\OneDrive\\Pulpit\\dane\\customers.csv")

#head(df_orders)
#head(df_order_items)
#head(df_products)
#head(df_brands)
#head(df_categories)

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

x1 <- merge(df_orders, df_order_items, by = "order_id")
x1 <- merge(x1, df_products, by = "product_id")
x1 <- merge(x1, df_customers, by = "customer_id")

x1$order_date <- as.Date(x1$order_date)
x1$quarter <- quarters(x1$order_date)
x1$year <- format(x1$order_date, "%Y")

xyz1 <- x1 %>%
  group_by(state, year, quarter, product_id, product_name, model_year) %>%
  summarise(amount = sum(quantity))

ANS_TASK_01 <- xyz1 %>%
  group_by(state, year, quarter) %>%
  top_n(1, amount) %>%
  select(year, state, quarter, product_name, amount)

#ANS_TASK_01

####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

ANS_TASK_02 <- df_orders %>%
  mutate(
    year_month = format(as.Date(order_date), "%Y-%m"), niezreal = ifelse(order_status != 4, 1, 0)) %>%
  group_by(year_month) %>%
  summarise(total = n(), notdone = sum(niezreal),
    procent_niezrealizowanych = (notdone / total) * 100)%>%
  select(year_month, procent_niezrealizowanych)

#ANS_TASK_02


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

x3 <- merge(df_order_items, df_orders, by = "order_id")
x3 <- merge(x3, df_products, by = "product_id")

x3 <- x3 %>%
  mutate(
    year = format(as.Date(order_date), "%Y"),
    przychod = quantity * (list_price.x * (1 - discount))) %>%
  group_by(year, product_id, product_name) %>%
  summarise(total_value = sum(przychod))

ANS_TASK_03 <- x3 %>%
  group_by(year) %>%
  top_n(1, total_value) %>%
  select(year, product_name, total_value)

#ANS_TASK_03


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

ANS_TASK_04 <- df_orders %>%
  mutate(year = format(as.Date(order_date), "%Y")) %>%
  group_by(year, customer_id) %>%
  summarise(total_orders = n(), .groups = 'drop') %>%
  group_by(year) %>%
  summarise(
    how_many_orders = max(total_orders),
    how_many_people = sum(total_orders == how_many_orders))

#ANS_TASK_04 


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?


ANS_TASK_05 <- df_orders %>%
  inner_join(df_customers, by = "customer_id") %>%
  mutate(year = format(as.Date(order_date), "%Y"),
         mail = sub(".*@", "", email)) %>% 
  group_by(year, mail) %>% 
  summarise(number_of_orders = n()) %>% 
  group_by(year) %>%  
  top_n(1, number_of_orders)

#ANS_TASK_05


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

aktywni <- df_customers %>%
  filter(state == "CA" | state == "TX") %>%
  nrow()

#?anti_join

nieaktywni <- df_customers %>%
  filter(state == "CA" | state == "TX") %>% 
  anti_join(df_orders %>% filter(format(as.Date(order_date), "%Y") == "2018") %>% 
  select(customer_id)) %>% 
  nrow()            

czy_byli_nieaktywni <- ifelse(nieaktywni > 0, "tak", "nie")

ANS_TASK_06 <- c(aktywni, czy_byli_nieaktywni)

#ANS_TASK_06


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

#?quantile

x7 <- df_order_items %>%
  summarise(
    kwantyl_5 = quantile(quantity * (list_price * (1 - discount)), 0.05),
    kwantyl_95 = quantile(quantity * (list_price * (1 - discount)), 0.95)
  )


ANS_TASK_07 <- df_orders %>%
  inner_join(df_order_items, by = "order_id") %>% 
  mutate(total_value = quantity * (list_price * (1 - discount))) %>%
  filter(total_value < x7$kwantyl_5 | total_value > x7$kwantyl_95) %>%
  select(customer_id) %>%
  distinct()

#ANS_TASK_07


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.


x8 <- df_orders %>%
  group_by(order_date = as.Date(order_date)) %>%
  summarise(order_count = n())

ANS_TASK_08 <- x8 %>%
  mutate(year = format(as.Date(order_date), "%Y"),
         quarter = floor(as.integer(format(order_date, "%m")) / 4) + 1) %>% 
  group_by(year, quarter) %>%
  summarise(
    max_orders = max(order_count, na.rm = TRUE),
    min_orders = min(order_count, na.rm = TRUE),
    med_orders = median(order_count, na.rm = TRUE))

#ANS_TASK_08


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie


ANS_TASK_09 <- df_orders %>%
  mutate(order_date = as.Date(order_date),
         shipped_date = as.Date(shipped_date))%>%
  inner_join(df_customers, by = "customer_id") %>%
  group_by(year = format(order_date, "%Y"), state) %>%
  summarise(czas_dostawy = mean(difftime(shipped_date, order_date), na.rm = TRUE))%>%
  pivot_wider(names_from = state, values_from = czas_dostawy)

#ANS_TASK_09


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.


#?substr

ANS_TASK_10 <- df_orders %>%
  mutate(order_year = format(as.Date(order_date), "%Y")) %>%
  group_by(customer_id) %>%
  filter(n_distinct(order_year) == 3) %>% 
  inner_join(df_customers) %>%  
  summarise(letter = unique(substr(last_name, 1, 1))) %>% 
  count(letter) 

#ANS_TASK_10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)


ANS_TASK_11 <- df_orders %>%
  inner_join(df_order_items, by = "order_id") %>%
  inner_join(df_products, by = "product_id") %>%
  inner_join(df_categories, by = "category_id") %>%
  group_by(customer_id, category_name) %>%
  summarise(ilosc = n(), 'New Bicycles' = sum(model_year == format(Sys.Date(), "%Y"))) %>%
  pivot_wider(names_from = category_name, values_from = ilosc, values_fill = 0)

#ANS_TASK_11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat


#?weekdays

ANS_TASK_12 <- df_orders %>%
  inner_join(df_order_items) %>%
  inner_join(df_products) %>%
  mutate(przecena = discount * 100, day_of_week = weekdays(as.Date(order_date))) %>%
  group_by(product_id, day_of_week) %>%
  summarise(avg_discount = mean(przecena, na.rm = TRUE))

#ANS_TASK_12


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "MiszczakJakub.rds")
