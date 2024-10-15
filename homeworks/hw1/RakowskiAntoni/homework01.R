library(dplyr)
library(tidyr)


df_orders <- read.csv('orders.csv')
df_order_items <- read.csv('order_items.csv')
df_products <- read.csv('products.csv')
df_brands <- read.csv('brands.csv')
df_categories <-  read.csv('categories.csv')
df_staffs <-  read.csv('staffs.csv')
df_stocks <-  read.csv('stocks.csv')
df_stores <-  read.csv('stores.csv')
df_customers <- read.csv('customers.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na
# stany z których pochodzą klienci?
# Podaj nazwę produktu i rok jego produkcji.

tmp1 <- df_orders %>%
  transmute(order_date = (as.POSIXct(order_date, format = "%Y-%m-%d"))) %>%
  transmute(year = format(order_date, "%Y"),
            month = as.numeric(format(order_date, "%m")),
            quarter = case_when(month %in% 1:3 ~ 1, month %in% 4:6 ~ 2,
                                month %in% 7:9 ~ 3,
                                month %in% 10:12 ~ 4)) %>%
  transmute(year_quarter = paste(year, quarter, sep = "-Q")) %>%
  bind_cols(df_orders) %>%
  inner_join(df_order_items, by = "order_id") %>%
  select(year_quarter, store_id, product_id, quantity) %>%
  group_by(year_quarter, store_id, product_id) %>%
  summarise(quantity = sum(quantity)) %>%
  group_by(year_quarter, store_id) %>%
  filter(quantity == max(quantity)) %>%
  ungroup()

tmp2 <- df_products %>%
  select(product_id, product_name, model_year)

tmp3 <- left_join(tmp1, tmp2, by = "product_id") %>%
  select(-quantity, -product_id) %>%
  left_join(df_stores[, c("store_id", "state")], by = "store_id") %>%
  select(year_quarter, state, product_name, model_year)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- tmp3


####### Zadanie 2
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu?
tmp1 <- df_orders %>%
  select(order_date, shipped_date) %>%
  mutate(order_date = as.POSIXct(order_date),
         year = format(order_date, "%Y"),
         month = format(order_date, "%m"),
         cancelled = case_when(shipped_date == "NULL" ~ 1,
                               TRUE ~ 0)) %>%
  select(year, month, cancelled) %>%
  group_by(year, month) %>%
  summarise(percent_cancelled = sum(cancelled) / n() * 100) %>%
  ungroup()

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- tmp1


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
tmp1 <- df_orders %>%
  inner_join(df_order_items, by = "order_id") %>%
  select(order_date, product_id, quantity, list_price, discount) %>%
  mutate(year = format(as.POSIXct(order_date), "%Y"),
         amount = round(quantity * list_price * (1 - discount), 2)) %>%
  select(year, product_id, amount) %>%
  group_by(year, product_id) %>%
  summarise(sum = sum(amount)) %>%
  group_by(year) %>%
  filter(sum == max(sum)) %>%
  ungroup() %>%
  left_join(df_products[, c("product_id", "product_name")],
            by = "product_id") %>%
  select(year, product_name)

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- tmp1


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień)
## w kazdym roku i ile to było zamówień?

tmp1 <- df_orders %>%
  mutate(year = format(as.POSIXct(order_date), "%Y")) %>%
  select(year, customer_id) %>%
  group_by(year, customer_id) %>%
  summarise(number_of_orders = n()) %>%
  group_by(year) %>%
  filter(number_of_orders == max(number_of_orders)) %>%
  mutate(number_of_customers = n()) %>%
  select(-customer_id) %>%
  unique() %>%
  ungroup()

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- tmp1


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku?
## Ile było tych zamówień?

tmp1 <- df_customers %>%
  mutate(domain = sub(".*@", "", email)) %>%
  select(customer_id, domain)

tmp2 <- df_orders %>%
  left_join(tmp1, by = "customer_id") %>%
  mutate(year = format(as.POSIXct(order_date), "%Y")) %>%
  select(year, domain) %>%
  group_by(year, domain) %>%
  summarise(orders_from_domain = n()) %>%
  group_by(year) %>%
  filter(orders_from_domain == max(orders_from_domain)) %>%
  ungroup()

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- tmp2


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie?
# Czy w bazie klientów z tych stanów są tacy,
# którzy nie zrobili żadnego zamówienia w 2018?

tmp1 <- unique(df_orders$customer_id)

tmp2 <- df_customers %>%
  filter(state %in% c("TX", "CA")) %>%
  mutate(is_active = case_when(customer_id %in% tmp1 ~ 1,
                               TRUE ~ 0)) %>%
  group_by(state) %>%
  summarise(active = sum(is_active),
            inactive = n() - sum(is_active))

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- tmp2


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn.
## wydali w jednym zamówieniu poniżej 5 kwantyla
## lub więcej niż 95 kwantyl wartosci zamówienia?

tmp1 <- df_orders %>%
  inner_join(df_order_items, by = "order_id") %>%
  mutate(amount = round(quantity * list_price * (1 - discount), 2)) %>%
  group_by(customer_id, order_id) %>%
  summarise(amount = sum(amount))

tmp2 <- quantile(tmp1$amount, 0.05)
tmp3 <- quantile(tmp1$amount, 0.95)

tmp4 <- tmp1 %>%
  filter(amount > tmp3 | amount < tmp2) %>%
  select(customer_id) %>%
  unique() %>%
  left_join(df_customers)

## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- tmp4


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień
# złożonych każdego dnia w poszczególnych kwartalach.

tmp1 <- df_orders %>%
  mutate(order_date = (as.POSIXct(order_date, format = "%Y-%m-%d"))) %>%
  mutate(year = format(order_date, "%Y"),
         day = format(order_date, "%d"),
         month = as.numeric(format(order_date, "%m")),
         quarter = case_when(month %in% 1:3 ~ 1, month %in% 4:6 ~ 2,
                             month %in% 7:9 ~ 3,
                             month %in% 10:12 ~ 4)) %>%
  group_by(year, quarter, day) %>%
  summarise(orders = n()) %>%
  group_by(year, quarter) %>%
  summarise(min = min(orders), max = max(orders), median = median(orders)) %>%
  ungroup()


## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- tmp1


####### Zadanie 9
# Jaki był średni czas dostarczania zamówienia w zależności od roku i
# stanu w którym mieszkał klient.
# Jako rozwiązanie przygotuj szeroka postac tabeli,
# która będzie miała informację o każdym stanie w innej kolumnie

tmp1 <- df_orders %>%
  left_join(select(df_stores, store_id, state), by = "store_id") %>%
  filter(shipped_date != "NULL") %>%
  mutate(order_date = as.POSIXct(order_date),
         shipped_date = as.POSIXct(shipped_date),
         delivery_time = round(shipped_date - order_date),
         year = format(order_date, "%Y")) %>%
  group_by(year, state) %>%
  summarise(mean_delivery = mean(delivery_time)) %>%
  ungroup() %>%
  pivot_wider(names_from = state, values_from = mean_delivery)

## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- tmp1


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów,
# którzy robili zamówienia co roku.
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

tmp1 <- df_orders %>%
  left_join(select(df_customers, customer_id, last_name),
            by = "customer_id") %>%
  mutate(year = format(as.POSIXct(order_date), "%Y")) %>%
  group_by(last_name) %>%
  summarise(n = n_distinct(year)) %>%
  filter(n == 3) %>%
  mutate(first_letter = substr(last_name, 1, 1)) %>%
  group_by(first_letter) %>%
  summarise(amount = n())

## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- tmp1


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy
# każdy klient kupił rower każdej kategorii.
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt
# (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

tmp1 <- df_order_items %>%
  left_join(select(df_orders, order_id, order_date, customer_id),
            by = "order_id") %>%
  left_join(select(df_products, product_id, category_id, model_year),
            by = "product_id") %>%
  mutate(year = format(as.POSIXct(order_date), "%Y"),
         new_model = case_when(year == model_year ~ 1, TRUE ~ 0)) %>%
  select(customer_id, quantity, category_id, new_model)

tmp2 <- tmp1 %>%
  group_by(customer_id, category_id) %>%
  summarise(quantity = sum(quantity)) %>%
  ungroup() %>%
  left_join(df_categories, by = "category_id") %>%
  select(customer_id, category_name, quantity) %>%
  pivot_wider(names_from = category_name, values_from = quantity,
              values_fill = 0)

tmp3 <- tmp1 %>%
  mutate(new_model = new_model * quantity) %>%
  group_by(customer_id) %>%
  summarise(new_model = sum(new_model))

tmp4 <- inner_join(tmp2, tmp3, by = "customer_id")

## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- tmp4


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym
# z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

tmp1 <- df_order_items$quantity * df_order_items$list_price

tmp2 <- df_order_items %>%
  left_join(select(df_orders, order_id, order_date), by = "order_id") %>%
  mutate(day = format(as.POSIXct(order_date), "%A"),
         discount = tmp1 - tmp1 * (1 - discount)) %>%
  group_by(product_id, day) %>%
  summarise(mean_discount = round(mean(discount), 2)) %>%
  ungroup() %>%
  left_join(select(df_products, product_id, product_name)) %>%
  select(product_id, product_name, day, mean_discount)

## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- tmp2


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                 ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                     "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "RakowskiAntoni.rds")
