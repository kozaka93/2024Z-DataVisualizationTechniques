library(dplyr)
library(tidyr)

df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <-  read.csv('homeworks/hw1/dane/customers.csv')
df_staffs <-  read.csv('homeworks/hw1/dane/staffs.csv')
df_stocks <-  read.csv('homeworks/hw1/dane/stocks.csv')
df_stores <-  read.csv('homeworks/hw1/dane/stores.csv')

file.exists("C:/Users/Liwia/Desktop/homeworks/hw1/dane/orders.csv")
file.exists("homeworks/hw1/dane/orders.csv")

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.

all_info_df <- df_products %>%
  # Wybieranie ze wszystkich ramek potrzebnych informacji
  select(c("product_id", "product_name", "model_year")) %>% 
  merge(df_order_items[, c("product_id", "quantity", "order_id")], by = "product_id") %>% 
  merge(df_orders[, c("order_id", "order_date", "customer_id")], by = "order_id") %>% 
  merge(df_customers[, c("customer_id", "state")], by = "customer_id") %>% 
  select(c("product_id", "quantity", "order_date", "state", "product_name", "model_year")) %>%
  mutate(month_value_numeric = as.numeric(substr(order_date, 6, 7))) %>% # dodanie kolumny z miesiącem
  mutate(
    quarter = case_when(
      month_value_numeric %in% c(1, 2, 3) ~ "Q1",
      month_value_numeric %in% c(4, 5, 6) ~ "Q2",
      month_value_numeric %in% c(7, 8, 9) ~ "Q3",
      month_value_numeric %in% c(10, 11, 12) ~ "Q4"
    )
  ) %>% # Dodanie kolumny z kwartałem
  group_by(state, quarter, product_id, product_name, model_year) %>% # Grupowanie
  summarise(total_quantity = sum(quantity, na.rm = TRUE), .groups = "drop") # Tworzenie total quantity dla każdego produktu w danym stanie i kwartale

top_products <- all_info_df %>%
  group_by(state, quarter) %>%  # Grupowanie po stanie i kwartale
  slice_max(total_quantity, n = 1, with_ties = FALSE) %>%  # Wybór maksymalnej ilości
  ungroup() %>%  # Usunięcie grupowania 
  select(c("product_name", "model_year"))

## Odpowiedz przypisana do zmiennej
ANS_TASK_01 <- top_products


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

# Zliczanie zamówień o statusie 1 (pending) lub 3 (rejected) 
unsuccessful_orders_df <- df_orders %>% 
  select(c("order_date", "order_status")) %>% # Wyberanie daty i statusu
  mutate(year_value_numeric = as.numeric(substr(order_date, 1, 4))) %>% # dodanie kolumny z rokiem
  mutate(month_value_numeric = as.numeric(substr(order_date, 6, 7))) %>% # dodanie kolumny z miesiącem
  select(c("year_value_numeric", "month_value_numeric", "order_status")) %>% 
  group_by(year_value_numeric, month_value_numeric) %>% 
  summarise(
    total_orders = n(),  # Liczenie wszystkich zamówień
    total_unsuccessful = sum(order_status %in% c(1, 3)),  # Liczenie niezrealizowanych zamówień
    .groups = "drop"
  ) %>% 
  mutate(percent_unsuccessful = total_unsuccessful * 100/ total_orders) %>% 
  select(c("year_value_numeric", "month_value_numeric", "percent_unsuccessful"))

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- unsuccessful_orders_df


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

max_profit_by_year <- df_order_items %>% 
  select(c("order_id", "quantity", "list_price", "product_id")) %>% 
  merge(df_orders[, c("order_id", "order_date")], by = "order_id") %>%  # Wybieranie potrzebnych informacji
  mutate(year_value_numeric = as.numeric(substr(order_date, 1, 4))) %>% # Tworzenie kolumny z rokiem
  group_by(product_id, year_value_numeric) %>% 
  mutate(sum_price = quantity*list_price) %>% # Tworzenie z klumn z ilością produktu i ceną za jeden, ceny za całość
  select(-c("order_id", "order_date", "list_price", "quantity")) %>% 
  summarise(total_sum = sum(sum_price, na.rm = TRUE), .groups = "drop") %>% # Sumowanie cen
  group_by(year_value_numeric) %>% 
  filter(total_sum == max(total_sum)) %>%  # Wybór wiersza z maksymalnym przychodem w danym roku
  ungroup() 


## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- max_profit_by_year


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

customer_most_orders <- df_orders %>% 
  select(c("customer_id", "order_date", "order_id")) %>% 
  mutate(year_value_numeric = as.numeric(substr(order_date, 1, 4))) %>% # Tworzenie kolumny z rokiem
  group_by(year_value_numeric, customer_id) %>% 
  summarise(sum_orders = n(), .groups = "drop") %>%  # Zliczanie liczby zamówień
  group_by(year_value_numeric) %>% 
  filter(sum_orders == max(sum_orders)) %>%  # Wybór klientów z maksymalną liczbą zamówień w każdym roku
  ungroup()


## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- customer_most_orders


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

most_freq_mail <- df_orders %>% 
  select(c("customer_id", "order_date")) %>% 
  merge(df_customers[, c("customer_id", "email")]) %>% 
  mutate(year_value_numeric = as.numeric(substr(order_date, 1, 4))) %>% # Tworzenie kolumny z rokiem
  group_by(year_value_numeric, email) %>% 
  summarise(orders_sum = n(), .groups = "drop") %>%   # Zliczanie liczby uzycia danego emaila
  group_by(year_value_numeric) %>% 
  filter(orders_sum == max(orders_sum)) %>%  # Wybór klientów z maksymalną liczbą zamówień w każdym roku
  ungroup()


## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- most_freq_mail


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

# Wszyscy aktywni klienci tych stanów
active_CA_TX <- df_customers %>% 
  select(c("customer_id", "state")) %>% 
  filter(state %in% c("CA", "TX"))

# Liczba aktywnych klientów
n_active_CA_TX <- active_CA_TX %>% 
  group_by(state) %>% 
  summarise(active_count = n(), .groups = "drop")

# Klijenci którzy wykonali zamówienie w 2018
customers_with_orders_2018 <- df_orders %>%
  filter(substr(order_date, 1, 4) == "2018") %>%
  select(customer_id)

# Klieci którzy nie wykonali zamówienia w 2018
inactive_customers_2018 <- active_CA_TX %>%
  anti_join(customers_with_orders_2018, by = "customer_id")

# Liczba klientów, którzy nie wykonali zamówienia w 2018
n_inactive_customers_2018 <- inactive_customers_2018 %>%
  group_by(state) %>%
  summarise(inactive_count_2018 = n(), .groups = "drop")

# Połączenie liczby aktywnych klientów z liczbą klientów bez zamówienia w 2018
n_customers_CA_TX <- n_active_CA_TX %>% 
  merge(n_inactive_customers_2018, by = "state")


## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- n_customers_CA_TX


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

all_clients <- df_order_items %>% 
  select(c("order_id", "quantity", "list_price", "discount")) %>% 
  merge(df_orders[, c("order_id", "customer_id")], by = "order_id") %>% 
  merge(df_customers[, c("customer_id")]) %>% 
  select(-"y") %>% 
  mutate(sum_price = list_price *quantity) %>% # Dodanie kolumny z ceną za wszystkie produkty
  mutate(price_after_dis = sum_price - sum_price*discount) %>% # Obliczenie ceny po zniżce
  select(c("order_id", "customer_id", "sum_price", "price_after_dis")) %>% 
  group_by(order_id, customer_id) %>%
  summarise(
    total_sum_price = sum(sum_price, na.rm = TRUE),  # Suma ceny zamówienia
    total_price_after_dis = sum(price_after_dis, na.rm = TRUE),  # Suma ceny zapłaconej
    .groups = "drop"
  ) %>% 
  mutate(total_discount = (1 - (total_price_after_dis/ total_sum_price))*100 ) # Obliczenie zniżki zamówienia

quantiles <- quantile(all_clients$total_discount, probs = c(0.05, 0.95), na.rm = TRUE) # Obliczenie wartości kwantyli

fifth_quantile <- quantiles[1]  
ninety_fifth_quantile <- quantiles[2]

extreme_clients <- all_clients %>% 
  filter(total_discount < fifth_quantile | total_discount > ninety_fifth_quantile) %>% 
  select("customer_id") # Wybranie id ekstremalnych klientów


## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- extreme_clients


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

quarter_orders_count <- df_orders %>% 
  select(c("order_id", "order_date")) %>% 
  mutate(
    quarter = case_when(
      as.numeric(substr(order_date, 6, 7)) %in% c(1, 2, 3) ~ "Q1",
      as.numeric(substr(order_date, 6, 7)) %in% c(4, 5, 6) ~ "Q2",
      as.numeric(substr(order_date, 6, 7)) %in% c(7, 8, 9) ~ "Q3",
      as.numeric(substr(order_date, 6, 7)) %in% c(10, 11, 12) ~ "Q4"
    )
  ) %>% # Dodanie kolumny z kwartałem
  group_by(order_date, quarter) %>% 
  summarise(daily_orders = n(), .groups = "drop") %>% # Zliczanie zamówień każdego dnia
  group_by(quarter) %>% 
  summarise(
    max_orders = max(daily_orders, na.rm = TRUE),
    min_orders = min(daily_orders, na.rm = TRUE),
    median_orders = median(daily_orders, na.rm = TRUE),
    .groups = "drop"
  ) # Dodanie wartości maksymalnej i minimalnej oraz mediany dla każdego kwartału
  

## Odpowiedz przypisana do zmiennej
ANS_TASK_08 <- quarter_orders_count


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

avg_time_order <- df_orders %>% 
  select(c("order_date", "shipped_date", "customer_id")) %>%  
  merge(df_customers[, c("customer_id", "state")], by = "customer_id") %>% 
  mutate(year_value_numeric = as.numeric(substr(order_date, 1, 4))) %>% # Tworzenie kolumny z rokiem
  filter(!is.na(order_date) & !is.na(shipped_date)) %>% # Usuwanie wartości NA
  mutate(
    order_date = as.Date(order_date, format = "%Y-%m-%d"),  # Konwersja do formatu Date
    shipped_date = as.Date(shipped_date, format = "%Y-%m-%d"),  # Konwersja do formatu Date
    delivery_time = difftime(shipped_date, order_date, units = "days")  # Obliczenie czasu dostarczenia
  ) %>% 
  filter(!is.na(delivery_time)) %>% # Ponowne usuwanie wartości NA
  group_by(year_value_numeric, state) %>% 
  summarise(avg_delivery_time = mean(delivery_time, na.rm = TRUE), .groups = "drop") %>% # Obliczenie średniego czasu dostawy
  pivot_wider(names_from = state, values_from = avg_delivery_time) # Zmiana w szeroką postać tabeli


## Odpowiedz przypisana do zmiennej
ANS_TASK_09 <- avg_time_order


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

clients_with_orders <- df_orders %>%
  merge(df_customers[, c("customer_id", "last_name")], by = "customer_id") %>%
  mutate(year = as.numeric(substr(order_date, 1, 4))) %>% # Dodanie kolumny z rokiem
  select(c("last_name", "year")) %>% 
  unique() %>% # Wybranie tylko unikalnych wierszy
  group_by(last_name) %>%
  summarise(count = n(), .groups = "drop") %>% # Policzenie ile wierszy przyporządkowanych jest do każdego klienta
  filter(count >= 3) # Wybranie klientów którzy złożyli zamówienie w każdym z 3 lat

letter_frequency <- clients_with_orders %>%
  mutate(first_letter = substr(last_name, 1, 1)) %>% # Pobierz pierwszą literę nazwiska
  group_by(first_letter) %>%
  summarise(count = n(), .groups = "drop") %>% # Liczenie wystapień każdej litery
  arrange(desc(count)) # Sortowanie według częstości występowania


## Odpowiedz przypisana do zmiennej
ANS_TASK_10 <- letter_frequency


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

bikes_and_clients <- df_orders %>% 
  select(c("order_id", "customer_id", "order_date")) %>% 
  merge(df_order_items[, c("order_id", "product_id", "quantity")], by = "order_id") %>% 
  merge(df_products[, c("product_id", "category_id", "model_year")]) %>% 
  select(-c("order_id", "product_id")) %>% 
  mutate(
    order_year = as.numeric(substr(order_date, 1, 4)),  # Tworzenie kolumny z rokiem
    is_newest = ifelse(model_year == order_year, TRUE, FALSE)  # Porównanie roku produkcji z rokiem zamówienia
  ) %>%
  group_by(customer_id, category_id) %>% 
  summarise(
    total_bikes = sum(quantity, na.rm = TRUE),  # Zliczanie wszystkich rowerów w kategorii
    newest_bikes = sum(is_newest * quantity, na.rm = TRUE),  # Zliczanie najnowszych rowerów
    .groups = "drop"
  ) %>% 
  pivot_wider( # Przekształcenie na szerszą wersję tabelki
    names_from = category_id, 
    values_from = total_bikes, 
    values_fill = 0 # Uzupełnienie zerami 
  )


## Odpowiedz przypisana do zmiennej
ANS_TASK_11 <- bikes_and_clients


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat

discounts_per_product_day <- df_orders %>%
  select(c("order_id", "order_date")) %>%
  merge(df_order_items[, c("order_id", "product_id", "list_price", "discount", "quantity")], by = "order_id") %>%
  mutate(
    sum_list_price = list_price * quantity,  # Obliczenie sumy ceny katalogowej
    sum_price_after_discount = sum_list_price * (1 - discount),  # Cena po rabacie
    discount_percentage = (1 - (sum_price_after_discount / sum_list_price)) * 100  # Obliczenie rabatu procentowego
  ) %>%
  mutate(
    day_of_week = weekdays(as.Date(order_date, format = "%Y-%m-%d"))  # Konwersja daty na dzień tygodnia
  ) %>%
  group_by(product_id, day_of_week) %>% 
  summarise(
    avg_discount = mean(discount_percentage, na.rm = TRUE),  # Średni rabat procentowy
    .groups = "drop"
  )


## Odpowiedz przypisana do zmiennej
ANS_TASK_12 <- discounts_per_product_day


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "NazwiskoImie.rds")