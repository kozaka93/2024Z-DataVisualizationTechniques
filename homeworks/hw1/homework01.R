# Instalacja potrzebnych pakietów
install.packages('lubridate')
install.packages('stringr')

# Ładowanie bibliotek
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Wczytanie danych
df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <- read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')
df_stores <- read.csv('homeworks/hw1/dane/stores.csv')
df_stocks <- read.csv('homeworks/hw1/dane/stocks.csv')
df_staffs <- read.csv('homeworks/hw1/dane/staffs.csv')

# Czyszczenie danych w df_orders
cleaned_df_orders <- df_orders %>%
  mutate(shipped_date = replace(shipped_date, shipped_date == "NULL", NA_character_)) %>% #pomimo róznic w semantyce NULLe będą zastąpione wartosciami NA w celu uniknięcia warningów przy konwersji stringów do dateformat
  mutate(order_date = ymd(order_date), required_date = ymd(required_date), shipped_date = ymd(shipped_date))

###############################################
# Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany, z których pochodzą klienci?
# Podaj nazwę produktu i rok jego produkcji.

# Łączenie tabel i dodanie kolumny z kwartałem zamówienia
# Grupowanie i wyliczenie najczęściej kupowanego produktu w każdym kwartale dla każdego stanu
popular_products <- cleaned_df_orders %>%
  mutate(order_quarter = quarter(order_date)) %>%
  inner_join(df_customers, by = "customer_id") %>%
  inner_join(df_order_items, by = "order_id") %>%
  inner_join(df_products, by = "product_id") %>% 
  group_by(state, order_quarter, product_name, model_year) %>%
  summarise(total_quantity = sum(quantity), .groups = 'drop') %>%
  arrange(state, order_quarter, desc(total_quantity)) %>%
  group_by(state, order_quarter) %>%
  slice(1)  # Wybieramy produkt, który był najczęściej kupowany w każdym kwartale i stanie

# Wynik
ANS_TASK_01 <- popular_products

###############################################
# Zadanie 2
# Jaki procent wszystkich zamówień nie został zrealizowany w każdym miesiącu?

# Grupowanie po miesiącach i wyliczenie procentu niezrealizowanych zamówień (brak daty wysyłki)
unfullfilled <- cleaned_df_orders %>%
  mutate(order_month = floor_date(order_date, "month")) %>%  # Ekstrakcja miesiąca
  group_by(order_month) %>%
  summarise(
    total_orders = n(),
    unfulfilled_orders = sum(is.na(shipped_date)),
    percent_unfulfilled = (unfulfilled_orders / total_orders) * 100
  ) %>%
  arrange(order_month)

# Wynik
ANS_TASK_02 <- unfullfilled

###############################################
# Zadanie 3
# Jaki produkt przyniósł największy przychód w każdym roku?

# Łączenie tabel orders, order_items, i products oraz dodanie kolumny z rokiem zamówienia
# Grupowanie po roku i produkcie, aby znaleźć produkt z największym przychodem w każdym roku
highest_profit<- cleaned_df_orders %>%
  inner_join(df_order_items, by = "order_id") %>%
  inner_join(df_products, by = "product_id") %>%
  mutate(order_year = year(order_date), revenue = quantity * list_price.y * (1 - discount)) %>% # Wyliczenie przychodu dla każdego zamówienia
  group_by(order_year, product_name) %>%
  summarise(total_revenue = sum(revenue), .groups = 'drop') %>%
  arrange(order_year, desc(total_revenue)) %>%
  group_by(order_year) %>%
  slice(1)  # Wybieramy produkt z największym przychodem w każdym roku

# Wynik
ANS_TASK_03 <- highest_profit

###############################################
# Zadanie 4
# Ile klientów zrobiło największe zakupy (najwięcej zamówień) w każdym roku i ile to było zamówień?

# Grupowanie po kliencie i roku, aby wyliczyć liczbę zamówień na klienta
# Znalezienie klientów, którzy złożyli największą liczbę zamówień w każdym roku
customer_orders_per_year <- cleaned_df_orders %>%
  mutate(order_year = year(order_date)) %>%
  group_by(order_year, customer_id) %>%
  summarise(total_orders = n(), .groups = 'drop') %>%
  arrange(order_year, desc(total_orders)) %>%
  group_by(order_year) %>%
  slice_max(total_orders, with_ties = TRUE) %>%  # Wybór klientów z maksymalną liczbą zamówień
  summarise(customer_count = n(), max_orders = first(total_orders))

# Wynik
ANS_TASK_04 <- customer_orders_per_year 

###############################################
# Zadanie 5
# Z jakiej domeny mailowej najczęściej robiono zamówienia w każdym roku? Ile było tych zamówień?

# Łączenie zamówień z klientami, wyciągnięcie domeny email i wyliczenie liczby zamówień dla każdej domeny w każdym roku
popular_emails <- cleaned_df_orders %>%
    left_join(df_customers, by = "customer_id") %>% 
    mutate(order_year = year(order_date),
           email_domain = str_extract(email, "@[a-zA-Z0-9.-]+$")) %>% #do znalezienia domen mailowych posłużyliśmy się regexem i pakietem stringr
    group_by(order_year, email_domain) %>%
    summarise(total_orders = n(), .groups = 'drop') %>%
    arrange(order_year, desc(total_orders)) %>%
    group_by(order_year) %>%
    slice_max(total_orders, with_ties = FALSE)  # Wybór domeny z największą liczbą zamówień

# Wynik 
ANS_TASK_05 <- popular_emails

###############################################
# Zadanie 6
# Ilu klientów z Kalifornii i Teksasu było aktywnych? Czy są klienci, którzy nie złożyli żadnego zamówienia w 2018? 


# Filtrowanie klientów z Kalifornii i Teksasu (gdzie "i" z polecania jest interpretowane jako operacja logiczna XOR)
california_texas_customers <- df_customers %>%
  filter(state %in% c("CA", "TX"))

# Łączenie klientów z zamówieniami z 2018 roku
customers_with_orders_2018 <- cleaned_df_orders %>%
  filter(year(ymd(order_date)) == 2018) %>%
  inner_join(california_texas_customers, by = "customer_id")

# Znalezienie klientów, którzy nie złożyli żadnego zamówienia w 2018
inactive_customers <- california_texas_customers %>%
  anti_join(customers_with_orders_2018, by = "customer_id")

# Zliczenie aktywnych klientów
active_customers_count <- customers_with_orders_2018 %>%
  distinct(customer_id) %>%
  count()

# Wynik
ANS_TASK_06 <- list(
  active_customers_count = active_customers_count,
  inactive_customers = inactive_customers
)

#wynik to lista. Pierwszy element to mały dataframe zawierający określający liczbę aktywntych klientów z Kalifornii i Teksasu, a drugi to dataframe zawierający informacje na temat nieaktywnych użytkowników
str(ANS_TASK_06)

###############################################
# Zadanie 7
# Którzy klienci wydali w jednym zamówieniu mniej niż 5-ty kwantyl lub więcej niż 95-ty kwantyl wartości zamówienia?

# Wyliczenie wartości całkowitej dla każdego zamówienia
order_totals <- df_order_items %>%
  mutate(total_price = quantity * list_price * (1 - discount)) %>%
  group_by(order_id) %>%
  summarise(order_total = sum(total_price))

# Znalezienie 5. i 95. percentyla
percentiles <- quantile(order_totals$order_total, probs = c(0.05, 0.95))

# Wybór zamówień poniżej 5. lub powyżej 95. percentyla
# Łączenie ekstremalnych zamówień z informacjami o klientach
extreme_orders <- order_totals %>%
  filter(order_total < percentiles[1] | order_total > percentiles[2]) %>%
  inner_join(df_orders, by = "order_id") %>%
  inner_join(df_customers, by = "customer_id") %>% 
  select(first_name, last_name, order_total) %>% 
  arrange(order_total)

#Wynik
ANS_TASK_07 <- extreme_orders 

###############################################
# Zadanie 8
# Oblicz maksymalną, minimalną oraz medianę liczby zamówień złożonych każdego dnia w poszczególnych kwartałach.

# Grupowanie po kwartale i dniu, aby policzyć liczbę zamówień
order_stats_per_quarter <- cleaned_df_orders %>%
  mutate(order_quarter = quarter(order_date)) %>% 
  group_by(order_quarter, order_date) %>%
  summarise(daily_orders = n(), .groups = 'drop') %>%
  group_by(order_quarter) %>%
  summarise(
    max_orders = max(daily_orders),
    min_orders = min(daily_orders),
    median_orders = median(daily_orders)
  )

#Wynik
ANS_TASK_08 <- order_stats_per_quarter

###############################################
# Zadanie 9
# Jaki był średni czas dostarczania zamówienia w zależności od roku i stanu, w którym mieszkał klient.
# Przygotuj szeroką postać tabeli, gdzie każda kolumna to osobny stan.

# Obliczanie czasu dostawy dla każdego zamówienia
# Łączenie zamówień z informacją o kliencie, aby uzyskać dane o stanie
orders_with_customers <- cleaned_df_orders %>%
  mutate(delivery_time = as.numeric(difftime(shipped_date, order_date, units = "days"))) %>%
  inner_join(df_customers, by = "customer_id")

# Obliczenie średniego czasu dostawy w zależności od roku i stanu
avg_delivery_time <- orders_with_customers %>%
  mutate(year = year(order_date)) %>%
  group_by(year, state) %>%
  summarise(avg_delivery_time = mean(delivery_time, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = state, values_from = avg_delivery_time)

# Wynik
ANS_TASK_09 <- avg_delivery_time

 ###############################################
# Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia każdego roku.
# Jak często występuje każda litera wśród tych nazwisk?

## Wyodrębnienie roku zamówienia
orders_with_year <- cleaned_df_orders %>%
  mutate(order_year = year(order_date))

# Znalezienie lat, w których każdy klient złożył zamówienie
customer_years <- orders_with_year %>%
  distinct(customer_id, order_year) %>%
  group_by(customer_id) %>%
  summarise(years_ordered = n_distinct(order_year))

# Klienci, którzy robili zamówienia w każdym roku
total_years <- length(unique(orders_with_year$order_year))
consistent_customers <- customer_years %>%
  filter(years_ordered == total_years) %>%
  inner_join(df_customers, by = "customer_id")

# Wyodrębnienie pierwszej litery nazwiska i policzenie częstotliwości występowania
last_name_letters <- consistent_customers %>%
  mutate(first_letter = substr(last_name, 1, 1)) %>%
  count(first_letter, sort = TRUE)

# Wynik
ANS_TASK_10 <- last_name_letters

###############################################
# Zadanie 11
# Zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii.
# Dodatkowo ile razy klient kupił najnowszy produkt (rower wyprodukowany w roku zamówienia).

# Łączenie order_items z products, orders i customers, aby uzyskać dane o kategoriach i klientach
order_items_with_products <- df_order_items %>%
  inner_join(cleaned_df_orders, by = "order_id") %>%
  inner_join(df_products, by = "product_id") %>%
  inner_join(df_categories, by = "category_id") %>%
  inner_join(df_customers, by = "customer_id")

# Liczba rowerów każdej kategorii kupionych przez każdego klienta
category_purchases <- order_items_with_products %>%
  group_by(customer_id, category_name) %>%
  summarise(category_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = category_name, values_from = category_count, values_fill = 0)


# Sprawdzenie, czy produkt był najnowszy (rok zamówienia odpowiada rokowi modelu)
latest_product_purchases <- order_items_with_products %>%
  mutate(latest_product = ifelse(year(order_date) == model_year, 1, 0)) %>%
  group_by(customer_id) %>%
  summarise(latest_count = sum(latest_product))

# Połączenie informacji o zakupach kategorii i najnowszych produktach
customer_purchases <- category_purchases %>%
  left_join(latest_product_purchases, by = "customer_id")

# Wynik
ANS_TASK_11 <- customer_purchases

###############################################
# Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?

# Dodanie informacji o dniu tygodnia dla każdego zamówienia
cleaned_df_orders <- cleaned_df_orders %>%
  mutate(order_day = wday(order_date, label = TRUE))

# Łączenie order_items z zamówieniami, aby uzyskać dane o dniu tygodnia
# Obliczanie procentowego rabatu dla każdego produktu
order_items_with_day <- df_order_items %>%
  inner_join(cleaned_df_orders, by = "order_id") %>%
  mutate(actual_revenue = quantity * list_price * (1 - discount),
         full_price = quantity * list_price,
         discount_pct = (full_price - actual_revenue) / full_price)

# Obliczanie średniego rabatu na każdy produkt w zależności od dnia tygodnia
avg_discount_by_day <- order_items_with_day %>%
  group_by(product_id, order_day) %>%
  summarise(avg_discount = mean(discount_pct, na.rm = TRUE), .groups = "drop")

# Wynik
ANS_TASK_12 <- avg_discount_by_day

### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "GwozdzMichal.rds")

