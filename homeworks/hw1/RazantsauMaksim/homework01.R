library(dplyr)
library(tidyr)
library(lubridate)

getwd()
setwd("/Users/admin/TWD/2024Z-DataVisualizationTechniques/homeworks/hw1/RazantsauMaksim")
# install.packages("lubridate")



df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.


head(df_products)
# zakladam ze najczesciej kupowany to taki/ ktorego najczesciej kupowaly razowo (rozne zamowienia)
# total_amount = n(), dlatego zwykly count
# nie ilosciowo
  df_orders_products_quarters_sum <- df_orders |>
    # filter(order_status == 4) |> # only completed orders (! nie wiem, chyba nie jest potrzebny, bo jak ktos zlozyl
    # zamowienie, to zaplacil i wykazal sie zainteresowaniem produktu)?
    mutate(quarter_year = paste0(year(as.Date(order_date)), "Q", quarter(as.Date(order_date)))) |> # order date as a date of buying
    inner_join(df_order_items, by = "order_id") |>
    inner_join(df_products, by = "product_id") |>
    inner_join(df_customers, by = "customer_id") |>
    group_by(state, quarter_year, product_name) |>
    summarise(total_amount = n(),
              model_year = first(model_year))
View(df_orders_products_quarters_sum)
  # kilka najczesciej kupowanych produktow w jedntm kwartale 
  # (maja ta sama liczbe kupowan w pewnym stanie za pewien kwartal)

# OK
ANS_TASK_01 <- df_orders_products_quarters_sum |>
  group_by(state, quarter_year) |>
  slice_max(total_amount) |>
  select(state, quarter_year, product_name, model_year, total_amount) |>
  arrange(state, quarter_year)
ANS_TASK_01
View(ANS_TASK_01)
  
#   names(df_orders_products_quarters)
#   head(df_orders_products_quarters)
#  
# head(df_orders)

## Odpowiedz przypisana do zmiennej
ANS_TASK_01


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 
# OK
ANS_TASK_02 <- df_orders |>
  mutate(month_year = format(as.Date(order_date), "%Y-%m")) |>
  group_by(month_year) |>
  summarise(not_completed_orders = (sum(order_status != 4)/n()) * 100) |>
  select(month_year, not_completed_orders)


## Odpowiedz przypisana do zmiennej
View(ANS_TASK_02)


####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?

# (Co wyróżnia produkt bardziej, jego product_id czy product_name?)
# ("Electra Cruiser 1 (24-Inch) - 2016" np jest pod roznymi product_id,
#  ale nazwa produktu ta sama)
# zakladam/ że w pytaniu chodzi o porduct_name, czyli jak jest pod innym poduct_id
# to nie ma rozincy/ chodzi o fizyczny produkt
# dlatego grupuje po product_name


# znizka na kazdy osobny produkt/ wiec licze ze znizka
# list_price z df_products oraz z df_order_items to to samo

# check <- df_products |>
#   group_by(product_name) |>
#   summarise(count = n())
# 
# df_products[df_products$product_name == "Electra Cruiser 1 (24-Inch) - 2016", ]
# df_products[df_products$product_name == "Electra Townie Go! 8i - 2017/2018", ]
# unique(check$count)
# check[check$count == 3, ]
# OK
df_merged <- df_orders |>
  mutate(year = format(as.Date(order_date), "%Y")) |>
  inner_join(df_order_items, by = "order_id") |>
  inner_join(df_products, by = "product_id") |>
  inner_join(df_customers, by = "customer_id")

# nie grupuje wedlug model_year bo trzeba po product_name
# uwzgledniam tylko zakonczone zamowienie (nie zakonczone zamowienie jeszcze nie przyniosle dochod )
# jednak nie uwzgledniam (z issue na github)
ANS_TASK_03 <- df_merged |>
  mutate(revenue = quantity * list_price.x * (1 - discount)) |>
  group_by(year, product_name) |>
  summarise(total_revenue = sum(revenue)) |>
  group_by(year) |>
  slice_max(total_revenue)

ANS_TASK_03

# head(df_order_items)
# head(df_orders)
# head(df_merged)
# 
# names(df_merged_orders_product)
## Odpowiedz przypisana do zmiennej
ANS_TASK_03


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 
# zamowienie = order/ nie order_items

# ???
# OK
ANS_TASK_04 <- df_orders |>
  mutate(year = format(as.Date(order_date), "%Y")) |>
  group_by(year, customer_id) |>
  summarise(orders_count = n()) |>
  group_by(year) |>
  slice_max(orders_count) |>
  group_by(year, orders_count) |>
  summarise(amount_of_customers = n())
  

# names(df_merged)
# head(df_merged)
## Odpowiedz przypisana do zmiennej
ANS_TASK_04
View(ANS_TASK_04)


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
#OK
ANS_TASK_05 <- df_orders |>
  inner_join(df_customers, by = "customer_id") |>
  mutate(mail_domain = sub(".*@", "", email),
         year = format(as.Date(order_date), "%Y")) |>
  group_by(year, mail_domain) |>
  summarise(total_orders = n()) |>
  group_by(year) |>
  slice_max(total_orders)

View(ANS_TASK_05)
## Odpowiedz przypisana do zmiennej
ANS_TASK_05


####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

# aktywny klient to taki/ ktory PROBOWAL zrobic zamowienie/ czyli 
# wszystki stany zamowienia sa okej

# df_result_6 <- df_orders |>
#   inner_join(df_customers, by = "customer_id") |>
#   filter(state %in% c("CA", "TX")) |>
#   group_by(state) |>
#   summarise(count = n())
#   
# head(df_result_6)  
# ?


active <- df_orders |>
  inner_join(df_customers, by = "customer_id") |>
  filter(state %in% c("CA", "TX")) |>
  group_by(customer_id) |>
  summarise(customer_id = first(customer_id), state = first(state)) |>
  group_by(state) |>
  summarise(active = n()) 
  # left_join(df_orders %>% filter(format(as.Date(order_date), "%Y") == "2018"), by = "customer_id") %>%
  # group_by(state) %>%
  # summarise(active_customers = sum(!is.na(order_id)),
  #           no_orders_2018 = sum(is.na(order_id)))

no_orders_2018 <- df_customers |>
  left_join(df_orders %>% filter(format(as.Date(order_date), "%Y") == "2018"), by = "customer_id") |>
  filter(state %in% c("CA", "TX")) |>
  group_by(state) |>
  summarise(no_orders_2018 = sum(is.na(order_id)))
View(no_orders_2018)

ANS_TASK_06 <- active |>
  inner_join(no_orders_2018, by = "state")

View(ANS_TASK_06)



View(ANS_TASK_06)

## Odpowiedz przypisana do zmiennej
ANS_TASK_06


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?

quantiles <- df_orders |>
  inner_join(df_order_items, by = "order_id") |>
  group_by(order_id) |>
  summarise(customer_id = first(customer_id),
            order_value = sum(quantity * list_price * (1 - discount)))

quantiles_values <- quantile(quantiles$order_value, probs = c(0.05, 0.95))
# quantiles_values
# ?
ANS_TASK_07 <- quantiles |>
  filter(order_value < quantiles_values[1] | order_value > quantiles_values[2]) |>
  select(customer_id) |>
  distinct(customer_id)

ANS_TASK_07
View(ANS_TASK_07)
# head(quantiles)
# names(quantiles)


## Odpowiedz przypisana do zmiennej
ANS_TASK_07


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.

# zakladam ze
# pasuje kazdy status zamowienia (1,2,3,4) bo kazde zamowienie musilo najpierw byc zlozone/ tylko potem albo odmowiono/ albo
# zrealizowane/ albo w trakcie i td

# ZROBIC LEFT JOIN ZEBY UWZGLEDNIC DNI W KTORYCH NIE BYLO
# ZAMOWIEN? czy nie
# zakladam ze  

library(dplyr)
library(lubridate)

# Załóżmy, że df_orders zawiera dane o zamówieniach z kolumną order_date

# liczymy wszystkie zamowienia kazdego dnia
totral_orders_by_days <- df_orders |>
  mutate(quarter_year = paste0(year(as.Date(order_date)), "Q", quarter(as.Date(order_date))),
         day = as.Date(order_date)) %>%
  group_by(quarter_year, day) %>%
  summarise(total_orders = n())
View(totral_orders_by_days)

# tworzymy pelny zakres dat od min do max w order_date
# bo sklep mogl miec dni/ kiedy zero zamowien
# z tabelki odczytuje
# 	
#2016-01-01 - 2018-12-28

all_dates <- tibble(
  day = seq.Date(from = as.Date("2016-01-01"), 
                  to = as.Date("2018-12-28"), 
                  by = "day")
) |>
  mutate(quarter_year = paste0(year(as.Date(day)), "Q", quarter(as.Date(day))))
View(all_dates)

# Krok 3: Left join pełnego zakresu dat z zamówieniami i wypełnij braki zerami
ANS_TASK_08 <- all_dates |>
  left_join(totral_orders_by_days, by = c("quarter_year", "day")) |>
  mutate(total_orders = ifelse(is.na(total_orders), 0, total_orders)) |>
  group_by(quarter_year) |>
  summarise(max_orders = max(total_orders),
            min_orders = min(total_orders),
            median_orders = median(total_orders))

View(ANS_TASK_08)

## Odpowiedz przypisana do zmiennej
ANS_TASK_08


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie

# dim(df_orders)
# joined <- df_orders |>
#   inner_join(df_customers, by = "customer_id")
# dim(joined)
# OK
task9 <- df_orders |>
  filter(!is.na(shipped_date)) |>
  mutate(year = format(as.Date(order_date), "%Y"),
         delivery_time = as.numeric(as.Date(shipped_date) - as.Date(order_date))) |>
  inner_join(df_customers, by = "customer_id") |>
  group_by(year, state) |>
  summarise(average_delivery_time = mean(delivery_time, na.rm = TRUE))

# Convert to wide format
ANS_TASK_09 <- task9 |>
  pivot_wider(
    names_from = state,        
    values_from = average_delivery_time
  )
View(ANS_TASK_09)
## Odpowiedz przypisana do zmiennej
ANS_TASK_09


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.

# zakladam, ze 
# baza klientow zostala calkowicie stworzona do 2016 (czyli nikt nie rejestrowal sie i robil zamownie pozniej)
# wtedy jest 3 lata - to jest co roku

ANS_TASK_10 <- df_orders %>%
  inner_join(df_customers, by = "customer_id") %>%
  group_by(customer_id) %>%
  summarise(last_name = first(last_name),
            distinct_years = n_distinct(format(as.Date(order_date), "%Y"))) |>
  filter(distinct_years == 3) |>
  mutate(first_letter = substr(last_name, 1, 1)) |>
  group_by(first_letter) |>
  summarise(letter_count = n())
  
View(ANS_TASK_10)


## Odpowiedz przypisana do zmiennej
ANS_TASK_10


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)

# df_merged <- df_orders |>
#   mutate(year = format(as.Date(order_date), "%Y")) |>
#   inner_join(df_order_items, by = "order_id") |>
#   inner_join(df_products, by = "product_id") |>
#   inner_join(df_customers, by = "customer_id")



# ANS_TASK_11 <- df_merged |>
#   inner_join(df_categories, by = "category_id")
#   filter(category_name == "Bikes") %>%
#   group_by(customer_id, category_name, product_id) %>%
#   summarise(total_bikes = sum(quantity),
#             latest_bike_purchases = sum(model_year == format(as.Date(order_date), "%Y"))) 
#   
  

# brand_new_count nie uwzgledniam quantity (po pytanie o razy/ nie o ilosc)

# OK
ANS_TASK_11 <- df_orders |>
  mutate(year = format(as.Date(order_date), "%Y")) |>
  inner_join(df_order_items, by = "order_id") |>
  inner_join(df_products, by = "product_id") |>
  inner_join(df_categories, by = "category_id") |>
  mutate(bike_in_this_year = ifelse(year == model_year, 1, 0)) |>
  group_by(customer_id, category_name) |>
  summarise(number_of_bikes = sum(quantity),
            brand_new_count = sum(bike_in_this_year)) |>
  pivot_wider(
    names_from = category_name,
    values_from = number_of_bikes,
    values_fill = 0
  ) |>
  group_by(customer_id) |>
  summarise(across(everything(), sum))

View(ANS_TASK_11)

# df_merged[df_merged$customer_id == 1, c("customer_id","product_id", "quantity", "model_year")] # 11*4 (alll 17)
# # sum(df_merged[df_merged$customer_id == 1, c("customer_id","product_id", "quantity")]$quantity)
# 
# orders_info[orders_info$customer_id == 1, ] # dim 11*4
# 
# 
# customers_merged <- df_customers |>
#   left_join(orders_info, by = "customer_id")
#   
# customers_merged
# 
# names(orders_info)
# names(df_customers)
View(ANS_TASK_11)
## Odpowiedz przypisana do zmiennej
ANS_TASK_11


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
# OK?
ANS_TASK_12 <- df_orders %>%
  inner_join(df_order_items, by = "order_id") |>
  mutate(week_day = weekdays(as.Date(order_date)),
     actual_revenue = quantity * (list_price * (1 - discount)),
     list_revenue = quantity * list_price,
     discount_percentage = 100 * (1 - (actual_revenue / list_revenue))) |>
  group_by(product_id, week_day) %>%
  summarise(average_discount = mean(discount_percentage, na.rm = TRUE)) |>
  pivot_wider(
    names_from = week_day,
    values_from = average_discount,
    values_fill = 0
  )

ANS_TASK_12
## Odpowiedz przypisana do zmiennej
View(ANS_TASK_12)



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")
solutions
### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "RazantsauMaksim.rds")

