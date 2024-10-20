library(dplyr)
library(tidyr)

df_brands <- read.csv('homeworks/hw1/dane/brands.csv')
df_categories <-  read.csv('homeworks/hw1/dane/categories.csv')
df_customers <- read.csv('homeworks/hw1/dane/customers.csv')
df_order_items <- read.csv('homeworks/hw1/dane/order_items.csv')
df_orders <- read.csv('homeworks/hw1/dane/orders.csv')
df_products <- read.csv('homeworks/hw1/dane/products.csv')
df_staffs <- read.csv('homeworks/hw1/dane/products.csv')
df_stocks.csv <- read.csv('homeworks/hw1/dane/stocks.csv')

####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.
with_orders <- inner_join(df_order_items, df_orders, by='order_id')
with_products <- inner_join(with_orders, df_products, by= 'product_id')
with_customers <- inner_join(with_products, df_customers, by='customer_id')
data <- with_customers %>%  filter(order_status == 4) %>% select(order_date, state, quantity, product_id, product_name)
assign_quarter <- function(date) {
  paste(format(as.Date(date), format="%Y"), (as.integer(format(as.Date(date), format="%m"))-1)%/%3 + 1)
}
data_with_quarters <- data %>% mutate(order_date = assign_quarter(order_date)) %>% rename(quarter=order_date)

result <- data_with_quarters %>% group_by(quarter, product_id, state) %>% summarise(quantity = sum(quantity))

best_selling_products_for_each_quarter_n_state <- result %>% group_by(quarter, state) %>% summarise(quantity = max(quantity)) %>% inner_join(result)

product_data <- df_products %>% select(product_id, product_name, model_year)

best_selling_products_for_each_quarter_n_state_with_product_data <- best_selling_products_for_each_quarter_n_state %>% left_join(product_data)

ANS_TASK_01 <- best_selling_products_for_each_quarter_n_state_with_product_data %>% select(quarter, state, product_name, model_year)


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu?
assign_month <- function(date) {
  format(as.Date(date), format="%Y-%m")
}
filtered_orders <- df_orders %>% select(order_status, order_date) %>% mutate(order_date = assign_month(order_date))
all_orders <- filtered_orders %>% count( order_date) %>% rename(order_count=n)
failed_orders <-filtered_orders %>% filter(order_status != 4)%>% count( order_date) %>% rename(fails=n)
result <- inner_join(all_orders, failed_orders) %>% transmute(order_date, failure_percentage=fails/order_count)
ANS_TASK_02 <- result

####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
assign_year <- function(date) {
  format(as.Date(date), format="%Y")
}
relevant_orders <- df_orders %>% filter(order_status==4)%>% select(order_id, order_date)
order_incomes <- df_order_items %>% transmute(order_id, product_id, income=list_price * quantity * (1-discount)) %>% group_by(order_id, product_id) %>% summarise(total_income = sum(income))
incomes_for_each_product_by_year <- relevant_orders %>% left_join(order_incomes) %>% transmute(order_year = assign_year(order_date), product_id, total_income)  %>% group_by(order_year, product_id) %>% summarise(new_total_income = sum(total_income))
best_sellers <- incomes_for_each_product_by_year %>% group_by(order_year) %>% summarise(new_total_income = max(new_total_income))

## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- best_sellers %>% left_join(incomes_for_each_product_by_year) %>% left_join(df_products) %>% select(order_year, product_name)

####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień?
assign_year <- function(date) {
  format(as.Date(date), format="%Y")
}

#> Czy w zadaniu 3 i 4 powinniśmy brać pod uwagę tylko zamówienia,
#>  które mają status 'completed', czy nie musimy tego rozważać?
# Wcześniej użyłem filter(order_status==4), ale wtedy każdy klient ma maksymalnie jedno zamówienie
# więc zgodnie z komentarzem na github pozwoliłem sobie nie wziąść statusu pod uwagę
# (co moim zdaniem jest słabe, bo Ci klienci wcale nie zrobili zakupów jeżeli 
# zamówienie nie zostało ukończone)
  
orders_with_year <- df_orders %>% transmute(order_id, year=assign_year(order_date), customer_id)
amount_of_orders_per_client<-orders_with_year %>% group_by(year, customer_id) %>% summarise(count = n())
max_orders<- amount_of_orders_per_client %>% group_by(year) %>% summarise(count = max(count))
result<- max_orders %>% left_join(amount_of_orders_per_client) %>% group_by(year, count) %>% summarise(client_count = n())
result<-result %>% transmute(year, order_count=count, customers_count = client_count)
## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <- result


####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?
orders <- df_orders %>% transmute(order_id, year=assign_year(order_date), customer_id) %>% left_join(df_customers) %>% select(order_id, year, email)
assign_domain <- function(mail) {
  strsplit(mail, "@")[[1]][2]
}
domain_orders<- orders %>% rowwise() %>%transmute(order_id, year, domain=assign_domain(email))

order_count <- domain_orders  %>% group_by(year, domain) %>% summarise(count = n())

order_count_max <- order_count %>% group_by(year) %>% summarise(count = max(count))

result <- order_count_max %>% left_join(order_count) %>% transmute(year, domain, order_count=count)

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- result

####### Zadanie 6
# Zobacz ile aktywnych klientow miala firma w stanie Kalifornia i Teksasie? 
# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?

## Odpowiedz przypisana do zmiennej
"github: Za aktywnych uznajemy takich klientów, którzy złożyli zamówienie w omawianej bazie zamówień."
active_customers <- df_orders %>% select(customer_id) %>% unique()
customers <- df_customers %>% select(customer_id) %>% unique()
total_active_customers <- active_customers %>% inner_join(customers)
# jak widzimy wg. tej definicji wystarczyło tak naprawdę zebrać użytkowników z tabeli df_orders, 
# zakładając, że wszyscy z tamtąd istnieją w df_users

active_customers_by_state <- total_active_customers %>% inner_join(df_customers) %>% filter(state=="CA" | state=="TX")%>% select(customer_id, state)
active_customers_by_state_count <- active_customers_by_state  %>% group_by(state) %>% summarise(count = n()) 

# Czy w bazie klientów z tych stanów są tacy, którzy nie zrobili żadnego zamówienia w 2018?
assign_year <- function(date) {
  format(as.Date(date), format="%Y")
}
customers_2018 <- df_orders %>% select(customer_id, order_date) %>% transmute(customer_id, year = assign_year(order_date)) %>% filter(year == "2018")
customers <- df_customers %>% select(customer_id) %>% left_join(customers_2018)
are_there_clients_without_order_in_2018 <- count(customers %>% filter(is.na(year))) > 0
ANS_TASK_06 <- list(active_customers_by_state_count, are_there_clients_without_order_in_2018) 


####### Zadanie 7
## Którzy klienci byli ekstremalnymi klientami tzn. wydali w jednym zamówieniu poniżej 5 kwantyla lub więcej niż 95 kwantyl wartosci zamówienia?
relevant_orders <- df_orders %>% filter(order_status==4)%>% select(order_id, order_date)
order_incomes <- df_order_items %>% transmute(order_id, product_id, income=list_price * quantity * (1-discount)) %>% group_by(order_id) %>% summarise(total_income = sum(income))
quantiles <- quantile(order_incomes[["total_income"]], c(0.05, 0.95))
quantile1 <- quantiles[[1]]
quantile2 <- quantiles[[2]]
extreme_orders <- order_incomes %>% filter(total_income < quantile1 |  quantile2 < total_income)
extreme_customers <- extreme_orders %>% left_join(df_orders) %>% select(customer_id) %>% unique()
ANS_TASK_07 <- extreme_customers


####### Zadanie 8
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień
# złożonych każdego dnia w poszczególnych kwartalach.

orders_by_day <- df_orders %>% select(order_date) %>% group_by(order_date) %>% summarise(count = n())
assign_quarter <- function(date) {
  paste(format(as.Date(date), format="%Y"), (as.integer(format(as.Date(date), format="%m"))-1)%/%3 + 1)
}
quarterly <- orders_by_day %>% transmute(quarter = assign_quarter(order_date), count) %>% group_by(quarter) %>% summarise(median_value = median(count), max_value = max(count), min_value = min(count))
 
ANS_TASK_08 <- quarterly


####### Zadanie 9 
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
delivery_time <- function(order_date, shipped_date) {
  as.numeric(difftime(as.Date(shipped_date), as.Date(order_date), units="days"))
}

assign_year <- function(date) {
  format(as.Date(date), format="%Y")
}

delivered_orders <- df_orders %>% filter(shipped_date != "NULL") %>% transmute(order_id, customer_id, order_year = assign_year(order_date),delivery_days=delivery_time(order_date, shipped_date))
delivered_orders_with_state <- delivered_orders %>% left_join(df_customers) %>% select(delivery_days, order_year, state) %>% group_by(order_year, state)  %>% summarise(avg_delivery_time = mean(delivery_days))
ca <- delivered_orders_with_state %>% filter(state=="CA") %>% select(order_year, CA=avg_delivery_time)
ny <- delivered_orders_with_state %>% filter(state=="NY") %>% select(order_year, NY=avg_delivery_time)
tx <- delivered_orders_with_state %>% filter(state=="TX") %>% select(order_year, TX=avg_delivery_time)
ANS_TASK_09 <- ca %>% left_join(ny) %>% left_join(tx)


####### Zadanie 10
# Od jakich liter zaczynają się nazwiska klientów, którzy robili zamówienia co roku. 
# Przeanalizuj jak często występuje każda litera wśród tych nazwisk.
assign_year <- function(date) {
  format(as.Date(date), format="%Y")
}
customers_with_year_count <- df_orders %>% transmute(customer_id, year = assign_year(order_date)) %>% unique() %>% group_by(customer_id) %>% summarise(count = n())
max_val <- max(customers_with_year_count[["count"]])
customers_with_all_years <- customers_with_year_count %>% filter(count == max_val) %>% select(customer_id)
ANS_TASK_10 <- customers_with_all_years %>% left_join(df_customers) %>% 
  select(last_name) %>% transmute(first_letter = substring(last_name, 0, 1)) %>% 
  group_by(first_letter) %>% summarise(count = n())


####### Zadanie 11
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)
assign_year <- function(date) {
  format(as.Date(date), format="%Y")
}
order_items <- df_order_items %>% select(order_id, product_id, quantity) %>% 
  left_join(df_products %>% select(product_id, category_id, model_year))
orders <- df_orders %>% transmute(order_id, customer_id, order_year = assign_year(order_date))

orders_with_year <- orders %>% left_join(order_items) %>% select(customer_id, category_id, order_year, model_year, quantity)
sums_by_customer_and_category <- orders_with_year %>% group_by(customer_id, category_id) %>% summarise(quantity = sum(quantity))
categories <- 1:7
result <- sums_by_customer_and_category %>% select(customer_id) %>% unique()

for (category in categories) {
  temp_table <- sums_by_customer_and_category %>% filter(category==category_id) %>% transmute(customer_id, quantity)
  colnames(temp_table) <- c("customer_id",df_categories[[2]][[category]] )
  result <- result %>% left_join(temp_table)
}

orders_with_the_same_year <- orders_with_year %>% filter(order_year==model_year)
sums_with_the_same_year_by_customer_and_category <- orders_with_the_same_year %>% group_by(customer_id) %>% summarise(quantity = sum(quantity))
colnames(sums_with_the_same_year_by_customer_and_category) <- c("customer_id", "newest_ones")
result <- result %>% left_join(sums_with_the_same_year_by_customer_and_category)
ANS_TASK_11 <- result %>% replace(is.na(.), 0)


### Zadanie 12
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
assign_weekday <- function(date) {
  format(as.Date(date), format="%A")
}
orders_with_discounts <- df_orders %>% select(order_id, order_date) %>% left_join(df_order_items %>% select(order_id, product_id, discount)) %>% transmute(product_id, weekday = assign_weekday(order_date), discount)
ANS_TASK_12 <- orders_with_discounts %>% group_by(product_id, weekday) %>% summarise(avg_discount = mean(discount))


### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "RyskiAdam.rds")
