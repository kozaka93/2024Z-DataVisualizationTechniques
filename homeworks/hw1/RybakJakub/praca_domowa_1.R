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

ANS_TASK_01 <- df_order_items %>% 
  inner_join(df_orders, by = "order_id") %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  mutate(kwartal = quarters(as.Date(order_date))) %>% 
  group_by(product_id,kwartal, state) %>% 
  summarise(czestosc_kupowania = n()) %>% 
  arrange(kwartal, state, desc(czestosc_kupowania)) %>% 
  group_by(kwartal, state) %>% 
  slice(1) %>% 
  ungroup() %>% 
  inner_join(df_products, by = "product_id") %>%
  select(product_name, model_year, kwartal,state)

pom2 <- df_orders %>% 
  mutate(miesiac = format(as.Date(order_date),"%m"))  %>%
  mutate(nie_zrealizowano = case_when(
    order_status == 3  ~ 1,
    T ~ 0)) 
pom22 <- pom2 %>% 
  group_by(miesiac) %>%
  summarise(razem = n())
ANS_TASK_02 <- pom2 %>% 
  group_by(miesiac) %>% 
  summarise(niezrealizowanych = sum(nie_zrealizowano)) %>% 
  inner_join(pom22, by = "miesiac") %>% 
  mutate(procent_niezrealizowanych = niezrealizowanych * 100/razem) %>% 
  select(miesiac, procent_niezrealizowanych)  

ANS_TASK_03 <- df_order_items %>% 
  inner_join(df_orders) %>%
  mutate(kasa = (list_price - discount) * quantity, rok = format(as.Date(order_date),"%Y")) %>% 
  group_by(rok, product_id) %>%
  summarise(przychod = sum(kasa)) %>% 
  arrange(rok, desc(przychod)) %>%
  group_by(rok) %>% 
  slice(1) %>% 
  ungroup() %>% 
  inner_join(df_products, by = "product_id") %>% 
  select(rok,product_name,przychod)

pom4 <- df_order_items %>% 
  inner_join(df_orders) %>% 
  mutate(rok = format(as.Date(order_date),"%Y")) %>% 
  group_by(rok,customer_id) %>% 
  summarise(razem = n())
ANS_TASK_04 <- pom4 %>% 
  group_by(rok) %>% 
  summarise(najwiecej = max(razem)) %>% 
  inner_join(pom4,by = "rok") %>% 
  mutate(czy_maks = case_when(
    razem == najwiecej ~ 1,
    T ~ 0
  )) %>% 
  group_by(rok,najwieksza_liczba_zamowien = najwiecej) %>% 
  summarise(ile_zrobilo_najwiecej = sum(czy_maks))

ANS_TASK_05  <- df_customers %>% 
  mutate(domena = sub(".*@","",email)) %>%
  select(customer_id, domena) %>% 
  inner_join(df_orders, by="customer_id") %>%
  mutate(rok = format(as.Date(order_date),"%Y")) %>%
  group_by(rok,domena) %>% 
  summarise(ile = n()) %>% 
  arrange(rok, desc(ile)) %>% 
  group_by(rok) %>% 
  slice(1) %>% 
  ungroup()

pom63 <- df_orders %>% 
  select(customer_id) %>% 
  distinct() %>% 
  inner_join(df_customers, by="customer_id") %>% 
  filter((state =="CA")|(state == "TX")) %>% 
  group_by(state) %>% 
  summarise("liczba klientow" = n())

pom6 <- df_orders %>% 
  mutate(rok = format(as.Date(order_date),"%Y")) %>% 
  filter(rok == 2018) %>% 
  select(customer_id) %>% 
  distinct()
pom62 <- df_orders %>% 
  select(customer_id) %>% 
  distinct()

ANS_TASK_06 <- pom63 %>% 
  mutate(drugie_pytanie = "tak, są klienci ktorzy nie zrobili zadnego zamowienia w 2018")
pom7 <- df_order_items %>% 
  mutate(kasa = (list_price-discount)*quantity)
kwant_5 <- quantile(pom7$kasa,probs = 0.05)
kwant_95 <- quantile(pom7$kasa,probs = 0.95)

ANS_TASK_07 <- pom7 %>% 
  filter((kasa<kwant_5)|(kasa>kwant_95)) %>% 
  inner_join(df_orders) %>% 
  select(customer_id) %>% 
  distinct()
ANS_TASK_08  <- df_orders %>% 
  group_by(order_date) %>% 
  summarise(liczba_jendego_dnia = n()) %>% 
  mutate(kwartal = quarters(as.Date(order_date))) %>% 
  group_by(kwartal) %>% 
  summarise(minimalna = min(liczba_jendego_dnia), maksymalna = max(liczba_jendego_dnia),mediana = median(liczba_jendego_dnia))

ANS_TASK_09  <- df_orders %>% 
  inner_join(df_customers) %>% 
  mutate(rok = format(as.Date(order_date),"%Y")) %>% 
  mutate(czas_dostawy = as.numeric(as.Date(required_date) - as.Date(order_date))) %>% 
  group_by(rok,state) %>% 
  summarise(sredni_czas_dostawy = mean(czas_dostawy)) %>% 
  pivot_wider(names_from = state, values_from = sredni_czas_dostawy)

pom10 <- df_orders %>% 
  mutate(rok = format(as.Date(order_date), "%Y")) %>% 
  select(rok, customer_id) %>% 
  distinct() %>% 
  group_by(customer_id) %>% 
  summarise(liczba_zamowien_w_roznych_latach = n()) %>% 
  filter(liczba_zamowien_w_roznych_latach == 3) %>% 
  inner_join(df_customers, by = "customer_id") %>% 
  mutate(pierwsza_litera=substr(last_name,1,1)) 
pom102 <- pom10 %>% 
  select(customer_id, pierwsza_litera) %>% 
  group_by(pierwsza_litera) %>% 
  summarise(liczba_wystapien = n())
pom103 <- pom10 %>% 
  select(customer_id, last_name)
nazwiska <- pom103$last_name
wszystkie_nazwiska <- paste(nazwiska, collapse = "")
litery <- unlist(strsplit(tolower(wszystkie_nazwiska), ""))
czestotliwosc_liter <- as.data.frame(table(litery))
ANS_TASK_10 <-list(pom102,czestotliwosc_liter)

pom11 <- df_order_items %>% 
  inner_join(df_orders,by = "order_id") %>% 
  right_join(df_products,by="product_id") %>% 
  inner_join(df_categories,by="category_id") %>% 
  select(order_id,customer_id,category_id) %>% 
  group_by(customer_id,category_id) %>% 
  summarise(liczba_zakupow = n())
wszystkie_kombinacje <- expand.grid(customer_id = unique(pom11$customer_id),category_id=1:7)
pom11 <- pom11 %>% 
  right_join(wszystkie_kombinacje,by=c("customer_id","category_id")) %>% 
  mutate(liczba_zakupow = replace_na(liczba_zakupow,0)) %>% 
  arrange(customer_id, category_id) %>% 
  pivot_wider(names_from = customer_id, values_from = liczba_zakupow)
pom112 <- df_order_items %>% 
  inner_join(df_orders,by = "order_id") %>% 
  right_join(df_products,by="product_id") %>% 
  mutate(rok_zamowienia = format(as.Date(order_date),"%Y")) %>% 
  select(customer_id, order_id, rok_zamowienia, model_year) %>% 
  mutate(czy_nowy = case_when(rok_zamowienia == model_year ~ 1, T ~ 0)) %>% 
  group_by(customer_id) %>% 
  summarise(ile_razy_nowy = sum(czy_nowy))
ANS_TASK_11 <- list(pom11,pom112)

ANS_TASK_12 <-df_order_items %>% 
  mutate(rabat = discount/list_price*100) %>% 
  select(product_id, order_id, rabat) %>% 
  inner_join(df_orders) %>% 
  mutate(dzien_tygodnia = format(as.Date(order_date),"%A")) %>% 
  group_by(product_id,dzien_tygodnia) %>% 
  summarise(sredni_rabat = mean(rabat))

solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "RybakJakub.rds")