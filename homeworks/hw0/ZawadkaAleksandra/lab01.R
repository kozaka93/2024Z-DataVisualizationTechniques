###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################


# 0. Prowadzący  -------------------------------------------------------------

# Anna Kozak/Maciej Chrabąszcz/Hubert Ruczyński/Katarzyna Woźnica
# Kontakt: MS Teams lub mail


# 1. Sposób pracy na zajęciach laboratoryjnych -------------------------------

# a) pracujemy w R (większość semestru) i Python
# b) pracujemy na przygotowanych plikach, które będą na repozytorium przedmiotu
# c) podczas zajęć prowadzący będzie wprowadzał zagdanienia, a następnie będzie rozwiązywanie zadań w celu utrwalenia wiadomości
# d) kolejna porcja materiału będzie omawiana jeżeli większość grupy wykona zadane zadanie 
# e) wszelkie pytania czy to związane z kodem, pracą domową czy kwestie teoretyczne proszę śmiało zgłaszać prowadzącemu 


# 2. Materiały ------------------------------------------------------------

# Repozytorium na GitHub
# https://github.com/kozaka93/2024Z-DataVisualizationTechniques


# 3. Jak działa GitHub? ---------------------------------------------------

# Jak zgłosić pracę domową/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/


# 4. Podstawy R - rozgrzewka ----------------------------------------------

data(mtcars)
head(mtcars,10)
tail(mtcars,3)

?mtcars

dim(mtcars)

str(mtcars)

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[1:5,3:4]
mtcars[1,1]

# Pierwszy wiersz, pierwsza kolumna?


# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,2:3]

# Jak wybieramy kolumny po nazwach? 

mtcars[,c("mpg","hp")]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c("am","wt","mpg")]


# Jak wybierać jedną kolumnę?
mtcars$hp
mtcars["mpg"]


# Uwaga na przecinek i wybór kolumn poprzez indeksy
mtcars[c(1,2)]
mtacras[1,2]

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 
# o wartości zmiennej "cyl" równej 4?
mask <- mtcars$cyl==4
mtcars[mask,]
mean(mtcars[mask,]$drat)
# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
unique(mtcars$am)
table(mtcars$am)

# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg,mtcars$hp)


# Zmienna "cyl" - barplot
barplot(mtcars$cyl)
barplot(table(mtcars$cyl))


# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)
proton()
# W pliku zapisuj sposób rozwiązania gry.
head(employees)
log <- employees[(employees$name == "John" & employees$surname == "Insecure"),"login"]
log
proton(action = "login", login = log)

head(top1000passwords)
for (i in 1:length(top1000passwords)) proton(action = "login", login=log, password=top1000passwords[i])

proton(action = "server", host="XYZ")
head(logs)
pietraszko <- logs[logs$login==log,]
d <- aggregate(pietraszko$host,pietraszko["host"],length)
proton(action = "server", host=d[d$x==max(d$x),"host"])
proton(action = "server", host="194.29.178.13")
# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------

