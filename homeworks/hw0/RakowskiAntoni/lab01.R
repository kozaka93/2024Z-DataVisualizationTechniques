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
head(mtcars)

mtcars[1:3, 6]

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?


# Pierwszy wiersz, pierwsza kolumna?


# 10 pierszych wierszy, 2 i 3 kolumna?

# Jak wybieramy kolumny po nazwach? 

mtcars["gear"]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych



# 2. Jakie są typy zmiennych?


# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 

median(mtcars$drat)
mean(mtcars$drat)

# o wartości zmiennej "cyl" równej 4?


# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

table(mtcars$am)

# Prosty wykres



# Zależność "mpg" i "hp" - scatter plot

?plot
plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

?barplot
barplot(table(mtcars$cyl))

# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)
proton()
# W pliku zapisuj sposób rozwiązania gry.
employees[employees$surname == "Pietraszko",]
employees[employees$name == "John" & employees$surname == "Insecure",]
proton(action = "login", login = "johnins")
for (i in top1000passwords) {
  proton(action = "login", login = "johnins", password=i)
}
sort(table(logs[logs$login == "slap", "host"]), decreasing = TRUE)
proton(action="server", host="194.29.178.16")
for (i in bash_history) {
  proton(action = "login", login = "slap", password=i)
}
# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------

