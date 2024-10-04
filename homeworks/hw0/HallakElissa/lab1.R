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

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[7, ] #zwraca ramke danych
mtcars[, 7] #zwraca wektor

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1, ]
mtcars[, 1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[seq(1,10), c(2,3)]
mtcars[1:10, 2:3]

# Jak wybieramy kolumny po nazwach? 
mtcars[, 'cyl']
mtcars$cyl

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[ , c('am', 'wt', 'mpg')]

# Jak wybierać jedną kolumnę?
mtcars[ , 1]

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
length(unique(mtcars$cyl))
table(mtcars$cyl)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 
# o wartości zmiennej "cyl" równej 4?
mean(mtcars$drat[mtcars$cyl == 4])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
table(mtcars$am)


# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)


# Zmienna "cyl" - barplot
barplot(table(mtcars$cyl))

# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.

# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------
employees
str(employees)
employees[employees$name == 'John',]
proton(action = "login", login="johnins")

top1000passwords
proton(action = "login", login="XYZ", password="slayer")

str(employees) <= str(top1000passwords)
top1000passwords['johnins']

sapply(top1000passwords









