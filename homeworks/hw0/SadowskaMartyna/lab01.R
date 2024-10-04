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
head(mtcars, 10)
tail(mtcars, 3)
dim(mtcars)
str(mtcars)


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[1:5, 3:4]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1, 1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]

# Jak wybieramy kolumny po nazwach? 
mtcars$cyl
mtcars[, c("cyl", "hp")]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$cyl
mtcars["cyl"]

var <- "cyl"
mtcars$var #null
mtcars[var]

# Uwaga na przecinek i wybór kolumn poprzez indeksy

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
mean(mtcars[mask,]$drat)

mean(mtcars[(mtcars$cyl==4), "drat"])


# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
unique(mtcars$am)
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

#1
proton()
head(employees)
XYZ <- employees[((employees$name=="John") & (employees$surname=="Insecure")), "login"]
proton(action="login", login=XYZ)

#2
head(top1000passwords)
for(i in 1:1000)
{
    ABC <- top1000passwords[i]
    proton(action = "login", login=XYZ, password=ABC)
}

#3
head(logs)
johnlogs <- logs[logs$login==XYZ, ]
johnlogs
johnslogs <- aggregate(johnlogs$host, list(johnlogs$host), FUN=length)
host <- johnslogs[johnslogs$x==max(johnslogs$x), "Group.1"]
host
proton(action = "server", host=host)


# W pliku zapisuj sposób rozwiązania gry.



# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------

