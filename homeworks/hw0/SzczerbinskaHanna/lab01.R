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
mtcars[1,] # pierwszy wiersz, wszytskie kolumny
mtcars[,3] # cala trzecia kolumna; zwraca jako wektor

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]

# 10 pierwszych wierszy, 2 i 3 kolumna?
mtcars[1:10,2:3]
mtcars[1:10,c(2,5)] # 10 pierwszych wierszy, 2 i 5 kolumna

# Jak wybieramy kolumny po nazwach? 
mtcars[,'wt'] # wszystkie wiersze, kolumna wt
mtcars[, c('wt','am')] # 2 kolumny
mtcars$wt # dziala tylko jak chcemy wszystkie wiersze i 1 kolumna

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c('am','wt','mpg')]

# Jak wybierać jedną kolumnę?
# jw - po nazwie / po indeksie

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych
dim(mtcars) # najpierw podaje liczbe wierszy, potem kolumn

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
length(unique(mtcars$cyl))
table(mtcars$cyl) # powie ile razy jest ktora z unikalnych wartosci z danej kolumny

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 
# o wartości zmiennej "cyl" równej 4?
mean(mtcars$drat[mtcars$cyl == 4])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
table(mtcars$am)


# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot
plot(x = mtcars$mpg, y = mtcars$hp)

# Zmienna "cyl" - barplot
barplot(table(mtcars$cyl)) # barplot jest raczej dla danych dyskretnych, histogram raczej dla ciaglych

# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.
head(employees)
employees$login[employees$name == "John" & employees$surname == "Insecure"]
proton(action = "login", login="johnins")
for(i in top1000passwords) {
  proton(action = "login", login="johnins", password=i)
}
head(logs)
employees$login[employees$name == "Slawomir" & employees$surname == "Pietraszko"]
which.max(table(logs$host[logs$login == "slap"]))
proton(action = "server", host="194.29.178.16")


# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------

