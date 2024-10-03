# kontakt do szefa z labow: hruczyski21@interia.pl
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
tail(mtcars)

dim(mtcars)
str(mtcars)
summary(mtcars)

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[1:3, ]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]
mtcars[1:10, c(2,3)]
# Jak wybieramy kolumny po nazwach? 

mtcars[, c("mpg", "cyl")]
# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 
# o wartości zmiennej "cyl" równej 4?
mean(mtcars$drat[mtcars$cyl==4])
mean(mtcars[mtcars$cyl==4, c("drat")])


# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
table(mtcars$am)


# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$hp, mtcars$mpg)

# Zmienna "cyl" - barplot
barplot(mtcars$cyl)

# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.



# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------
proton()
head(employees)
employees$login[employees$name=="John" & employees$surname=="Insecure"]
proton(action = "login", login="johnins")
for (password in top1000passwords) {
  proton(action = "login", login="johnins", password=password)
}
sort(table(logs[logs$login=="slap", c("host")]), ascending=TRUE)
proton(action="server", host="194.29.178.81")
head(logs)
proton(action = "server", host="194.29.178.16")

head(bash_history)
length((bash_history))
x <- strsplit(bash_history, split = " ")
for (password in strsplit(bash_history, split = " ")){
  proton(action = "login", login="slav", password=password)
}
