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


# Pierwszy wiersz, pierwsza kolumna?


# 10 pierszych wierszy, 2 i 3 kolumna?


# Jak wybieramy kolumny po nazwach? 
mtcars[,c("wt","am")]
mtcars$wt

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[ , c("am","wt","mpg")]

# Jak wybierać jedną kolumnę?

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych


# 2. Jakie są typy zmiennych?


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
plot(x = mtcars$mpg, y = mtcars$hp)

# Zależność "mpg" i "hp" - scatter plot
plot(x = mtcars$mpg, y = mtcars$hp)

# Zmienna "cyl" - barplot
barplot(table(mtcars$cyl))


# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.



# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------

employees

which(employees$name == "John")
employees[c(217,432),]

johnins

proton(action = "login", login="johnins")
top1000passwords

for (passwd in top1000passwords) {
  result <- proton(action = "login", login="johnins", password=passwd)
  
  if (result == TRUE) {  
    cat("Znaleziono hasło", passwd)
    break 
  }
}
freepass

logs
which(employees$surname == "Pietraszko")
employees[477,]

logs$login == "slap"

slap_servers <- logs[logs$login == "slap", "host"]
max(table(slap_servers))

194.29.178.160

proton(action = "server", host="194.29.178.16")
bash_history

filtered <- filtered[!grepl("^service httpd start", filtered)]
DHbb7QXppuHnaXGN

proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")
