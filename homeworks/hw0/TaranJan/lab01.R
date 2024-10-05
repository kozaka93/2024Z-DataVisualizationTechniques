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
mtcars["mpg",]
mtcars[1,1]
mtcars[mtcars$qsec,c('hp', 'disp')]
# Pierwszy wiersz, pierwsza kolumna?
dim(mtcars)


# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]

# Jak wybieramy kolumny po nazwach? 


# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
str(mtcars)

# Jak wybierać jedną kolumnę?
mtcars$mpg
# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych


# 2. Jakie są typy zmiennych?
typeof(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?


# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 
# o wartości zmiennej "cyl" równej 4?
length(unique(mtcars$cyl))
table(mtcars$cyl)

mean(mtcars[mtcars$drat==4])
# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
table(mtcars$am)


# Prosty wykres
plot(x=mtcars$mpg, y=mtcars$hp)
barplot(table(mtcars$cyl))

# Zależność "mpg" i "hp" - scatter plot


# Zmienna "cyl" - barplot


# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.
proton()

data("employees")
employees[employees$name=="John",]
proton(action = "login", login="johnins")
data("top1000passwords")
for (PASS in top1000passwords) {
  proton(action = "login", login="johnins", password=PASS)
}

data("logs")

employees[employees$surname=="Pietraszko",]
sort(employees$name)
str(logs)
table(logs$host)

unique(logs$login)

which.max(table(logs[logs$login=="slap","host"]))
#194.29.178.16

proton(action = "server", host="194.29.178.16")

data("bash_history")
View(sort(bash_history))
substr(bash_history)
helper <- bash_history[grep("vim", bash_history, invert = TRUE)]
helper <- helper[! helper %in% grep("vi", bash_history)]
length(helper)
helper <- helper[!grep("cat", bash_history, invert = TRUE)]
helper <- helper[!grep("ls", bash_history, invert = TRUE)]
helper <- helper[!grep("rm", bash_history, invert = TRUE)]
helper <- helper[!grep("cp", bash_history, invert = TRUE)]
helper

View(helper)
?grep

# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------

