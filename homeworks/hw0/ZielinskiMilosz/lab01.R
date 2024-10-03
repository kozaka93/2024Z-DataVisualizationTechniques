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
mtcars[1,]
mtcars[,3]
# Pierwszy wiersz, pierwsza kolumna?


# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,2:3]
mtcars[1:10,c(2,5)]
# Jak wybieramy kolumny po nazwach? 
mtcars[,'wt']
mtcars[,c('wt', 'am')]
mtcars$hp

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c('am','wt','mpg')]

# Jak wybierać jedną kolumnę?


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

mtcars$cyl==4
mtcars$drat[mtcars$cyl==4]
mean(mtcars$drat[mtcars$cyl==4])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
table(mtcars$am)


# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg,mtcars$hp)

# Zmienna "cyl" - barplot
barplot(table(mtcars$cyl))

# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.
table(employees$name=='John')

employees$login[employees$surname=='Pietraszko']
proton(action = "login", login="johnins")
top1000passwords
for(i in top1000passwords){
  proton(action = "login", login="slap", password=i)
}
proton(action = "login", login="johnins", password="ABC")

unique(logs$host[logs$login=='slap'])
table(logs$host[logs$login=='slap'])

which.max(table(logs$host[logs$login=='slap']))

length(unique(logs$host[logs$login=='johnins']))


proton(action = "server", host="194.29.178.16 ")
# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------
Well done! This is the right password!
  Bit used John Insecure's account in order to log into the Proton server.
It turns out that John has access to server logs.
Now, Bit wants to check from which workstation Pietraszko is frequently logging into the Proton server. Bit hopes that there will be some useful data.  

Logs are in the `logs` dataset. 
Consecutive columns contain information such as: who, when and from which computer logged into Proton.

Problem 3: Check from which server Pietraszko logs into the Proton server most often.

Use `proton(action = "server", host="XYZ")` command in order to learn more  about what can be found on the XYZ server.
The biggest chance to find something interesting is to find a server from which Pietraszko logs in the most often.
