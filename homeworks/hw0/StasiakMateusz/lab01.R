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

head(mtcars,10)
tail(mtcars,12)

mtcars[3:5,4:6]

mtcars[c("mpg","hp")]

mtcars$mpg
mtcars["mpg"]

x<-"mpg"
mtcars$x
mtcars[,x]

str(mtcars)

length(unique(mtcars$cyl))

mask<-mtcars$cyl==4

mtcars[mask,]

mean(mtcars[mask,]$drat)

table(mtcars$am)

plot(mtcars$mpg,mtcars$hp)

barplot(table(mtcars$cyl))

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?


# Pierwszy wiersz, pierwsza kolumna?


# 10 pierszych wierszy, 2 i 3 kolumna?


# Jak wybieramy kolumny po nazwach? 


# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?


# Jak wybierać jedną kolumnę?

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych


# 2. Jakie są typy zmiennych?


# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?


# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 
# o wartości zmiennej "cyl" równej 4?


# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 



# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot


# Zmienna "cyl" - barplot


# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

proton(hint=TRUE)

str(employees)

XYZ<-employees[(employees$name=="John")&(employees$surname=="Insecure"),"login"]

proton(action="login",login=XYZ)
proton(hint=TRUE)

for (pass in top1000passwords){
  response<-proton(action = "login", login=XYZ, password=pass)
  if(response=="Success! User is logged in!"){
    cat(pass)
    break
  }
}

tail(top1000passwords)

x<-employees[(employees$surname=="Pietraszko"),"login"]

cat(x)

pom<-logs[logs$login==x,]
n<-table(pom$host)
print(which.max(n))

print(proton(action="server",host="194.29.178.16"))
length(bash_history)
bash_history<-unlist(strsplit(bash_history, split=' ', fixed=TRUE))
head(bash_history)



# W pliku zapisuj sposób rozwiązania gry.



# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------

