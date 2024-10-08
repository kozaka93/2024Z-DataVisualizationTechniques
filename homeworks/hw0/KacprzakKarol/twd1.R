head(mtcars)
data(mtcars)
head(mtcars,10)
tail(mtcars)
dim(mtcars)
str(mtcars)
summary(mtcars)
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
mtcars[1:3,1:2]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,2:3]

# Jak wybieramy kolumny po nazwach? 
mtcars[,c("cyl","mpg")]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$mpg

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
length(unique(mtcars$cyl))
# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 
# o wartości zmiennej "cyl" równej 4?
mean(mtcars$drat[mtcars$cyl==4])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
table(mtcars$am)


# Prosty wykres
plot(mtcars$mpg,mtcars$hp)

# Zależność "mpg" i "hp" - scatter plot


# Zmienna "cyl" - barplot
barplot(mtcars$cyl)
barplot(table(mtcars$cyl))

View(mtcars)
# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.

proton()
employees[employees$name=="John",]
proton(action="login",login="johnins")
top1000passwords
for(pass in top1000passwords){
  response<-proton(action = "login", login="johnins", password=pass)
  if(response==TRUE){
    cat(password)
  }
}
employees[employees$surname=="Pietraszko",]
data.frame(table(logs[logs$login=="slap",c("host")]))

proton(action="server",host="194.29.178.16")
bash_history
comands<-c()
split_bash_history<-strsplit(bash_history," ")
for(x in split_bash_history){
  
  
}
# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------

