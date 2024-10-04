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
library(dplyr)
data(mtcars)
head(mtcars)

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

mtcars[1,1]

mtcars %>% filter(disp>14)

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1:10,2:3]

# 10 pierszych wierszy, 2 i 3 kolumna?


# Jak wybieramy kolumny po nazwach? 
mtcars[c("cyl","mpg")]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[c("am","wt","mpg")]


# Jak wybierać jedną kolumnę?



# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)


# 2. Jakie są typy zmiennych?
class(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

length(unique(mtcars[c("cyl")]))

table(mtcars$cyl)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 

mean(mtcars$drat[mtcars$cyl==4])
# o wartości zmiennej "cyl" równej 4?


# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

plot(mtcars$mpg,mtcars$hp)

# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot


# Zmienna "cyl" - barplot
barplot(mtcars$cyl)

# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.
data <- employees[employees$name=="John" & employees$surname=="Insecure",]

data


for(pass in top1000passwords){
  score <- proton(action = "login", login="johnins", password=pass)
  
  if(score==TRUE){
    print(score)
  }
}

x <- logs[logs$login=="Pietraszko",]$host
x <- as.data.frame(x,colnames("host"))
x
x%>%arrange(host)
# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------
