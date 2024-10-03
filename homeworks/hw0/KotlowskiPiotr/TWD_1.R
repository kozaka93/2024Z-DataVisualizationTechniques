

data(mtcars)
head(mtcars)



# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[0,]


# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,]
mtcars[,1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,2:3]

# Jak wybieramy kolumny po nazwach? 
mtcars[,c("mpg","cyl")]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c("am","wt","mpg")]

# Jak wybierać jedną kolumnę?
mtcars$mpg
# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars[,c("cyl")])
length(unique(mtcars[,c("cyl")]))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów 
# o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl==4,c("drat")])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
table(mtcars$am)


# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg,mtcars$hp)

# Zmienna "cyl" - barplot
barplot(mtcars$cyl)
barplot(table(mtcars$cyl))
# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
#install.packages("proton")
library(proton)

# W pliku zapisuj sposób rozwiązania gry.
# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------
proton()
data("employees")
employees[employees$name=="John" & employees$surname=="Insecure",c("login")]
proton(action="login",login="johnins")


for (pass in top1000passwords){
  response<-proton(action="login",login="johnins",password=pass)
  if (response =="Success! User is logged in!"){
   cat(pass)
  }
}

employees[employees$surname=="Pietraszko",]
result<-data.frame(table(logs[logs$login=="slap",c("host")]))


proton(action = "server", host="194.29.178.16")

comands<-c()

split_bash_history<-strsplit(bash_history," ")
for (x in split_bash_history){
  comands<-c(comands,x[[1]])
}
for (comand in unique(comands)){
  proton(action="login",login="slap",password=comand)
  
}

     