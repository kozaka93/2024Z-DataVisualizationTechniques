# 5. Zadanie "proton" -----------------------------------------------------

# Utwórz nowy plik .R. Zainstaluj/uruchom pakiet proton:
install.packages("proton")
library(proton)
proton()

# W pliku zapisuj sposób rozwiązania gry.
employees

employees[employees$name == "John" & employees$surname == "Insecure",]
#johnins

proton(action = "login", login="johnins")

#Problem 2

top1000passwords

for (variable in top1000passwords) {
  proton(action = "login", login="johnins", password=variable)
}

#Problem 3
logs

employees[employees$surname == "Pietraszko",]

data.frame(table(logs[logs$login == "slap", c("host")]))

proton(action = "server", host="194.29.178.16")

#Problem 4
bash_history

?strsplit
split_bash_history <- strsplit(bash_history, " ")

comands <- c()

for (x in split_bash_history) {
  comands <- c(comands, x[[1]])
}

for (comand in unique(comands)) {
  proton(action = "login", login = "slap", password = comand)
}

# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------
