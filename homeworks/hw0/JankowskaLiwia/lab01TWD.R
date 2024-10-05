# Gierka

install.packages("proton")
library(proton)
proton()

employees
employees[employees$surname == "Insecure", c("login")]

johnins

proton(action = "login", login = "johnins")

top1000passwords

for(pass in top1000passwords)
  response <- proton(action = "login", login="johnins", password=pass)
if (response == "Success! User is logged in!"){
  cat(pass)
}

employees[employees$surname == "Pietraszko", c("login")]

table(logs[logs$login == "slap", c("host")])

# Jak znaleźć tą najczęstszą stację?
# 194.29.178.16

proton(action = "server", host="194.29.178.16")

bash_history

split_bash_history <- strsplit(bash_history, " ")

comands <- c()

for (x in split_bash_history){
  comands <- c(comands, x[[1]])
}

for (comand in unique(comands)){
  proton(action = "login", login = "slap", password = comand)
}




