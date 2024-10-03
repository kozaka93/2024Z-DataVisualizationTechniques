install.packages("proton")
library(proton)
proton()
View(employees)
employees[employees$name == "John" & employees$surname == "Insecure", ]
proton(action = "login", login = employees[employees$name == "John" & employees$surname == "Insecure", "login"])
l <- employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login = l, password = top1000passwords)
for (pass in top1000passwords){
  proton(action = "login", login = l, password = pass)
}
employees[employees$surname == "Pietraszko", ]
sort(table(logs[logs$login == "slap", "host"]))
proton(action = "server", host = "194.29.178.16")
View(bash_history)
?strsplit
head(bash_history)
df <- strsplit(bash_history, " ")
commands <- c()
for (x in df){
  commands <- c(commands, x[[1]])
}
commands
for (command in unique(commands)){
  proton(action = "login", login = "slap", password = command)
}
