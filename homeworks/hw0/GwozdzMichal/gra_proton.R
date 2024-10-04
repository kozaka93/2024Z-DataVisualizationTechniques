proton()

head(employees)
login_john <- employees[employees$name == "John" & employees$surname == "Insecure", c("login")]
proton(action = "login", login = login_john)
head(top1000passwords)

for (i in top1000passwords){
  proton(action = "login", login= login_john, password= i)
}

head(logs)

# 6. Zamieszczamy rozwiązanie na repozytorium -----------------------------
employees[employees$surname == "Pietraszko", c("login")]
max_logins <- max(table(logs[logs$login == "slap", c("host")]))
str(table(logs[logs$login == "slap", c("host")]))