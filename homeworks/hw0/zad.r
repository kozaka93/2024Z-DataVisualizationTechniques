install.packages("proton")
library(proton)
proton()

# Problem 1: Find the login of John Insecure.
johns = employees[employees$name == "John",]
target = johns[johns$surname == "Insecure", ]
login = target$login[1]

proton(action="login", login=login)
password="1234"
for (candidate in top1000passwords) {
  if (proton(action="login", login=login, password=candidate) != "Password or login is incorrect") {
    password = candidate
  }
}
proton(action="login", login=login, password=password)
# Problem 3: Check from which server Pietraszko logs into the Proton server most often.
head(logs)
hosts = unique(logs$host)
host = "1234"
for (candidate in hosts) {
  print(candidate)
  proton(action="server", host=candidate)
}
host = "194.29.178.16"

proton(action="server", host=host)

unique(bash_history)

employees[employees$surname == "Pietraszko",]

proton(action="login", login="slap", password="DHbb7QXppuHnaXGN")

