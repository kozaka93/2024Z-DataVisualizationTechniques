library(proton)
library(stringi)

proton()

# 1
login <- employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login = login)

# 2
top1000passwords
login

ans <- ""
i = 1
while (i < 1000) {
  ans <- top1000passwords[i]
  if(proton(action = "login", login=login, password=ans) == "Success! User is logged in!"){
    break
  }
  i <- i+1
}

proton(action = "login", login=login, password=ans)
head(logs)

login <- employees[employees$surname == "Pietraszko", "login"]

host <- sort(table(logs[logs$login == login,]$host), decreasing = TRUE)[1]
host <- "194.29.178.16"
proton(action = "server", host=host)

bash_history

proton(action = "login", login=login, password="")
