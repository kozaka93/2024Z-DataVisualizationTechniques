library(proton)

proton()
data(employees)
employees[employees$surname=="Insecure",]
proton(action = "login", login="johnins")
data("top1000passwords")

for (i in 1:length(top1000passwords)){
proton(action = "login", login="johnins", password=top1000passwords[i])}

data(logs)

employees[employees$surname=="Pietraszko",]

x <- table(logs[logs$login=="slap","host"])
sort(x,dec=TRUE)
y <- as.data.frame(x)

proton(action = "server", host="194.29.178.16")

data("bash_history")

bash_history[!grepl(" ",bash_history)]

proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")
