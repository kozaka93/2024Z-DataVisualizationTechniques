library(proton)
proton()

head(employees)
typeof(employees)
employees[employees$surname==c("Pietraszko"),]
?lapply

for(x in 1:1000){
  proton(action = "login", login="johnins", password=top1000passwords[x])
}

proton(action = "login", login="johnins", password=)
head(logs)
table(logs[logs$login=="slap",]$host)

head(bash_history)

proton(action = "server", host="194.29.178.16")
