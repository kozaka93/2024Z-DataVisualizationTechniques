install.packages("proton")

library(proton)

proton()
proton(action = "login", login="XYZ")

employees[employees$name == "John", ]
#johnins

proton(action = "login", login="johnins")
top1000passwords
for ( i in 1:1000){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}
logs
logs[logs$login == "slap", ]

cos <- table(logs$host[logs$login == "slap"])
x <- sort(cos, decreasing = TRUE)
x
proton(action = "server", host="194.29.178.16")
employees[employees$surname == "Pietraszko", ]
bash_history
