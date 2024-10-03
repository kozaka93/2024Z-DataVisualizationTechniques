library(proton)

proton()

employees[employees$surname == "Pietraszko", ]


proton(action = "login", login="johnins")
proton(action = "login", login="johnins", password="ABC")

top1000passwords[1]

for (i in 1:1000) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

logs
proton(action = "server", host="194.29.178.13")

logs[logs$login == "johnins", c("host")]

logs[logs$login == "johnins", ]

logs

employees

dim(logs[logs$login == "johnins" & logs$host == "194.29.178.13", ])

max(table(logs[logs$login == "slap", c("host")])) # 65

new_table <- table(logs[logs$login == "slap", c("host")]) == 112 # 65

which(new_table)

sum(table(logs[logs$login == "slap", c("host")]) == 112)



