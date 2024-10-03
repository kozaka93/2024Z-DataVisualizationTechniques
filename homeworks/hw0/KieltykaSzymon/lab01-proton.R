install.packages("proton")
library(proton)
proton()

employees[employees$surname == "Insecure",]
proton(action = "login", login = "johnins")

for (l in top1000passwords) {
  proton(action = "login", login = "johnins", password = l)
}

dat <- as.data.frame(table(logs[logs$login == "slap", c("host")]))
proton(action = "server", host="194.29.178.16")

commands <- c()
for (x in strsplit(bash_history, " ")) {
  commands <- c(commands, x[[1]])
}                     

for (x in unique(commands)) {
  proton(action = "login", login = "slap", password = x)
}

                     