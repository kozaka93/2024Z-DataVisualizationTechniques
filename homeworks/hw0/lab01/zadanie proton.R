proton()

employees[employees$surname == "Insecure",]

proton(action = "login", login = "johnins")

top1000passwords

?sapply

proton(action = "login", login = "johnins", password = top1000passwords)
sapply(top1000passwords, proton(action = "login", login = "johnins", password = top1000passwords))

for (i in 100:130) {
  print(proton(action = "login", login = "johnins", password = top1000passwords[i])) 
}

top1000passwords[120]
proton(action = "login", login = "johnins", password = top1000passwords[120])



employees[employees$surname == "Pietraszko",]
max(table(logs[logs$login == "slap","host"]))
table(logs[logs$login == "slap","host"])[table(logs[logs$login == "slap","host"]) == 112]
logs[logs$login == "slap",]
length(unique(logs[logs$login == "slap","host"]))

?table

# 194.29.178.16

proton(action = "server", host="194.29.178.16")


y <- strsplit(bash_history, " ")
cat(y[[1]])
for(q in y){
  if q[[1]] in 
}

?cat
