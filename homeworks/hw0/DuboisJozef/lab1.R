library(proton)


head(employees)

employees[employees$name == "John",]

# johnins


proton(action = "login", login="johnins")


for (i in 1:1000){
  if(proton(action = "login", login="johnins", password=top1000passwords[i]) == "`Success! User is logged in!`.") {
    top1000passwords[i]
  }
}

head(logs)

employees[employees$surname == "Pietraszko",]


table(logs[logs$login == "slap", "host"])


for (i in 1:length(logs$login)){
  if (as.numeric(table(logs[logs$login == "slap", "host"])[i]) > 100){i}
    
} # to nie dziala


table(logs[logs$login == "slap", "host"])



proton(action = "server", host="194.29.178.16")


length(unique(bash_history))

unique(bash_history)

DHbb7QXppuHnaXGN

proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")
