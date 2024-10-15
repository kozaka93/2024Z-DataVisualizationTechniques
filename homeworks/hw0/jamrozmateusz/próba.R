library(proton)
proton()
proton(employees)
employees
employees[employees$surname=='Insecure',]
proton(action = "login", login="johnins")
top1000passwords
for(i in 1:length(top1000passwords)){
  if ((proton(action = "login", login="johnins", password=top1000passwords[i]))=='Success! User is logged in!')
    
    proton(action = "login", login="johnins", password=top1000passwords[i])
  
}
l <- logs[logs$login=="johnins",]
max(table(l$host))

