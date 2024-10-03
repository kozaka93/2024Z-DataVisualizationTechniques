install.packages("proton")
library(proton)
proton()
employees[employees$surname=='Insecure',]
proton(action="login",login="johnins")
for(x in 1:length(top1000passwords)) {
  proton(action="login",login="johnins",password=top1000passwords[x])
}
proton(action = "server", host="XYZ")
employees[employees$surname=="Pietraszko",]
names(which.max(table(logs[logs$login=="slap",'host'])))
proton(action = "server", host="194.29.178.16")
bash_history