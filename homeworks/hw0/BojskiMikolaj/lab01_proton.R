library(proton)
proton()
data("employees")
typeof(top1000passwords)
proton(action = "login", login="johnins")
for (i in 1:length(top1000passwords)) {
  
proton(action = "login", login="johnins", password=top1000passwords[i])
}
unique(logs$host)
for (i in 1:length(unique(logs[['host']]))) {
  
  proton(action = "server", host=logs[['host']][i])

}
proton(action = "server", host="XYZ")
