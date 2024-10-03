library(proton)


haslo <- function(x){
  proton(action = "login", login="johnins", password=x)
}