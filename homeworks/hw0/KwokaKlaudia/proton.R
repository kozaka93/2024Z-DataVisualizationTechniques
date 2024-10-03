When you finally find out what John's login is, use `proton(action = "login", login="XYZ")` command, where XYZ is Insecure's login.

Use `proton(action = "login", login="XYZ", password="ABC")` command in order to log into the Proton server with the given credentials.

for(i in 1:1000){
  pass <- top1000passwords[i]
  proton(action = "login", login="johnins", password=pass)
}

Now, Bit wants to check from which workstation Pietraszko is frequently logging into the Proton server. Bit hopes that there will be some useful data.  

Logs are in the `logs` dataset. 
Consecutive columns contain information such as: who, when and from which computer logged into Proton.

Problem 3: Check from which server Pietraszko logs into the Proton server most often.

Use `proton(action = "server", host="194.29.178.16")` command in order to learn more  about what can be found on the XYZ server.
The biggest chance to find something interesting is to find a server from which Pietraszko logs in the most often.

slap
