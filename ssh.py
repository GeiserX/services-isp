import sys
from pexpect import pxssh
s = pxssh.pxssh()
s.login(sys.argv[1], "admin", "password")  
s.sendline("mca-status")
s.prompt()
print s.before
s.logout()

###### CÓDIGO R ANTIGUO:

#             print("adios")
#              
#              python.exec(python.code = sprintf(
# 'from pexpect import pxssh
# s = pxssh.pxssh()
# s.login("%s", "admin", "t1l2cm3r")  
# s.sendline("mca-status")
# s.prompt()
# stats = s.before
# s.logout()',
#                           tabla$`IP Antena`[1])) 
#              
#              print("hola")
#              print(python.get("stats"))
#              print("hecho")
#              stats = python.get("stats")
             
             stats = system(sprintf("python /home/tecnico/WebServicios/ssh.py %s", tablaAntena$`IP Antena`[i]), intern = T)
             stats <- gsub(x = stats[-1], pattern = "\r\r", replacement = "")
#              statsAntena <- strsplit(stats, "\r\r\n")
#              statsAntenaSplit <- strsplit(statsAntena[[1]], "=")
             
             statsAntenaSplit <- strsplit(stats, "=")
             
             tableSSHAntena <- data.frame(matrix(nrow = 1, ncol = 0))
             
             #primeraLinea <- strsplit(statsAntena[[1]][1], ",")
             primeraLinea <- strsplit(stats[1], ",")
             
             tableSSHAntena$Firmware <- strsplit(primeraLinea[[1]][grep("firmwareVersion", primeraLinea[[1]])], "=")[[1]][2]
             tableSSHAntena$Hardware <- strsplit(primeraLinea[[1]][grep("platform", primeraLinea[[1]])], "=")[[1]][2]
             
             tableSSHAntena$`Conectado a` <- statsAntenaSplit[[7]][2]
             tableSSHAntena$`WlanUptime / Uptime` <- paste(as.numeric(statsAntenaSplit[[6]][2])/60/60/24, "/", 
                                                           as.numeric(statsAntenaSplit[[12]][2])/60/60/24, "días")
             tableSSHAntena$`Frecuencia` <- paste(statsAntenaSplit[[8]][2], "MHz")
             tableSSHAntena$SNR <- paste(statsAntenaSplit[[9]][2], statsAntenaSplit[[10]][2], sep = "/")
             tableSSHAntena$CCQ <- paste(substr(statsAntenaSplit[[11]][2], 0,2), substr(statsAntenaSplit[[11]][2], 2,3), sep=",")
             tableSSHAntena$Distancia <- paste(statsAntenaSplit[[18]][2], "metros")
             tableSSHAntena$`TX-RX` <- paste(statsAntenaSplit[[21]][2], statsAntenaSplit[[22]][2], sep = "/")
             tableSSHAntena$Latencia <- paste(statsAntenaSplit[[23]][2], "ms")
             tableSSHAntena$`Lan Speed` <- statsAntenaSplit[[34]][2]
             
             
             output$sshAntena <- renderDataTable(tableSSHAntena, options = list(searching = FALSE, pageLength = 5))
