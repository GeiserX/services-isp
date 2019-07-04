# RSelenium::checkForServer() #para instalar
# nav$getCurrentUrl()
# “xpath”, “css selector”, “id”, “name”, “tag name”, 
# “class name”, “link text”, “partial link text”
# nav$getPageSource()
# nav$screenshot(display = TRUE)
library("stringr")
Sys.sleep(10)

print("-1")
library("RSelenium")
print("0")
pJS <- phantom(extras = "--ignore-ssl-errors=true --ssl-protocol=tlsv1", port = 4449)
print("1")
Sys.sleep(2)
nav <- remoteDriver(remoteServerAddr = "localhost", port = 4448, browserName = "phantomjs")
print("2")
Sys.sleep(2)

nav$open()
print("3")

while(TRUE){
  ptm <- proc.time()
  print(c("Start ", strptime(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  
  nav$navigate("https://airenetworks.es/")
  Sys.sleep(2)
  geco <- nav$findElement(using = "partial link text", value = "Oficina Virtual")
  geco$clickElement()
  
  Sys.sleep(2)
  #nav$executeScript("virtual()")
  nav$switchToWindow(nav$getWindowHandles()[[1]][2]) #Cambiar ventana
  user <- nav$findElement(using = "id", value = "user")
  user$sendKeysToElement(list("user"))
  pass <- nav$findElement(using = "id", value = "pass")
  pass$sendKeysToElement(list("password"))
  enter <- nav$findElement(using = "id", value = "button")
  enter$clickElement()
  
  Sys.sleep(2)
  frame <- nav$findElement("name", "topFrame")
  nav$switchToFrame(frame)
  telefonia <- nav$findElement(using = "id", value = "divoCMenu0_3")
  telefonia$clickElement()
  nav$switchToFrame(NULL) # Volvemos al frame principal
  frame2 <- nav$findElement("name", "bajo")
  nav$switchToFrame(frame2)
  consumo <- nav$findElement(using = "xpath", value = "//*[contains(text(), 'Detalle de co')]")
  consumo$clickElement()
  resumen <- nav$findElement(using = "xpath", value = "//*[contains(text(), 'Resumen Facturacion Movil')]")
  resumen$clickElement()
  
  Sys.sleep(2)
  Buscar <- nav$findElement(using = "name", value = "busq")
  Buscar$clickElement()
  Sys.sleep(2)
  
  # download <- nav$findElement(using = "partial link text", value = "Exportar a CSV")
  # download$clickElement()
  
  td <- nav$findElement(using = "xpath", value = "//td")
  csv <- td$getElementText()
  csv <- str_split(csv, "\n")
  
  NumLineas <- as.double(word(csv[[1]][23], -1))
  
  tabla <- data.frame(matrix(ncol = 0, nrow = NumLineas))
  j <- 1
  for(i in 0:(NumLineas-1)){
    tabla$Telefono[j] <- csv[[1]][i*6+31]
    tabla$Titular[j] <- csv[[1]][i*6+32]
    tabla$Duracion[j] <- csv[[1]][i*6+33]
    tabla$Tarifa[j] <- as.numeric(gsub(",", ".", csv[[1]][i*6+34]))
    tabla$Extra[j] <- as.numeric(gsub(",", ".", csv[[1]][i*6+35]))
    tabla$Total[j] <- as.numeric(gsub(",", ".", csv[[1]][i*6+36]))
    j=j+1
  }
  
  for(i in 1:dim(tabla)[1]){
    tabla$Alarma[i] <- if(tabla$Extra[i] > tabla$Tarifa[i]*0.5) 1 else 0
  }
  tablaConjunto <- tabla[tabla$Alarma == 1, -7]
  
  tablaEXCEED <- tablaConjunto[tablaConjunto$Tarifa != 0.0000,]
  tablaZERO <- tablaConjunto[tablaConjunto$Tarifa == 0.0000,]
  
  tablaZERO <- tablaZERO[tablaZERO$Extra > 5,]
  
  tablaEXCEEDED <- rbind(tablaEXCEED, tablaZERO)
  write.csv(tablaEXCEEDED, "/home/tecnico/WebServicios/data/listaEXCEED.csv")
  
  print(c("Done ", strptime(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  delay <- proc.time() - ptm
  Sys.sleep(30*60 - delay[[3]])
}
