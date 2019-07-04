# RSelenium::checkForServer() #para instalar
# nav$getCurrentUrl()
# “xpath”, “css selector”, “id”, “name”, “tag name”, 
# “class name”, “link text”, “partial link text”
# nav$getPageSource()
# nav$screenshot(display = TRUE)
# pJS$stop()



library("RSelenium")
library("stringr")
pJS <- phantom(extras = "--ignore-ssl-errors=true --ssl-protocol=tlsv1", port = 4449)
Sys.sleep(2)
nav <- remoteDriver(remoteServerAddr = "localhost", port = 4446, browserName = "phantomjs")

nav$open()

while(TRUE){
  ptm <- proc.time()
  print(c("Start ", strptime(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  
  Sys.sleep(2)
  nav$navigate("https://airenetworks.es/")
  geco <- nav$findElement(using = "partial link text", value = "Oficina Virtual")
  geco$clickElement()
  
  Sys.sleep(2)
  nav$switchToWindow(nav$getWindowHandles()[[1]][2]) #Cambiar ventana
  user <- nav$findElement(using = "id", value = "user")
  user$sendKeysToElement(list("user"))
  pass <- nav$findElement(using = "id", value = "pass")
  pass$sendKeysToElement(list("password"))
  enter <- nav$findElement(using = "id", value = "button")
  enter$clickElement()
  
  frame <- nav$findElement("name", "topFrame")
  nav$switchToFrame(frame)
  telefoniaFIJA <- nav$findElement(using = "id", value = "divoCMenu0_1")
  telefoniaFIJA$clickElement()
  nav$switchToFrame(NULL) # Volvemos al frame principal
  frame2 <- nav$findElement("name", "bajo")
  nav$switchToFrame(frame2)
  consumo <- nav$findElement(using = "xpath", value = "//*[contains(text(), 'DID Fija')]")
  consumo$clickElement()
  resumen <- nav$findElement(using = "xpath", value = "//*[contains(text(), 'Resumen Facturacion')]")
  resumen$clickElement()
  
  option <- nav$findElement("xpath", "//*/option[@value = 'cdr_164']")
  option$clickElement()
  
  Buscar <- nav$findElement(using = "name", value = "busq")
  Buscar$clickElement()
  Sys.sleep(2)
  
  td <- nav$findElement(using = "xpath", value = "//td")
  csv <- td$getElementText()
  csv <- gsub("   ", "\n\n", csv, fixed=T)
  csv <- gsub("\n\n\n", "\n\n", csv, fixed=T)
  csv <- strsplit(x = csv,"\n",fixed = T)
  
  csv <- c(csv[[1]][1:2179], "", csv[[1]][2180:length(csv[[1]])])
  
  NumLineas <- as.double(word(csv[167], -1))
  tabla <- data.frame(matrix(ncol = 0, nrow = NumLineas))
  j <- 1
  for(i in 0:(NumLineas-1)){
    tabla$Telefono[j] <- csv[i*6+175]
    tabla$Anotaciones[j] <- csv[i*6+176]
    tabla$Duracion[j] <- csv[i*6+177]
    tabla$Base[j] <- as.numeric(gsub(",", ".", csv[i*6+178]))
    tabla$Beneficio[j] <- as.numeric(gsub(",", ".", csv[i*6+179]))
    tabla$Total[j] <- as.numeric(gsub(",", ".", csv[i*6+180]))
    j=j+1
  }
 
  tabla <- tabla[tabla$Total >= 10,]
  
  write.csv(tabla, "/home/tecnico/WebServicios/data/listaEXCEEDfijo.csv")
  
  print(c("Done ", strptime(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  delay <- proc.time() - ptm
  Sys.sleep(30*60 - delay[[3]])
}
