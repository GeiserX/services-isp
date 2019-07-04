# shiny::runApp(appDir = "/home/tecnico/WebServicios", launch.browser = FALSE, port = 8080, host = "0.0.0.0")

options(scipen=999)
library(shiny)
library(stringr)
library(readr)
library(RMySQL)
library(shinyjs)
library(RSelenium)
library(rPython)
library(knitr)

shinyServer(
  function(input, output, session) {
    
    observeEvent(input$searchSinEXT,{
      if(input$numClienteSinEXT != "" || input$nombreClienteSinEXT != ""){
        shinyjs::disable("searchSinEXT")
        output$errorAntena <- renderUI({ tags$h3("") })
        output$alarma <- renderUI({ tags$h3("") })
        output$resultado <- DT::renderDataTable({ NULL })
        output$resultadoAntena <- DT::renderDataTable({ NULL })
        output$resultadoTraficoDATOS <- DT::renderDataTable({ NULL })
        
        radius <- dbConnect(MySQL(), user="root", password="PASSWORD", db="radius", host="x.x.x.x")
        services <- dbGetQuery(radius, "SELECT srvid, srvname FROM rm_services")
        
        if(input$numClienteSinEXT != ""){
          user <- dbGetQuery(radius, sprintf("SELECT username, firstname, srvid, phone, mobile, address, city, state, expiration, lastlogoff, comment FROM rm_users WHERE username LIKE '%s%%'", input$numClienteSinEXT))
          #ip <- dbGetQuery(radius, sprintf("SELECT username, acctstoptime FROM radacct WHERE username LIKE '%s%%'", input$numClienteSinEXT))
        }
        else{
          user <- dbGetQuery(radius, paste0("SELECT username, firstname, srvid, phone, mobile, address, city, state, expiration, lastlogoff, comment FROM rm_users WHERE firstname LIKE '%", input$nombreClienteSinEXT, "%'"))
          #ip <- dbGetQuery(radius, sprintf("SELECT username, acctstoptime FROM radacct WHERE firstname LIKE '%s%%'", input$nombreClienteSinEXT))
        }
        
        radacct <- vector(mode = "logical")
        for(i in 1:dim(user)[1]){
          user$srvid[i] <- services$srvname[services$srvid %in% user$srvid[i]] 
          radacct[i] <- tail(dbGetQuery(radius, sprintf("SELECT acctstoptime FROM radacct WHERE username = '%s'", user$username[i])),1)
          radacct[i] <-  if(is.na(radacct[i])) TRUE else FALSE
        }
        
        dbDisconnect(radius)
        
        names(user) <- c("Cliente", "Nombre", "Servicio", "Teléfono", "Móvil", "Dirección", "Ciudad", "Región", "Expiración", "Última desconexión", "Comentario")
        
        for(i in 1:dim(user)[1]){
          if(radacct[[i]] == T){
            user$Cliente[i] <- sprintf('<span style="color:green"><b>%s</b></span>', user$Cliente[i])
          } else {
            user$Cliente[i] <- sprintf('<span style="color:red"><b>%s</b></span>', user$Cliente[i])
          }
        }
        
        output$resultado <- DT::renderDataTable(user, options = list(searchable = FALSE, pageLength = 5), escape = FALSE)
        
      }
    })
    
    
    
    
    
    observeEvent(input$search,{
      if(input$numCliente != "" || input$nombreCliente != ""){
        shinyjs::disable("search")
        output$errorAntena <- renderUI({ tags$h3("") })
        output$alarma <- renderUI({ tags$h3("") })
        output$resultado <- DT::renderDataTable({NULL})
        output$resultadoAntena <- DT::renderDataTable({NULL})
        output$resultadoTraficoDATOS <- DT::renderDataTable({ NULL })
        
        radius <- dbConnect(MySQL(), user="root", password="PASSWORD", db="radius", host="X.X.X.X")
        nases <- dbGetQuery(radius, "SELECT nasname, shortname FROM nas ORDER BY shortname")
        services <- dbGetQuery(radius, "SELECT srvid, srvname FROM rm_services")
        connections <- dbGetQuery(radius, sprintf("SELECT nasipaddress, framedipaddress, acctstarttime, acctstoptime, callingstationid, acctoutputoctets, acctinputoctets
                                                   FROM radacct WHERE username = '%s' ORDER BY radacctid DESC", input$numCliente))

        ip <- tail(dbGetQuery(radius, sprintf("SELECT nasipaddress, framedipaddress, acctstoptime, callingstationid FROM radacct WHERE username = '%s'", input$numCliente)),1)
        user <- dbGetQuery(radius, sprintf("SELECT firstname, staticipcpe, srvid, username FROM rm_users WHERE username = '%s'", input$numCliente))

        
        dbDisconnect(radius)
        
        if(dim(user)[1] == 0) {
          output$alarma <- renderUI({ tags$h3("No existe este cliente PPPoE") })
          
        } else {
        
        tabla <- data.frame(matrix(ncol=7, nrow=1))
        names(tabla) <- c("Cliente", "Nombre", "Servicio", "IP", "Tipo IP", "MAC CPE", "NAS")
        tabla$Cliente <- user$username
        tabla$Nombre <- user$firstname
        tabla$Servicio <- services$srvname[services$srvid %in% user$srvid] 
        
        
        
        if(dim(ip)[1] == 0) {
          output$alarma <- renderUI({ tags$h3("Este cliente no ha usado nunca su cliente PPPoE") })
          output$resultado <- DT::renderDataTable(tabla, options = list(searching = FALSE, paging = FALSE))
          
        } else {
      
          if(is.na(ip$acctstoptime)){
            tabla$Cliente <- sprintf('<span style="color:green"><b>%s</b></span>', tabla$Cliente)
          } else {
            tabla$Cliente <- sprintf('<span style="color:red"><b>%s</b></span>', tabla$Cliente)
          }
          
            
          tabla$IP <- sprintf('<a href="http://%s:8080" target="_blank">%s</a>', ip$framedipaddress, ip$framedipaddress)
          tabla$`Tipo IP` <- if(user$staticipcpe == "") "Dinámica" else "Estática"
          tabla$`MAC CPE` <- ip$callingstationid
          tabla$NAS <- nases$shortname[nases$nasname %in% ip$nasipaddress]
          tabla$NAS <- sprintf('<a href="http://%s" target="_blank">%s</a>',tail(strsplit(tabla$NAS, " ")[[1]],1), tabla$NAS)
          
          isolate({
            output$resultado <- DT::renderDataTable(tabla, options = list(searching = FALSE, paging = FALSE), escape = FALSE)
          })
          
          MikroTik <- system(ignore.stdout = F, ignore.stderr = T, command = sprintf('python3 /home/tecnico/WebServicios/GetAntena.py %s', nases$nasname[nases$nasname %in% ip$nasipaddress]), intern = T)
          posicion <- grep(sprintf(">>>=host-name=%s", paste0(gsub("t", "", user$username), str_split(str_split(user$firstname, " ")[[1]][1], "")[[1]][1])), str_replace_all(MikroTik, fixed(" "), ""))
          
          if(length(posicion) == 0) {
            output$errorAntena <- renderUI({ tags$h3("No se encuentra la antena del cliente. Puede ser debido a: 
                                                     \n 1. Nombre de antena incorrecto \n 2. Antena configurada como router \n 3. Antena sin conectar por +3 días \n 4. Cliente de ADSL") })
          }
          else {
            
            tablaAntena <- data.frame(matrix(ncol=4, nrow=1))
            names(tablaAntena) <- c("IP Antena", "Expira en", "MAC Antena", "Nombre Antena")
            
            for(i in 1:length(posicion)){
              if(i >= 2){
                tablaAntena[i,] <- NA
              }
              
              tablaAntena$`IP Antena`[i] <- sprintf('<a href="http://%s" target="_blank">%s</a>', str_split(MikroTik[posicion[i]-4], "=")[[1]][3], str_split(MikroTik[posicion[i]-4], "=")[[1]][3])
              tablaAntena$`Expira en`[i] <- str_split(MikroTik[posicion[i]-6], "=")[[1]][3]
              tablaAntena$`MAC Antena`[i] <- str_split(MikroTik[posicion[i]-3], "=")[[1]][3]
              tablaAntena$`Nombre Antena`[i] <- str_split(MikroTik[posicion[i]], "=")[[1]][3]
            }
            
            isolate({
              output$resultadoAntena <- DT::renderDataTable(tablaAntena, options = list(searching = FALSE, paging = FALSE), escape = FALSE)
            })
            
            
          }
        }
        }
        
        ## TABLA DE CONEXIONES ##
        names(connections) <- c("IP NAS", "IP CPE", "Start", "Stop", "MAC CPE", "Download", "Upload")
        for(i in 1:dim(connections)[1]){
          if(as.numeric(connections$Download[i]) < 1024){
            connections$Download[i] <- paste(as.character(connections$Download[i]), "B")
          } 
          else if(as.numeric(connections$Download[i]) >= 1024 && as.numeric(connections$Download[i]) < 1024*1024){
            connections$Download[i] <- paste(as.character(round(as.numeric(connections$Download[i])/1024, digits = 2)), "KB")
          } 
          else if (as.numeric(connections$Download[i]) >= 1024*1024 && as.numeric(connections$Download[i]) < 1024*1024*1024){
            connections$Download[i] <- paste(as.character(round(as.numeric(connections$Download[i])/(1024*1024), digits = 2)), "MB")
          } 
          else if (as.numeric(connections$Download[i]) >= 1024*1024*1024 && as.numeric(connections$Download[i]) < 1024*1024*1024*1024){
            connections$Download[i] <- paste(as.character(round(as.numeric(connections$Download[i])/(1024*1024*1024), digits = 2)), "GB")
          }
          else if (as.numeric(connections$Download[i]) >= 1024*1024*1024*1024 && as.numeric(connections$Download[i]) < 1024*1024*1024*1024*1024){
            connections$Download[i] <- paste(as.character(round(as.numeric(connections$Download[i])/(1024*1024*1024*1024), digits = 2)), "TB")
          }
        }
        for(i in 1:dim(connections)[1]){
          if(as.numeric(connections$Upload[i]) < 1024){
            connections$Upload[i] <- paste(as.character(connections$Upload[i]), "B")
          } 
          else if(as.numeric(connections$Upload[i]) >= 1024 && as.numeric(connections$Upload[i]) < 1024*1024){
            connections$Upload[i] <- paste(as.character(round(as.numeric(connections$Upload[i])/1024, digits = 2)), "KB")
          } 
          else if (as.numeric(connections$Upload[i]) >= 1024*1024 && as.numeric(connections$Upload[i]) < 1024*1024*1024){
            connections$Upload[i] <- paste(as.character(round(as.numeric(connections$Upload[i])/(1024*1024), digits = 2)), "MB")
          } 
          else if (as.numeric(connections$Upload[i]) >= 1024*1024*1024 && as.numeric(connections$Upload[i]) < 1024*1024*1024*1024){
            connections$Upload[i] <- paste(as.character(round(as.numeric(connections$Upload[i])/(1024*1024*1024), digits = 2)), "GB")
          }
          else if (as.numeric(connections$Upload[i]) >= 1024*1024*1024*1024 && as.numeric(connections$Upload[i]) < 1024*1024*1024*1024*1024){
            connections$Upload[i] <- paste(as.character(round(as.numeric(connections$Upload[i])/(1024*1024*1024*1024), digits = 2)), "TB")
          }
        }
        
        
        connections$Start <- str_replace_all(connections$Start, "-", "/")
        connections$Stop <- str_replace_all(connections$Stop, "-", "/")
        
        was.na <- FALSE
        if(is.na(connections$Stop[1])) {
          was.na <- TRUE
          connections$Stop[1] <-  format(Sys.time(), "%Y/%m/%d %H:%M:%S") ## Para los cálculos, hacemos que stoptime sea ahora, luego lo ponemos a NA de nuevo
        }
        
        connections$TRY <- difftime(as.POSIXct(connections$Stop), as.POSIXct(connections$Start), units = "secs")
        connections$`Online Time` <- NA
        
        for(i in 1:dim(connections)[1]){
          if(as.numeric(connections$TRY[i]) < 60){
            connections$`Online Time`[i] <- paste(round(difftime(as.POSIXct(connections$Stop[i]), as.POSIXct(connections$Start[i]), units = "secs"), 2), "secs")
          } 
          else if(as.numeric(connections$TRY[i]) >= 60 && as.numeric(connections$TRY[i]) < 60*60){
            connections$`Online Time`[i] <- paste(round(difftime(as.POSIXct(connections$Stop[i]), as.POSIXct(connections$Start[i]), units = "mins"), 2), "mins")
          } 
          else if(as.numeric(connections$TRY[i]) >= 60*60 && as.numeric(connections$TRY[i]) < 60*60*24){
            connections$`Online Time`[i] <- paste(round(difftime(as.POSIXct(connections$Stop[i]), as.POSIXct(connections$Start[i]), units = "hours"), 2), "hours")
          } 
          else if(as.numeric(connections$TRY[i]) >= 60*60*24){
            connections$`Online Time`[i] <- paste(round(difftime(as.POSIXct(connections$Stop[i]), as.POSIXct(connections$Start[i]), units = "days"), 2), "days")
          } 
        }
        
        connections <- connections[,-8]
        connections <- connections[,c(1:4,8,5:7)]
        
        if(was.na) connections$Stop[1] <- NA
        
        output$resultadoTraficoDATOS <- DT::renderDataTable(connections, options = list(searching = FALSE, pageLength = 5))
        
        shinyjs::enable("search")
      }
    })
  
    
    
    
    
    
    
    
    
    output$listaMoviles <- DT::renderDataTable({
      
      if(input$selectGECO == "Móvil")
        read.csv("/home/tecnico/WebServicios/data/listaEXCEED.csv")[,-1]
      else
        read.csv("/home/tecnico/WebServicios/data/listaEXCEEDfijo.csv")[,-1]
      
    })
    
    
    
    
    
    
    
    
    
    
    
    output$firma <- renderUI({
      
      archivo <-  '
                   <style>

                   #contenedor {
                   position: relative;
                   height:220px;
                   padding:10px;
    }
                   
                   #left {
                   float:left;
  }
                   
                   .divider{
                   position:absolute;
                   left:250px;
                   top:5px;
                   bottom:5px;
                   border-left:1px solid grey;
                   }
                   
                   #right {
                   position:absolute;
                   left:270px;
                   width:390px;
                   height:200px;
                   float:right;
                   }
                   
                   p {
                   font-family: "Trebuchet MS", Helvetica, sans-serif;
                   }
                   
                   </style>
                   
                   
                   <hr style="width:670px;" align="left" id="pr">
                   <div id= "contenedor">
                   
                   <div id="left">
                   <img src="http://s3.postimg.org/r3opa07kj/logo_Emartinezconfondo.png" alt="Electrónica Martínez" height="220" width="220">
                   </div>
                   
                   <div class="divider"></div>
                   
                   <div id="right">
                   
                   <b>Nombre:</b> %s<br>
                   <b>Cargo:</b> %s<br>
                   <b>Departamento:</b> %s<br>
                   <b>Email:</b> %s<br>
                   <b>Oficinas:</b> 968 165 000<br>
                   <b>Aver&iacuteas:</b> 676 966 000<br>
                   <b>P&aacute;gina web:</b> <a href="http://www.electronicamartinez.com" target="_blank">www.electronicamartinez.com</a>
                   
                   <hr>
                   
                   <a href="https://www.linkedin.com/company/10271615" target="_blank" > 
                   <img src="http://s15.postimg.org/ioxdj3dkr/Linked_In_logo_initials_1.png" alt="LinkedIn" height="45" width="45"></a>
                   
                   <a href="https://twitter.com/EMartinez_SL" target="_blank" style="margin-left:10px;"> 
                   <img src="http://s16.postimg.org/7o3u41wlx/twitter_logo.png" alt="Twitter" height="45" width="45"></a>
                   
                   <a href="https://www.facebook.com/Electrónica-Martínez-1702802093290734" target="_blank" style="margin-left:10px;"> 
                   <img src="http://s21.postimg.org/6ba5enton/fb_icon_325x325.png" alt="Facebook" height="45" width="45"></a>

		               <a href="https://plus.google.com/104958156336764853065" target="_blank" style="margin-left:10px;"> 
                   <img src="http://s27.postimg.org/8odzlznub/Google_Plus_Logo.png" alt="Google Plus" height="45" width="45"></a>	
                   </div>
                   
                   </div>
                   <hr style="position:relative;width:670px;top:20px;" align="left" id="pr">
                   '
        if(input$averias == "No") archivo <- gsub("<b>Aver&iacuteas:</b> 676 966 000<br>", "", archivo)
        if(input$empresa == "Safety 14") {
            archivo <- gsub('src="http://s3.postimg.org/r3opa07kj/logo_Emartinezconfondo.png" alt="Electrónica Martínez"', 
                            'src="http://s2.postimg.org/v6l7kda61/safety.png" alt="Safety 14"', archivo)
            archivo <- gsub('<a href="http://www.electronicamartinez.com" target="_blank">www.electronicamartinez.com</a>',
                            '<a href="http://www.safety14.com" target="_blank">www.safety14.com</a>', archivo)
            archivo <- gsub('<b>Oficinas:</b> 968 165 000<br>', '<b>Oficinas:</b> 968 554 924<br>', archivo)
            
            archivo <- gsub('https://www.linkedin.com/company/10271615', 'https://www.linkedin.com/company/safety-14', archivo)
            archivo <- gsub('https://twitter.com/EMartinez_SL', 'https://twitter.com/Safety14EM', archivo)
            archivo <- gsub('https://www.facebook.com/Electrónica-Martínez-1702802093290734', 'https://www.facebook.com/Safety-14-1038865176200618', archivo)
            archivo <- gsub('https://plus.google.com/104958156336764853065', 'https://plus.google.com/b/102573920170267038001/102573920170267038001', archivo)
        }
        else if(input$empresa == "Internet Levante") {
            archivo <- gsub('src="http://s3.postimg.org/r3opa07kj/logo_Emartinezconfondo.png" alt="Electrónica Martínez"', 
                            'src="http://s15.postimg.org/io3sb6997/Internet_Levante.png" alt="Internet Levante"', archivo)
            archivo <- gsub('<a href="http://www.electronicamartinez.com" target="_blank">www.electronicamartinez.com</a>',
                            '<a href="http://internetlevante.com/" target="_blank">www.internetlevante.com</a>', archivo)
            
            archivo <- gsub('https://www.linkedin.com/company/10271615', 'https://www.linkedin.com/company/internet-levante', archivo)
            archivo <- gsub('https://twitter.com/EMartinez_SL', 'https://twitter.com/InternetLevante', archivo)
            archivo <- gsub('https://www.facebook.com/Electrónica-Martínez-1702802093290734', 'https://www.facebook.com/Internet-Levante-1051277904891504', archivo)
            archivo <- gsub('https://plus.google.com/104958156336764853065', 'https://plus.google.com/+InternetLevanteCartagena', archivo)
        }
        
        HTML(sprintf(archivo, input$nombre, input$cargo, input$departamento, input$email))
   })
    
   observeEvent(input$submit,
                 {
                   archivo <- read_file("/home/tecnico/WebServicios/firma.html")
                   archivo <- sprintf(archivo, input$nombre, input$cargo, input$departamento, input$email)
                   if(input$averias == "No") archivo <- gsub("\t\t\t<b>Aver&iacute;as:</b> 676 966 000<br>\n", "", archivo)
                   if(input$empresa == "Safety 14") {
                     archivo <- gsub('src="http://s3.postimg.org/r3opa07kj/logo_Emartinezconfondo.png" alt="Electrónica Martínez"', 
                                     'src="http://s2.postimg.org/v6l7kda61/safety.png" alt="Safety 14"', archivo)
                     archivo <- gsub('<a href="http://www.electronicamartinez.com" target="_blank">www.electronicamartinez.com</a>',
                                     '<a href="http://www.safety14.com" target="_blank">www.safety14.com</a>', archivo)
                     archivo <- gsub('<b>Oficinas:</b> 968 165 000<br>', '<b>Oficinas:</b> 968 554 924<br>', archivo)
                     
                     archivo <- gsub('https://www.linkedin.com/company/10271615', 'https://www.linkedin.com/company/safety-14', archivo)
                     archivo <- gsub('https://twitter.com/EMartinez_SL', 'https://twitter.com/Safety14EM', archivo)
                     archivo <- gsub('https://www.facebook.com/Electrónica-Martínez-1702802093290734', 'https://www.facebook.com/Safety-14-1038865176200618', archivo)
                     archivo <- gsub('https://plus.google.com/104958156336764853065', 'https://plus.google.com/b/102573920170267038001/102573920170267038001', archivo)
                   }
                   else if(input$empresa == "Internet Levante") {
                     archivo <- gsub('src="http://s3.postimg.org/r3opa07kj/logo_Emartinezconfondo.png" alt="Electrónica Martínez"', 
                                     'src="http://s15.postimg.org/io3sb6997/Internet_Levante.png" alt="Internet Levante"', archivo)
                     archivo <- gsub('<a href="http://www.electronicamartinez.com" target="_blank">www.electronicamartinez.com</a>',
                                     '<a href="http://internetlevante.com/" target="_blank">www.internetlevante.com</a>', archivo)
                     
                     archivo <- gsub('https://www.linkedin.com/company/10271615', 'https://www.linkedin.com/company/internet-levante', archivo)
                     archivo <- gsub('https://twitter.com/EMartinez_SL', 'https://twitter.com/InternetLevante', archivo)
                     archivo <- gsub('https://www.facebook.com/Electrónica-Martínez-1702802093290734', 'https://www.facebook.com/Internet-Levante-1051277904891504', archivo)
                     archivo <- gsub('https://plus.google.com/104958156336764853065', 'https://plus.google.com/+InternetLevanteCartagena', archivo)
                   }
                   
                   write(archivo, sprintf("/home/tecnico/WebServicios/data/%s", 
                                          paste('Firma', str_split(input$nombre, " ")[[1]][1], Sys.Date(), encodeString(chartr('áéíóúñ', 'aeioun', str_replace_all(input$empresa, " ", ""))), '.html', sep='')))
                   system(sprintf("wkhtmltoimage --crop-w 700 /home/tecnico/WebServicios/data/%s /home/tecnico/WebServicios/data/%s", 
                          paste('Firma', str_split(input$nombre, " ")[[1]][1], Sys.Date(), encodeString(chartr('áéíóúñ', 'aeioun', str_replace_all(input$empresa, " ", ""))), '.html', sep=''),
                          paste('Firma', str_split(input$nombre, " ")[[1]][1], Sys.Date(), encodeString(chartr('áéíóúñ', 'aeioun', str_replace_all(input$empresa, " ", ""))), '.jpg', sep='')))
                   
                    output$DescargaFirma <- renderUI({ downloadButton("downloadData", "Descarga HTML") })
                    output$downloadData <-  downloadHandler(
                        filename = function() {
                          paste('Firma', str_split(input$nombre, " ")[[1]][1], Sys.Date(), encodeString(chartr('áéíóúñ', 'aeioun', str_replace_all(input$empresa, " ", ""))), '.html', sep='')
                        },
                        content = function(con) {
                          
                          writeBin(archivo, con)
                        }
                      )
                    
                    output$DescargaPNG <- renderUI({ downloadButton("downloadPNG", "Descarga JPG") })
                    output$downloadPNG <-  downloadHandler(
                      filename = function() {
                        paste('Firma', str_split(input$nombre, " ")[[1]][1], Sys.Date(), encodeString(chartr('áéíóúñ', 'aeioun', str_replace_all(input$empresa, " ", ""))), '.jpg', sep='')
                      },
                      content = function(con) {
                        
                        file.copy(sprintf("/home/tecnico/WebServicios/data/%s", 
                                  paste('Firma', str_split(input$nombre, " ")[[1]][1], Sys.Date(), encodeString(chartr('áéíóúñ', 'aeioun', str_replace_all(input$empresa, " ", ""))), '.jpg', sep='')), con)
                      }
                    )
                    
                    
                    
                 }
    )

   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   observe({
      if(!("mkt" %in% input$opciones)){
        if(sum(c("tracert", "ping", "nmap") %in% input$opciones) == 3)
          updateCheckboxGroupInput(session, inputId = "opciones", selected = c("tracert", "ping", "nmap"))
        else if(sum(c("tracert", "ping") %in% input$opciones) == 2)
          updateCheckboxGroupInput(session, inputId = "opciones", selected = c("tracert", "ping"))
        else if(sum(c("tracert", "nmap") %in% input$opciones) == 2)
          updateCheckboxGroupInput(session, inputId = "opciones", selected = c("tracert", "nmap"))
        else if(sum(c("ping", "nmap") %in% input$opciones) == 2)
          updateCheckboxGroupInput(session, inputId = "opciones", selected = c("ping", "nmap"))
        else if("tracert" %in% input$opciones)
          updateCheckboxGroupInput(session, inputId = "opciones", selected = "tracert")
        else if("ping" %in% input$opciones)
          updateCheckboxGroupInput(session, inputId = "opciones", selected = "ping")
        else if("nmap" %in% input$opciones)
          updateCheckboxGroupInput(session, inputId = "opciones", selected = "nmap")
        else if (sum(!(c("tracert", "ping", "nmap") %in% input$opciones)) == 3) 
          updateCheckboxGroupInput(session, inputId = "opciones", selected = "")
      }
        
   })
   observeEvent(input$searchSinEXT2,{
     if(input$numClienteSinEXT2 != "" || input$nombreClienteSinEXT2 != ""){
       shinyjs::disable("search2")
       output$errorAntena2 <- renderUI({ tags$h3("") })
       output$errorAntena3 <- renderUI({ tags$h3("") })
       output$alarma2 <- renderUI({ tags$h3("") })
       output$resultado2 <- DT::renderDataTable({NULL})
       output$resultadoAntena2 <- DT::renderDataTable({NULL})
       output$resultadoTrafico <- DT::renderDataTable({NULL})
       output$sshAntena <- DT::renderDataTable({NULL})
       output$pingUI <- renderUI({ tags$h3("") })
       output$tracerouteUI <- renderUI({ tags$h3("") })
       output$nmapUI <- renderUI({ tags$h3("") })
       output$hr <- renderUI({ tags$h3("") })
       output$descargaPDF <- renderUI({ tags$h3("") })
       output$envioMAIL <- renderUI({ tags$h3("") })
       output$buttonMAIL <- renderUI({ tags$h3("") })
       
       radius <- dbConnect(MySQL(), user="root", password="PASSWORD", db="radius", host="X.X.X.X")
       services <- dbGetQuery(radius, "SELECT srvid, srvname FROM rm_services")
       
       if(input$numClienteSinEXT2 != ""){
         user <- dbGetQuery(radius, sprintf("SELECT username, firstname, srvid, phone, mobile, address, city, state, expiration, lastlogoff,
                                            comment FROM rm_users WHERE username LIKE '%s%%'", input$numClienteSinEXT2))
       }
       else{
         user <- dbGetQuery(radius, paste0("SELECT username, firstname, srvid, phone, mobile, address, city, state, expiration, lastlogoff,
                                           comment FROM rm_users WHERE firstname LIKE '%", input$nombreClienteSinEXT2, "%'"))
       }
       
       radacct <- vector(mode = "logical")
       for(i in 1:dim(user)[1]){
         user$srvid[i] <- services$srvname[services$srvid %in% user$srvid[i]] 
         radacct[i] <- tail(dbGetQuery(radius, sprintf("SELECT acctstoptime FROM radacct WHERE username = '%s'", user$username[i])),1)
         radacct[i] <-  if(is.na(radacct[i])) TRUE else FALSE
       }
       
       dbDisconnect(radius)
       
       names(user) <- c("Cliente", "Nombre", "Servicio", "Teléfono", "Móvil", "Dirección", "Ciudad", "Región", "Expiración", "Última desconexión", "Comentario")
       
       for(i in 1:dim(user)[1]){
         if(radacct[[i]] == T){
           user$Cliente[i] <- sprintf('<span style="color:green"><b>%s</b></span>', user$Cliente[i])
         } else {
           user$Cliente[i] <- sprintf('<span style="color:red"><b>%s</b></span>', user$Cliente[i])
         }
       }
       output$resultado2 <- DT::renderDataTable(user, options = list(searchable = FALSE, pageLength = 5), escape = FALSE)
       
     }
   })
   
   
   
   
   
   
   observeEvent(input$search2,{
     if(input$numCliente2 != "" || input$nombreCliente2 != ""){
       shinyjs::disable("search2")
       output$errorAntena2 <- renderUI({ tags$h3("") })
       output$errorAntena3 <- renderUI({ tags$h3("") })
       output$alarma2 <- renderUI({ tags$h3("") })
       output$resultado2 <- DT::renderDataTable({NULL})
       output$resultadoAntena2 <- DT::renderDataTable({NULL})
       output$resultadoTrafico <- DT::renderDataTable({NULL})
       output$sshAntena <- DT::renderDataTable({NULL})
       output$pingUI <- renderUI({ tags$h3("") })
       output$tracerouteUI <- renderUI({ tags$h3("") })
       output$nmapUI <- renderUI({ tags$h3("") })
       
       output$diagnosisPDF <- renderUI({ tags$h3("") })
       
       
       radius <- dbConnect(MySQL(), user="root", password="PASSWORD", db="radius", host="x.x.x.x")
       nases <- dbGetQuery(radius, "SELECT nasname, shortname FROM nas ORDER BY shortname")
       services <- dbGetQuery(radius, "SELECT srvid, srvname FROM rm_services")

       ip <- tail(dbGetQuery(radius, sprintf("SELECT nasipaddress, framedipaddress, acctstoptime, callingstationid FROM radacct WHERE username = '%s'", input$numCliente2)),1)
       user <- dbGetQuery(radius, sprintf("SELECT firstname, staticipcpe, srvid, username, expiration, createdon, phone, mobile, address, city, comment FROM rm_users WHERE username = '%s'", input$numCliente2))
       connections <- dbGetQuery(radius, sprintf("SELECT nasipaddress, framedipaddress, acctstarttime, acctstoptime, callingstationid, acctoutputoctets, acctinputoctets
                                                   FROM radacct WHERE username = '%s' ORDER BY radacctid DESC", input$numCliente2))
  
       dbDisconnect(radius)
       
       if(dim(user)[1] == 0) {
         output$alarma2 <- renderUI({ tags$h3("No existe este cliente PPPoE") })
         
       } else {
       
         tabla <- data.frame(matrix(ncol=9, nrow=1))
         names(tabla) <- c("Cliente", "Nombre", "Servicio", "Expiración PPPoE", "Fecha creación PPPoE", "IP", "Tipo IP", "MAC CPE", "NAS")
         tabla$Cliente <- user$username
         tabla$Nombre <- user$firstname
         tabla$Servicio <- services$srvname[services$srvid %in% user$srvid]
         tabla$`Expiración PPPoE` <- user$expiration
         tabla$`Fecha creación PPPoE` <- user$createdon
         tabla$Teléfono <- user$phone
         tabla$Móvil <- user$mobile
         tabla$Dirección <- user$address
         tabla$Ciudad <- user$city
         tabla$Comentario <- user$comment
         
         if(dim(ip)[1] == 0) {
           output$alarma2 <- renderUI({ tags$h3("Este cliente no ha usado nunca su cliente PPPoE") })
           output$resultado2 <- DT::renderDataTable(tabla,  options = list(searchable = FALSE, paging = FALSE), escape = FALSE)
           
         } else {
           
           if(is.na(ip$acctstoptime)){
             tabla$Cliente <- sprintf('<span style="color:green"><b>%s</b></span>', tabla$Cliente)
           } else {
             tabla$Cliente <- sprintf('<span style="color:red"><b>%s</b></span>', tabla$Cliente)
           }
           
           tabla$IP <- sprintf('<a href="http://%s:8080" target="_blank">%s</a>', ip$framedipaddress, ip$framedipaddress)
           tabla$`Tipo IP` <- if(user$staticipcpe == "") "Dinámica" else "Estática"
           tabla$`MAC CPE` <- ip$callingstationid
           tabla$NAS <- nases$shortname[nases$nasname %in% ip$nasipaddress]
           tabla$NAS <- sprintf('<a href="http://%s" target="_blank">%s</a>',tail(strsplit(tabla$NAS, " ")[[1]],1), tabla$NAS)
           
           output$resultado2 <- DT::renderDataTable(tabla, options = list(searching = FALSE, paging = FALSE), escape = FALSE)
           
           
           if("mkt" %in% input$opciones){
             MikroTik <- system(ignore.stdout = F, ignore.stderr = T, command = sprintf('python3 /home/tecnico/WebServicios/GetAntena.py %s', nases$nasname[nases$naname %in% ip$nasipaddress]), intern = T)
             posicion <- grep(sprintf(">>>=host-name=%s", paste0(str_replace(user$username, "t", ""), str_split(str_split(user$firstname, " ")[[1]][1], "")[[1]][1])), str_replace_all(MikroTik, fixed(" "), ""))
             
             if(length(posicion) == 0) {
               output$errorAntena2 <- renderUI({ tags$h3("No se encuentra la antena del cliente. Puede ser debido a: 
                                                         \n 1. Nombre de antena incorrecto \n 2. Antena configurada como router \n 3. Antena sin conectar por +3 días \n 4. Cliente de ADSL") })
               
             } else {
               
               tablaAntena <- data.frame(matrix(ncol=4, nrow=1))
               names(tablaAntena) <- c("IP Antena", "Expira en", "MAC Antena", "Nombre Antena")
               
               for(i in 1:length(posicion)){
                 if(i >= 2){
                   tablaAntena[i,] <- NA
                 }
                 
                 ipAntena <- str_split(MikroTik[posicion[i]-4], "=")[[1]][3]
                 tablaAntena$`IP Antena`[i] <- sprintf('<a href="http://%s" target="_blank">%s</a>', ipAntena, ipAntena)
                 tablaAntena$`Expira en`[i] <- str_split(MikroTik[posicion[i]-6], "=")[[1]][3]
                 tablaAntena$`MAC Antena`[i] <- str_split(MikroTik[posicion[i]-3], "=")[[1]][3]
                 tablaAntena$`Nombre Antena`[i] <- str_split(MikroTik[posicion[i]], "=")[[1]][3]
               }
               
               
               output$resultadoAntena2 <- DT::renderDataTable(tablaAntena, options = list(searching = FALSE, paging = FALSE), escape = FALSE)
               
               
               
             
              
               # nav$getCurrentUrl()
               # “xpath”, “css selector”, “id”, “name”, “tag name”, 
               # “class name”, “link text”, “partial link text”
               # nav$getPageSource()
               # nav$screenshot(display = TRUE)
               
               ###########################
               ### Llamada a la antena ###
               ###########################
               
              if("ssh" %in% input$opciones){

                tryCatch({
                  python.exec(python.code = c("import paramiko", 
                                               "ssh = paramiko.SSHClient()",
                                               "ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())",
                                               sprintf('ssh.connect("%s", username="admin", password="PASSWORD") ', ipAntena), 
                                               'stdin, stdout, stderr = ssh.exec_command("mca-status")',
                                               'stats = stdout.readlines()'))
                   
                  statsAntena <- gsub(x = python.get("stats"), pattern = "\r\n", replacement = "")
                  statsAntenaSplit <- strsplit(statsAntena, "=")
                
                  SSH <- data.frame(matrix(nrow = 1, ncol = 0))
                   
                  primeraLinea <- strsplit(statsAntena[[1]][1], ",")
                  SSH$Firmware <- strsplit(primeraLinea[[1]][grep("firmwareVersion", primeraLinea[[1]])], "=")[[1]][2]
                  SSH$Hardware <- strsplit(primeraLinea[[1]][grep("platform", primeraLinea[[1]])], "=")[[1]][2]
                   
                   
                  Sectorial <- strsplit(ipAntena, "\\.")
                  Sectorial <- paste(Sectorial[[1]][1], Sectorial[[1]][2], Sectorial[[1]][3], "10", sep = ".", collapse = "")
                  SSH$`Conectado a` <- sprintf("<a href='http://%s' target='_blank'>%s</a>", Sectorial, statsAntenaSplit[[grep("essid", statsAntenaSplit)]][2])
                   
                   
                  SSH$`WlanUptime / Uptime` <- paste(round(as.numeric(statsAntenaSplit[[grep("wlanUptime", statsAntenaSplit)]][2])/60/60/24, 2), "/", 
                                                                 round(as.numeric(statsAntenaSplit[[grep("uptime", statsAntenaSplit)]][2])/60/60/24,2), "días")
                  SSH$`Frecuencia` <- paste(statsAntenaSplit[[grep("freq", statsAntenaSplit)]][2], "MHz")
                  SSH$SNR <- paste(statsAntenaSplit[[grep("signal", statsAntenaSplit)]][2], statsAntenaSplit[[grep("noise", statsAntenaSplit)]][2], sep = "/")
                  SSH$CCQ <- paste(substr(statsAntenaSplit[[grep("ccq", statsAntenaSplit)]][2], 0,2), substr(statsAntenaSplit[[grep("ccq", statsAntenaSplit)]][2], 2,3), sep=",")
                  SSH$Distancia <- paste(statsAntenaSplit[[grep("distance", statsAntenaSplit)]][2], "metros")
                  SSH$`TX-RX` <- paste(statsAntenaSplit[[grep("wlanTxRate", statsAntenaSplit)]][2], statsAntenaSplit[[grep("wlanRxRate", statsAntenaSplit)]][2], sep = "/")
                  SSH$Latencia <- paste(statsAntenaSplit[[grep("wlanTxLatency", statsAntenaSplit)]][2], "ms")
                  SSH$`Lan Speed` <- statsAntenaSplit[[grep("lanSpeed", statsAntenaSplit)]][2]
                    
  
                  output$sshAntena <- DT::renderDataTable(SSH, options = list(searching = FALSE, paging = FALSE), escape = FALSE)
                }, error=function(e){
                  output$errorAntena3 <- renderUI({ tags$h3("Error al conectar a la antena") })
                  })
              }
               
               
               
  #               puerto <- sample(4450:65000, 1)
  #               pJS <- phantom(extras = "--ignore-ssl-errors=true --ssl-protocol=tlsv1", port = puerto)
  #               Sys.sleep(2)
  #               nav <- remoteDriver(remoteServerAddr = "localhost", port = puerto, browserName = "phantomjs")
  #               
  #               nav$open()
  #               nav$navigate(sprintf("http://%s/", tabla$`IP Antena`[1])) #10.4.18.24
  #               Sys.sleep(2)
  #                 
  #               print("1")
  #               user <- nav$findElement("id", value = "username")
  #               print("1")
  #               user$sendKeysToElement(list("admin"))
  #               print("1")
  #               password <- nav$findElement("id", value = "password")
  #               print("1")
  #               password$sendKeysToElement(list("PASSWORD"))
  #               print("1")
  #               enter <- nav$findElement(using = "id", value = "loginform")
  #               print("1")
  #               enter$submitElement()
  #               print("1")
  #               foto <- nav$screenshot()
  #               print("1")
  #               pJS$stop()
  #               print("1")
  #               output$fotoAntena <- renderImage(foto)
  
               
               
  #               pagina <- nav$getPageSource()
  #               pagina <- str_split(pagina, "\n")
  #               
  #               versionN <- grep("Versión:", pagina[[1]])
  #               versionN <- str_extract(pagina[[1]][versionN+1], "v5.{9}")
  #               
  #               Tactivo <- grep("Tiempo activo:", pagina[[1]])
  #               Tactivo <- str_extract(pagina[[1]][Tactivo+1], "v5.{9}")
                
                #x = scp(tabla$`IP Antena`[1], "/home/dir/file.txt", "PASSWORD", user="admin")
                #system("sshpass ") ##########MCA-STATUS#
               
             }
           }
         }
         
         
         
         
         names(connections) <- c("IP NAS", "IP CPE", "Start", "Stop", "MAC CPE", "Download", "Upload")
         for(i in 1:dim(connections)[1]){
           if(as.numeric(connections$Download[i]) < 1024){
             connections$Download[i] <- paste(as.character(connections$Download[i]), "B")
           } 
           else if(as.numeric(connections$Download[i]) >= 1024 && as.numeric(connections$Download[i]) < 1024*1024){
             connections$Download[i] <- paste(as.character(round(as.numeric(connections$Download[i])/1024, digits = 2)), "KB")
           } 
           else if (as.numeric(connections$Download[i]) >= 1024*1024 && as.numeric(connections$Download[i]) < 1024*1024*1024){
             connections$Download[i] <- paste(as.character(round(as.numeric(connections$Download[i])/(1024*1024), digits = 2)), "MB")
           } 
           else if (as.numeric(connections$Download[i]) >= 1024*1024*1024 && as.numeric(connections$Download[i]) < 1024*1024*1024*1024){
             connections$Download[i] <- paste(as.character(round(as.numeric(connections$Download[i])/(1024*1024*1024), digits = 2)), "GB")
           }
           else if (as.numeric(connections$Download[i]) >= 1024*1024*1024*1024 && as.numeric(connections$Download[i]) < 1024*1024*1024*1024*1024){
             connections$Download[i] <- paste(as.character(round(as.numeric(connections$Download[i])/(1024*1024*1024*1024), digits = 2)), "TB")
           }
         }
         for(i in 1:dim(connections)[1]){
           if(as.numeric(connections$Upload[i]) < 1024){
             connections$Upload[i] <- paste(as.character(connections$Upload[i]), "B")
           } 
           else if(as.numeric(connections$Upload[i]) >= 1024 && as.numeric(connections$Upload[i]) < 1024*1024){
             connections$Upload[i] <- paste(as.character(round(as.numeric(connections$Upload[i])/1024, digits = 2)), "KB")
           } 
           else if (as.numeric(connections$Upload[i]) >= 1024*1024 && as.numeric(connections$Upload[i]) < 1024*1024*1024){
             connections$Upload[i] <- paste(as.character(round(as.numeric(connections$Upload[i])/(1024*1024), digits = 2)), "MB")
           } 
           else if (as.numeric(connections$Upload[i]) >= 1024*1024*1024 && as.numeric(connections$Upload[i]) < 1024*1024*1024*1024){
             connections$Upload[i] <- paste(as.character(round(as.numeric(connections$Upload[i])/(1024*1024*1024), digits = 2)), "GB")
           }
           else if (as.numeric(connections$Upload[i]) >= 1024*1024*1024*1024 && as.numeric(connections$Upload[i]) < 1024*1024*1024*1024*1024){
             connections$Upload[i] <- paste(as.character(round(as.numeric(connections$Upload[i])/(1024*1024*1024*1024), digits = 2)), "TB")
           }
         }
         
         
         connections$Start <- str_replace_all(connections$Start, "-", "/")
         connections$Stop <- str_replace_all(connections$Stop, "-", "/")
         
         was.na <- FALSE
         if(is.na(connections$Stop[1])) {
           was.na <- TRUE
           connections$Stop[1] <-  format(Sys.time(), "%Y/%m/%d %H:%M:%S") ## Para los cálculos, hacemos que stoptime sea ahora, luego lo ponemos a NA de nuevo
         }
         
         connections$TRY <- difftime(as.POSIXct(connections$Stop), as.POSIXct(connections$Start), units = "secs")
         connections$`Online Time` <- NA
         
         for(i in 1:dim(connections)[1]){
           if(as.numeric(connections$TRY[i]) < 60){
             connections$`Online Time`[i] <- paste(round(difftime(as.POSIXct(connections$Stop[i]), as.POSIXct(connections$Start[i]), units = "secs"), 2), "secs")
           } 
           else if(as.numeric(connections$TRY[i]) >= 60 && as.numeric(connections$TRY[i]) < 60*60){
             connections$`Online Time`[i] <- paste(round(difftime(as.POSIXct(connections$Stop[i]), as.POSIXct(connections$Start[i]), units = "mins"), 2), "mins")
           } 
           else if(as.numeric(connections$TRY[i]) >= 60*60 && as.numeric(connections$TRY[i]) < 60*60*24){
             connections$`Online Time`[i] <- paste(round(difftime(as.POSIXct(connections$Stop[i]), as.POSIXct(connections$Start[i]), units = "hours"), 2), "hours")
           } 
           else if(as.numeric(connections$TRY[i]) >= 60*60*24){
             connections$`Online Time`[i] <- paste(round(difftime(as.POSIXct(connections$Stop[i]), as.POSIXct(connections$Start[i]), units = "days"), 2), "days")
           } 
         }
         
         connections <- connections[,-8]
         connections <- connections[,c(1:4,8,5:7)]
         
         if(was.na) connections$Stop[1] <- NA
         
         output$resultadoTrafico <- DT::renderDataTable(connections, options = list(searching = FALSE, pageLength = 5))
         
         if("ping" %in% input$opciones){
           output$pingUI <- renderUI({ verbatimTextOutput("ping") })
           output$ping <- renderText({   paste(system(sprintf("ping -c 4 %s", ip$framedipaddress), intern = T), collapse = "\n")   })
         }
         
         if("tracert" %in% input$opciones){
           output$tracerouteUI <- renderUI({ verbatimTextOutput("traceroute") })
           output$traceroute <- renderText({   paste(system(sprintf("traceroute %s", ip$framedipaddress), intern = T), collapse = "\n")   })
         }
         
         if("nmap" %in% input$opciones){
           output$nmapUI <- renderUI({ verbatimTextOutput("nmap") })
           output$nmap <- renderText({   paste(system(sprintf("nmap -T4 -A -v %s", ip$framedipaddress), intern = T), collapse = "\n")   })
         }
         
       }
      
       ###############################################
       ############# EMAIL ENVIO KNITR ###############
       ###############################################
       
       #output$descargaPDF <- renderUI()
       #output$hr <- renderUI(hr())
       output$diagnosisPDF <- renderUI({
         downloadButton("downloadPDF", "Descarga PDF de Diagnosis")
       })
       
       output$downloadPDF <- downloadHandler(
           filename = function() {
             paste('Diagnosis', tabla$Nombre, Sys.Date(), '.pdf', sep='')
           },
           content = function(file) {
             setwd("/home/tecnico/WebServicios/RMD/")
             knit("diagnosis.rmd")
             system("pandoc -s diagnosis.md --latex-engine=xelatex -o Diagnosis.pdf")
             file.copy("Diagnosis.pdf", file)
           }
        )
       
       shinyjs::enable("search")
     }
   })
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   #observe({
     
     
     output$dslamUI <- renderUI({
       infoDSLAMs <- read.csv("/home/tecnico/WebServicios/data/infoDSLAMs.csv", stringsAsFactors = F)
       choices <- setNames(infoDSLAMs$IP, paste(infoDSLAMs$Ubicacion, infoDSLAMs$IP))
       selectInput("dslam", "Escoge DSLAM", choices, selected = "", selectize = T)
     })
     
     
     output$boardUI <- renderUI({
       if(!is.na(input$dslam)){
         infoDSLAMs <- read.csv("/home/tecnico/WebServicios/data/infoDSLAMs.csv", stringsAsFactors = F)
         poss <- infoDSLAMs$IP == input$dslam
         board <- which(cbind(infoDSLAMs$Board1[poss], infoDSLAMs$Board2[poss], infoDSLAMs$Board3[poss], infoDSLAMs$Board4[poss]) == 1)
         
         selectInput("board", "Escoge board:", board)
       }
     })
     
     
     
     
     observeEvent(input$connectSSH, {
       if(!is.na(input$board)){
         python.exec(sprintf("
import subprocess
import sys
import pexpect
                             
try:
  try:
    child = pexpect.spawn('ssh -oStrictHostKeyChecking=no tecnico@%s')
    fout = file('/home/tecnico/WebServicios/data/logSSHsession.txt','w')
    child.logfile = fout
    child.timeout = 4
    child.expect('password:')
  except pexpect.TIMEOUT:
    raise error('Couldnt log on to the DSLAM')
                             
  child.sendline('PASSWORD')
  child.expect('>')
  child.sendline('enable')
  child.expect('#')
  child.sendline('display vdsl line operation board 0/%s')
  child.expect('{ <cr>||<K> }:')
  child.sendline('')
  child.expect(' ----')
  child.sendline('') #15 Enters es lo maximo si todas las lineas estan ocupadas
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
                             
  child.expect('#')
  child.sendline('display port desc 0/%s')
  child.expect('{ <cr>||<K> }:')
  child.sendline('')
  child.expect(' ----')
  child.sendline('') #15 Enters es lo maximo si todas las lineas estan ocupadas
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
  child.sendline('')
                             
  child.expect('#')
  child.sendline('quit')
  child.expect(':')
  child.sendline('y')
except (pexpect.EOF, pexpect.TIMEOUT), e:
  print('Error while trying to extract info from DSLAM')
                             
                             ", input$dslam, input$board, input$board))
         
         library(readr)
         outputVDSL <- read_lines("/home/tecnico/WebServicios/data/logSSHsession.txt")
         outputVDSL <- gsub("---- More \\( Press 'Q' to break \\) ----\033\\[37D                                     \033\\[37D", "", outputVDSL)
         response6 <- gsub(x = outputVDSL, pattern = "      ", replacement = "\t")
         response5 <- gsub(x = response6, pattern = "     ", replacement = "\t")
         response4 <- gsub(x = response5, pattern = "    ", replacement = "\t")
         response3 <- gsub(x = response4, pattern = "   ", replacement = "\t")
         response2 <- gsub(x = response3, pattern = "  ", replacement = "\t")
         responseFINAL <- strsplit(response2, "\t")
         
         dataframe <- as.data.frame(matrix(nrow = 0, ncol = 12))
         colnames(dataframe) <- c("XDSL Port", "UpSNR Margin", "DwSNR Margin", "UpSig Atten", "DwSig Atten", "MaxUp Rate",
                                  "MaxDw Rate", "UpOut Power", "DwOut Power", "UpAct Rate", "DwAct Rate", "Port Desc")
         
         for(i in 48:length(responseFINAL)){
           if(responseFINAL[[i]][2] == "------------------------------------------------------------------------------") {
             break
           } else {
             for(j in 2:(length(dataframe)[1]+1)){
               dataframe[i-47,j-1] <- responseFINAL[[i]][j]
             }
           }
         }
         
         posicionesBLANK <- which(!(0:31 %in% dataframe$`XDSL Port`) == T)
         
         dataframeNEW <- as.data.frame(matrix(nrow = 32, ncol = 12))
         colnames(dataframeNEW) <- c("XDSL Port", "UpSNR Margin", "DwSNR Margin", "UpSig Atten", "DwSig Atten", "MaxUp Rate",
                                     "MaxDw Rate", "UpOut Power", "DwOut Power", "UpAct Rate", "DwAct Rate", "Port Desc")
         j <- 1
         for(i in 1:32){
           if(!(i %in% posicionesBLANK)){
             dataframeNEW[i,] <- dataframe[j,]
             j <- j+1
           } else {
             dataframeNEW$`XDSL Port`[i] <- i-1
           }
         }
         
         rownames(dataframeNEW) <- 0:31
         
         dataframe <- dataframeNEW
         
         posicionTAGs <- grep("Port Description", outputVDSL)+2
         
         for(i in posicionTAGs:length(responseFINAL)){
           if(responseFINAL[[i]][2] == "------------------------------------------------------------") {
             break
           } else {
             if(!is.na(responseFINAL[[i]][5])){
               if(!(responseFINAL[[i]][5] == ""))
                 dataframe$`Port Desc`[i-posicionTAGs+1] <- responseFINAL[[i]][5]
             }
           }
         }
         
         output$sshVDSL <- DT::renderDataTable({ dataframe[,-1] }, options = list(searching = FALSE, pageLength = 16))
         
       }
     
     
     shinyjs::enable("connectSSH")
  })
     
     
     

   
     
     
     
       
       
       
   observeEvent(input$buttonTAG, {

      python.exec(sprintf("
import subprocess
import sys
import pexpect
                         
try:
  try:
    child = pexpect.spawn('ssh -oStrictHostKeyChecking=no tecnico@%s')
    fout = file('/home/tecnico/WebServicios/data/logSSHanotacion.txt','w')
    child.logfile = fout
    child.timeout = 4
    child.expect('password:')
  except pexpect.TIMEOUT:
    raise error('Couldnt log on to the DSLAM')
                         
  child.sendline('PASSWORD')
  child.expect('>')
  child.sendline('enable')
  child.expect('#')
  child.sendline('config')
  child.expect('#')

  child.sendline('port desc 0/%s/%s description %s')

  child.expect('#')
  child.sendline('quit')
  child.expect('#')
  child.sendline('quit')
  child.expect(':')
  child.sendline('y')
except (pexpect.EOF, pexpect.TIMEOUT), e:
  error('Error while trying to extract info from DSLAM')
  raise
", input$dslam, input$board, input$puertoTAG, input$textTAG))

output$ok <- renderUI({ h5("Correcto!") })
 
   })
   
   
  }
)