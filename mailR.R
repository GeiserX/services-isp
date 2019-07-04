library("mailR")
send.mail(from = "my-email@gmail.com", 
          to = c("another-email@gmail.com"), 
          subject = "Resumen Alarmas GeCo",
          body = "Envío adjunto la lista de clientes ION Móvil con alto consumo en formato CSV",
          smtp = list(host.name = "smtp.gmail.com", 
                      port = 25,
                      user.name = "my-email@gmail.com",
                      passwd = "PASSWORD", 
                      ssl = TRUE), 
          attach.files = c("/home/tecnico/WebServicios/data/listaEXCEED.csv", "/home/tecnico/WebServicios/data/listaEXCEEDfijo.csv"),
          authenticate = T,
          debug = T
)