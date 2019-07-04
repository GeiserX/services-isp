# shiny::runApp(appDir = "/home/tecnico/WebServicios", launch.browser = FALSE, port = 8080, host = "0.0.0.0")

library(shiny)
library(shinyjs)

shinyUI(
  navbarPage("Servicios EM", id="navbar", position="static-top", inverse=F, theme = "bootstrap.css",
             
             tabPanel("Datos CPE", value=1,
                      
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"),
                      tags$head(tags$script(src="enterKEY.js")),
                  
                      # headerPanel("Datos sobre Antena, MAC CPE por usuario"),
                      
                      sidebarPanel( width = 2,
                                    h4("Si no sabes la extensión:"),
                                    textInput("numClienteSinEXT", "Introduzca número de cliente:"),
                                    textInput("nombreClienteSinEXT", "O introduzca el nombre del cliente:"),
                                    actionButton("searchSinEXT", "Buscar usuarios", icon("search", lib = "font-awesome")),
                                    hr(),
                                    h4("Si sabes la extensión:"),
                                    textInput("numCliente", "Introduzca número de cliente:"),
                                    #textInput("nombreCliente", "O introduzca el nombre del cliente:"),
                                    actionButton("search", "Buscar", icon("search", lib = "font-awesome"))
                                  ),
                      mainPanel(width = 4,
                                DT::dataTableOutput("resultado"),
                                DT::dataTableOutput("resultadoAntena"),
                                DT::dataTableOutput("resultadoTraficoDATOS"),
                                uiOutput("alarma"),
                                uiOutput("errorAntena")
                                )
                                    
                      ),
             
             tabPanel("Alarmas GeCo", value=2,
                      
                      headerPanel("Facturas que exceden el 50% de su tarifa / O que excedan 5€ sin tarifa"),
                      sidebarPanel( width = 2,
                                    selectInput("selectGECO", "Escoja Tipo de Facturación", choices = c("Móvil", "Fija"), selected = "Móvil", multiple = F)
                      ),
                      mainPanel(
                        DT::dataTableOutput('listaMoviles')
                      )
                      
             ),
             
              tabPanel("Firmas", value=3, 
                        headerPanel("Creación de firmas para correo electrónico"),
                        sidebarPanel( width = 5,
                            radioButtons("empresa", "Elija empresa:",
                                         choices = c("Electrónica Martínez", "Safety 14", "Internet Levante"),
                                         selected = "Electrónica Martínez", inline = T),
                            textInput("nombre", "Nombre:"),
                            textInput("cargo", "Cargo:"),
                            textInput("departamento", "Departamento:"),
                            textInput("email", "Correo electrónico:"),
                            radioButtons("averias", "¿Debe de aparecer el teléfono de averías?", choices=c("Sí", "No"), selected = "Sí", inline = T),
                            actionButton("submit", "Crear", icon("pencil", lib = "font-awesome"))
                        ),
                            
                        mainPanel(width = 4,
                            h3("Previsualización: "),
                            uiOutput("firma"),
                            br(),
                            fluidRow(
                              column(5, uiOutput("DescargaFirma")),
                              column(5, uiOutput("DescargaPNG"))
                            )
                            
                        )
              ),
             
              tabPanel("Diagnosis", value=4, 
                      # headerPanel("Diagnosticador de conexión de Internet para clientes"),
                      sidebarPanel( width = 2, position = "right",
                                    h4("Si no sabes la extensión:"),
                                    textInput("numClienteSinEXT2", "Introduzca número de cliente:"),
                                    textInput("nombreClienteSinEXT2", "O introduzca el nombre del cliente:"),
                                    actionButton("searchSinEXT2", "Buscar PPPoE's", icon("search", lib = "font-awesome")),
                                    hr(),
                                    h4("Si sabes la extensión:"),
                                    textInput("numCliente2", "Introduzca número de cliente:"),
                                    #textInput("nombreCliente2", "O introduzca el nombre del cliente:"),
                                    actionButton("search2", "Buscar", icon("search", lib = "font-awesome")),
                                    
                                    checkboxGroupInput(inputId = "opciones", h4("Opciones:"), list("Conexión a MikroTik" = "mkt",
                                                      "Conexión a Antena" = "ssh", "Ping" = "ping", "Traceroute" = "tracert", "NMap" = "nmap"), 
                                                      selected = c("mkt", "ssh"))
                      ),
                      mainPanel(width = 4,
                                #shiny::flowLayout(
                                DT::dataTableOutput("resultado2"),
                                DT::dataTableOutput("resultadoAntena2"),
                                DT::dataTableOutput("sshAntena"),
                                uiOutput("errorAntena3"),
                                DT::dataTableOutput("resultadoTrafico"),
                                uiOutput("pingUI"),
                                uiOutput("tracerouteUI"),
                                uiOutput("nmapUI"),
                                
                                tags$head(tags$style(type="text/css", "#traceroute {width: 1100px}
                                                     #ping {width: 1100px}
                                                     #nmap {width: 1100px}")),
                                #uiOutput("hr"),
#                                 fluidRow(
#                                   column(width=8, uiOutput("textMAIL") ),
#                                   column(width=4, br(), uiOutput("buttonMAIL") )
#                                 ),
                                uiOutput("diagnosisPDF"),
                                uiOutput("alarma2"),
                                uiOutput("errorAntena2")
                                
                                
                      )
                      
              
             ),
             tabPanel("VDSL", value=5, 
                      
                      sidebarPanel( width = 2,
                                 
                                 uiOutput("dslamUI"),
                                 uiOutput("boardUI"),
                                 actionButton("connectSSH", "Conectar", icon("arrows-h", lib = "font-awesome")),
                                 
                                 hr(),
                                 h4("Añadir anotaciones: "),
                                 selectInput("puertoTAG", "Escoge puerto:", 0:31),
                                 textInput("textTAG", "Anotación:"),
                                 actionButton("buttonTAG", "Marcar", icon("thumb-tack", lib = "font-awesome")),
                                 uiOutput("ok")
                                 
                      ),
                      
                      mainPanel( width= 10, 
                                 DT::dataTableOutput("sshVDSL"),
                                 DT::dataTableOutput("sshVDSL2")
                               )
                      
             )
                      
                      
                      
  )
)