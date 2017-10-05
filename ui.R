library(shiny)
library(shinythemes)

shinyUI(navbarPage("Calculadora epidemiológica",theme = shinytheme("united"),
###################################################################################################                   
tabPanel("Riesgo relativo",
        
         tabsetPanel(type = "tabs",
                     tabPanel("2 niveles de exposición",                 
                              wellPanel(
                              fluidPage(                              
                              fluidRow(
                              h4(div("Completar la tabla",style="color:grey")),                             
                              
                     column(3,(numericInput("cexp",label="Enfermos expuestos",value = 690)),
                     (numericInput("cnoexp",label="Enfermos no expuestos",value = 1000))),
                     
                     column(3,(numericInput("ncexp",label=" Sanos expuestos",value = 10263)),
                     (numericInput("nocnoexp",label=" Sanos no expuestos",value = 24008))),
                     
                     
                     column(6,strong(h4(textOutput(("t1"))))),
                     column(6,(tableOutput("r1")))
                     
                              )))),
                    
                   
                   tabPanel("3 niveles de exposición",
                            wellPanel(
                            fluidPage(                              
                            fluidRow( 
                                h4(div("Completar la tabla",style="color:grey")),                             
                                
                                column(3,(numericInput("cexpa",label="Enfermos expuestos nivel I",value = 9)),
                                (numericInput("cexp2",label="Enfermos expuestos nivel II",value = 11)),
                                (numericInput("cnoexpa",label="Enfermos no expuestos",value = 10))),
                                column(3,(numericInput("ncexpa",label=" Sanos expuestos nivel I",value = 40)),
                                (numericInput("ncexp2",label=" Sanos expuestos nivel II",value = 95)),
                                (numericInput("nocnoexpa",label=" Sanos no expuestos",value = 135))),
                                column(5,strong(textOutput(("t2"))),
                                       (tableOutput("r2"))),
                                column(5,offset=6,strong(textOutput(("t2b"))),
                                       (tableOutput("r2b")))
                                
                                )))),
                              
                   tabPanel("4 niveles de exposición",
                            wellPanel(
                            fluidPage(                              
                            fluidRow( 
                                h4(div("Completar la tabla",style="color:grey")),                             
                                
                                column(3,(numericInput("cexpb",label="Enfermos expuestos nivel I",value = 10)),
                                (numericInput("cexp2b",label="Enfermos expuestos nivel II",value = 10)),
                                (numericInput("cexp3b",label="Enfermos expuestos nivel III",value = 10)),
                                (numericInput("cnoexpb",label="Enfermos no expuestos",value = 10))),
                                
                                column(3,(numericInput("ncexpb",label=" Sanos expuestos nivel I",value = 30)),
                                (numericInput("ncexp2b",label=" Sanos expuestos nivel II",value = 40)),
                                (numericInput("ncexp3b",label=" Sanos expuestos nivel III",value = 60)),
                                (numericInput("nocnoexpb",label=" Sanos no expuestos",value = 100))),
                                column(6,strong(textOutput(("t3"))),
                                       (tableOutput("r3"))),
                                column(6,strong(textOutput(("t3b"))),
                                       (tableOutput("r3b"))),
                                column(6,offset = 6, strong(textOutput(("t3c"))),
                                       (tableOutput("r3c")))
                                )))),
                              
                                                 
                   tabPanel("5 niveles de exposición",
                            wellPanel(
                            fluidPage(                              
                            fluidRow( 
                                h4(div("Completar la tabla",style="color:grey")),                             
                                
                                column(3,(numericInput("cexpc",label="Enfermos expuestos nivel I",value = 10)),
                                (numericInput("cexp2c",label="Enfermos expuestos nivel II",value = 10)),
                                (numericInput("cexp3c",label="Enfermos expuestos nivel III",value = 10)),
                                (numericInput("cexp4c",label="Enfermos expuestos nivel IV",value = 10)),
                                (numericInput("cnoexpc",label="Enfermos no expuestos",value = 10))),
                                column(3,(numericInput("ncexpc",label=" Sanos expuestos nivel I",value = 15)),
                                (numericInput("ncexp2c",label=" Sanos expuestos nivel II",value = 31)),
                                (numericInput("ncexp3c",label=" Sanos expuestos nivel III",value = 51)),
                                (numericInput("ncexp4c",label=" Sanos expuestos nivel IV",value = 71)),
                                (numericInput("nocnoexpc",label=" Sanos no expuestos",value = 101))),
                                
                                column(6,strong(textOutput(("t4"))),
                                       (tableOutput("r4"))),
                                column(6,strong(textOutput(("t4a"))),
                                       (tableOutput("r4a"))),
                                column(6,offset = 6,strong(textOutput(("t4b"))),
                                       (tableOutput("r4b"))),
                                column(6,offset = 6,strong(textOutput(("t4c"))),
                                       (tableOutput("r4c")))
                                
                                ))))                 
                              
                              
)
),
######################################################################################################
tabPanel("Odds ratio",
         tabsetPanel(type = "tabs",
                     tabPanel("2 niveles de exposición",
                              wellPanel(
                              fluidPage(                              
                              fluidRow(
                                    h4(div("Completar la tabla:",style="color:grey")),                             
                                    
                                    column(3,(numericInput("casoexp",label="Casos expuestos",value = 24)),
                                           (numericInput("casonoexp",label="Casos no expuestos",value = 36))),
                                    
                                    column(3,(numericInput("nocasoexp",label=" Controles expuestos",value = 58)),
                                           (numericInput("nocasonoexp",label=" Controles  no expuestos",value = 40))),
                                    
                                    
                                    column(6,strong(textOutput(("texto1")))),
                                    column(6,(tableOutput("resultado")))
                                    
                                  )))),
                     
                     tabPanel("3 niveles de exposición",
                              wellPanel(
                                fluidPage(                              
                                  fluidRow( 
                                    h4(div("Completar la tabla:",style="color:grey")),                             
                                    
                                    column(3,(numericInput("casoexpa",label="Casos expuesros nivel I",value = 9)),
                                           (numericInput("casoexp2",label="Casos expuestos nivel II",value = 15)),
                                           (numericInput("casonoexpa",label="Casos no expuestos",value = 24))),
                                    column(3,(numericInput("nocasoexpa",label="Controles expuestos nivel I",value = 58)),
                                           (numericInput("nocasoexp2",label="Controles expuestos nivel II",value = 8)),
                                           (numericInput("nocasonoexpa",label=" Controles  no expuestos",value = 50))),
                                    
                                    column(6,strong(textOutput(("texto2"))),
                                           (tableOutput("resultado2"))),
                                    column(6,strong(textOutput(("texto2b"))),
                                           (tableOutput("resultado2b")))
                                    
                                  )))),
                     tabPanel("4 niveles de exposición",
                              wellPanel(
                                fluidPage(                              
                                  fluidRow( 
                                    h4(div("Completar la tabla:",style="color:grey")),                            
                                    
                                    column(3,(numericInput("casoexpb",label="Casos expuestos nivel I",value = 10)),
                                           (numericInput("casoexp2b",label="Casos expuestos nivel II",value = 10)),
                                           (numericInput("casoexp3b",label="Casos expuestos nivel III",value = 10)),
                                           (numericInput("casonoexpb",label="Casos no expuestos",value = 10))),
                                    
                                    column(3,(numericInput("nocasoexpb",label="Controles expuestos nivel I",value = 31)),
                                           (numericInput("nocasoexp2b",label="Controles expuestos nivel II",value = 61)),
                                           (numericInput("nocasoexp3b",label="Controles expuestos nivel III",value = 91)),
                                           (numericInput("nocasonoexpb",label=" Controles  no expuestos",value = 211))),
                                    
                                    column(6,strong(textOutput(("texto3"))),
                                           (tableOutput("resultado3"))),
                                    column(6,strong(textOutput(("texto3b"))),
                                           (tableOutput("resultado3b"))),
                                    column(6,offset=6,strong(textOutput(("texto3c"))),
                                           (tableOutput("resultado3c")))
                                  )))),
                     tabPanel("5 niveles de exposición",
                              wellPanel(
                                fluidPage(                              
                                  fluidRow( 
                                    h4(div("Completar la tabla:",style="color:grey")),                            
                                    
                                    column(3,(numericInput("casoexpc",label="Casos expuestos nivel I",value = 10)),
                                           (numericInput("casoexp2c",label="Casos expuestos nivel II",value = 10)),
                                           (numericInput("casoexp3c",label="Casos expuestos nivel III",value = 10)),
                                           (numericInput("casoexp4c",label="Casos expuestos nivel IV",value = 10)),
                                           (numericInput("casonoexpc",label="Casos no expuestos",value = 10))),
                                    
                                    column(3,(numericInput("nocasoexpc",label="Controles expuestos nivel I",value = 10)),
                                           (numericInput("nocasoexp2c",label="Controles expuestos nivel II",value = 71)),
                                           (numericInput("nocasoexp3c",label="Controles expuestos nivel III",value = 81)),
                                           (numericInput("nocasoexp4c",label="Controles expuestos nivel IV",value = 91)),
                                           (numericInput("nocasonoexpc",label=" Controles  no expuestos",value = 171))),
                                    
                                    column(6,strong(textOutput(("texto4"))),
                                           (tableOutput("resultado4"))),
                                    column(6,strong(textOutput(("texto4a"))),
                                           (tableOutput("resultado4a"))),
                                    column(6,strong(textOutput(("texto4b"))),
                                           (tableOutput("resultado4b"))),
                                    column(6,offset = 6, strong(textOutput(("texto4c"))),
                                           (tableOutput("resultado4c")))
                                    
                                  ))))                 
                     
         )),
#####################################################################################################
tabPanel("Razón de tasas",
         tabsetPanel(type = "tabs",
                    tabPanel("2 niveles de exposición",
                             wellPanel(
                               fluidPage(                              
                                 fluidRow(
                                   h4(div("Completar la tabla:",style="color:grey")),                             
                                   
                                   column(3,(numericInput("c1exp",label="Casos expuestos",value = 204)),
                                          (numericInput("c1noexp",label="Casos no expuestos",value = 240))),
                                   
                                   column(3,(numericInput("praexp",label=" Personas/tiempo expuestos",value = 94029)),
                                          (numericInput("pranoexp",label="Personas/tiempo no expuestos",value = 128528))),
                                   
                                   
                                   column(6,strong(textOutput(("text1")))),
                                   column(6,(tableOutput("res")))
                                   
                                 )))),
                    
                    tabPanel("3 niveles de exposición",
                             wellPanel(
                               fluidPage(                              
                                 fluidRow( 
                                   h4(div("Completar la tabla:",style="color:grey")),                             
                                   
                                   column(3,(numericInput("c1expa",label="Casos expuestos nivel I",value = 67)),
                                          (numericInput("c1exp2",label="Casos expuestos nivel II",value = 99)),
                                          (numericInput("c1noexpa",label="Casos no expuestos",value = 114))),
                                   column(3,(numericInput("praexpa",label="Personas/tiempo expuestos nivel I",value = 1200)),
                                          (numericInput("praexp2",label="Personas/tiempo expuestos nivel II",value = 1400)),
                                          (numericInput("pranoexpa",label=" Personas/tiempo no expuestos",value = 1500))),
                                   
                                   column(6,strong(textOutput(("text2"))),
                                          (tableOutput("res2"))),
                                   column(6,strong(textOutput(("text2b"))),
                                          (tableOutput("res2b")))
                                   
                                 )))),
                    tabPanel("4 niveles de exposición",
                             wellPanel(
                               fluidPage(                              
                                 fluidRow( 
                                   h4(div("Completar la tabla:",style="color:grey")),                            
                                   
                                   column(3,(numericInput("c1expb",label="Casos expuestos nivel I",value = 10)),
                                          (numericInput("c1exp2b",label="Casos expuestos nivel II",value = 19)),
                                          (numericInput("c1exp3b",label="Casos expuestos nivel III",value = 31)),
                                          (numericInput("c1noexpb",label="Casos no expuestos",value = 41))),
                                   
                                   column(3,(numericInput("praexpb
                                                          ",label=" Personas/tiempo nivel I",value = 300)),
                                          (numericInput("praexp2b",label=" Personas/tiempo expuestos nivel II",value = 312)),
                                          (numericInput("praexp3b",label=" Personas/tiempo expuestos nivel III",value = 432)),
                                          (numericInput("pranoexpb",label=" Personas/tiempo no expuestos",value = 324))),
                                   
                                   column(6,strong(textOutput(("text3"))),
                                          (tableOutput("res3"))),
                                   column(6,strong(textOutput(("text3b"))),
                                          (tableOutput("res3b"))),
                                   column(6,offset=6,strong(textOutput(("text3c"))),
                                          (tableOutput("res3c")))
                                   )))),
                    tabPanel("5 niveles de exposición",
                             wellPanel(
                               fluidPage(                              
                                 fluidRow( 
                                   h4(div("Completar la tabla:",style="color:grey")),                            
                                   
                                   column(3,(numericInput("c1expc",label="Casos expuestos nivel I",value = 10)),
                                          (numericInput("c1exp2c",label="Casos expuestos nivel II",value = 10)),
                                          (numericInput("c1exp3c",label="Casos expuestos nivel III",value = 10)),
                                          (numericInput("c1exp4c",label="Casos expuestos nivel IV",value = 10)),
                                          (numericInput("c1noexpc",label="Casos no expuestos",value = 10))),
                                   
                                   column(3,(numericInput("praexpc",label="Personas/tiempo expuestos nivel I",value = 1000)),
                                          (numericInput("praexp2c",label="Personas/tiempo expuestos nivel II",value = 2000)),
                                          (numericInput("praexp3c",label=" Personas/tiempo expuestos nivel III",value = 3000)),
                                          (numericInput("praexp4c",label=" Personas/tiempo expuestos nivel IV",value = 4000)),
                                          (numericInput("pranoexpc",label=" Personas/tiempo no expuestos",value = 5000))),
                                   
                                   column(6,strong(textOutput(("text4"))),
                                          (tableOutput("res4"))),
                                   column(6,strong(textOutput(("text4a"))),
                                          (tableOutput("res4a"))),
                                   column(6,strong(textOutput(("text4b"))),
                                          (tableOutput("res4b"))),
                                   column(6,offset=6,strong(textOutput(("text4c"))),
                                          (tableOutput("res4c")))
                                   
                                 ))))                 
                    
         )),
########################################################################################################
tabPanel(" OR Análisis estratificado",
         tabsetPanel(type = "tabs",
                     tabPanel("2 estratos", 
                              fixedPage(
                                wellPanel(
                                  fixedRow(width = 12,
                                           column(width = 12,br(),h4("Completar las tablas:")),br(),br(),
                                           
                                           column(3,(h4(div("Tabla cruda",style="color:grey"))),
                                                  
                                                  (numericInput("cexpestra",label="Casos expuestos",value = 24)),
                                                  (numericInput("cnoexpestra",label="Casos no expuestos",value = 36)),
                                                  
                                                  
                                                  h4(div("Estrato 1",style="color:grey")),
                                                  (numericInput("cexpe1",label="Casos en expuestos",value = 9)),
                                                  (numericInput("cnoexpe1",label="Casos no expuestos",value = 32)),
                                                  
                                                  h4(div("Estrato 2",style="color:grey")),
                                                  
                                                  (numericInput("cexpe2",label="Casos en expuestos",value = 15)),
                                                  (numericInput("cnoexpe2",label="Casos no expuestos",value = 4))),
                                           
                                           column(3,h4(br()),
                                                  (numericInput("ncexpestra",label=" Controles expuestos",value = 58)),
                                                  (numericInput("nocnoexpestra",label=" Controles  no expuestos",value = 40)),
                                                  h4(br()),
                                                  (numericInput("ncexpe1",label=" Controles expuestos",value = 8)),
                                                  (numericInput("nocnoexpe1",label=" Controles  no expuestos",value = 28)),
                                                  h4(br()),
                                                  (numericInput("ncexpe2",label=" Controles expuestos",value = 50)),
                                                  (numericInput("nocnoexpe2",label=" Controles  no expuestos",value = 12))),
                                           
                                           
                                           column(6,h4(br()),
                                                  strong(textOutput(("te1"))),
                                                  (tableOutput("re1")),
                                                  strong(textOutput(("te2aa"))),
                                                  (tableOutput("re2")),
                                                 
                                                  strong(textOutput(("te2"))),
                                                  (tableOutput("re2aa")))
                                           
                                           
                                           
                                           
                                  )))),
                     
                     
                     tabPanel("3 estratos",
                              fixedPage(
                                wellPanel(
                                  fixedRow(width = 12,
                                           column(width = 12,br(),h4("Completar las tablas:")),br(),br(),
                                           
                                           column(3,(h4(div("Tabla cruda",style="color:grey"))),
                                                  
                                                  (numericInput("cexp3",label="Casos expuestos",value = 20)),
                                                  (numericInput("cnoexp3",label="Casos no expuestos",value = 20)),
                                                  
                                                  
                                                  h4(div("Estrato 1",style="color:grey")),
                                                  (numericInput("cexpe4",label="Casos en expuestos",value = 5)),
                                                  (numericInput("cnoexpe4",label="Casos no expuestos",value = 10)),
                                                  
                                                  h4(div("Estrato 2",style="color:grey")),
                                                  
                                                  (numericInput("cexpe5",label="Casos en expuestos",value = 10)),
                                                  (numericInput("cnoexpe5",label="Casos no expuestos",value = 15)),
                                                  
                                                  h4(div("Estrato 3",style="color:grey")),
                                                  
                                                  (numericInput("cexpe6",label="Casos en expuestos",value = 1)),
                                                  (numericInput("cnoexpe6",label="Casos no expuestos",value = 8))),
                                           
                                           column(3,h4(br()),
                                                  (numericInput("ncexp3",label=" Controles expuestos",value = 200)),
                                                  (numericInput("nocnoexp3",label=" Controles no expuestos",value = 300)),
                                                  h4(br()),
                                                  (numericInput("ncexpe4",label=" Controles expuestos",value = 100)),
                                                  (numericInput("nocnoexpe4",label=" Controles  no expuestos",value = 150)),
                                                  h4(br()),
                                                  (numericInput("ncexpe5",label=" Controles expuestos",value = 100)),
                                                  (numericInput("nocnoexpe5",label=" Controles  no expuestos",value = 150)),
                                                  h4(br()),
                                                  (numericInput("ncexpe6",label=" Controles expuestos",value = 100)),
                                                  (numericInput("nocnoexpe6",label=" Controles  no expuestos",value = 100))),
                                           
                                           column(6,h4(br()),
                                                  strong(textOutput(("te3"))),
                                                  (tableOutput("re3")),
                                                  strong(textOutput(("te4aa"))),
                                                  (tableOutput("re4")),
                                                 
                                                  strong(textOutput(("te4"))),
                                                  (tableOutput("re4aa"))))
                                  
                                  
                                  
                                  
                                  
                                ))))
         
         
         
         
),
###################################################################################################################
tabPanel("RT Analisis estratificado",
         tabsetPanel(type = "tabs",
                     tabPanel("2 estratos",                 
                              fixedPage(
                                wellPanel(
                                  fixedRow(width = 12,
                                           column(width = 12,br(),h4("Completar las tablas:")),br(),br(),
                                           
                                           column(3,(h4(div("Tabla cruda",style="color:grey"))),
                                                  
                                                  (numericInput("ce",label="Enfermos expuestos",value = 60)),
                                                  (numericInput("cnoe",label="Enfermos no exp",value = 75)),
                                                  h4(div("Estrato 1",style="color:grey")),
                                                  (numericInput("ce1",label="Enfermos en expuestos",value = 25)),
                                                  (numericInput("cnoe1",label="Enfermos no exp",value = 75)),
                                                  h4(div("Estrato 2",style="color:grey")),
                                                  (numericInput("ce2",label="Enfermos en expuestos",value = 30)),
                                                  (numericInput("cnoe2",label="Enfermos no exp",value = 60))),
                                           
                                           column(3,h4(br()),
                                                  (numericInput("pte",label=" Personas/tiempo expuestos",value = 1500)),
                                                  (numericInput("ptnoe",label=" Personas/tiempo no expuestos",value = 1500)),
                                                  h4(br()),      
                                                  (numericInput("pte1",label=" Personas/tiempo expuestos",value = 1000)),
                                                  (numericInput("ptnoe1",label=" Personas/tiempo no expuestos",value = 1000)),
                                                  h4(br()), 
                                                  (numericInput("pte2",label=" Personas/tiempo expuestos",value = 500)),
                                                  (numericInput("ptnoe2",label=" Personas/tiempo no expuestos",value = 500))),
                                           
                                           column(6,h4(br()),
                                                  strong(textOutput(("tcmp1"))),
                                                  (tableOutput("rcmp1")),
                                                  strong(textOutput(("tcmp2aa"))),
                                                  (tableOutput("rcmp2")),
                                                 
                                                  strong(textOutput(("tcmp2"))),
                                                  (tableOutput("rcmp2aa")))
                                           
                                           
                                  )))),
                     
                     
                     tabPanel("3 estratos",
                              fixedPage(
                                wellPanel(
                                  fixedRow(width = 12,
                                           column(width = 12,br(),h4("Completar las tablas:")),br(),br(),
                                           
                                           column(3,(h4(div("Tabla cruda",style="color:grey"))),
                                                  
                                                  (numericInput("ce3",label="Enfermos expuestos",value =200)),
                                                  (numericInput("cnoe3",label="Enfermos no ",value = 100)),
                                                  
                                                  h4(div("Estrato 1",style="color:grey")),
                                                  (numericInput("ce4",label="Enfermos en expuestos",value = 90)),
                                                  (numericInput("cnoe4",label="Enfermos no expuestos ",value = 50)),
                                                  
                                                  h4(div("Estrato 2",style="color:grey")),
                                                  (numericInput("ce5",label="Enfermos en expuestos",value = 60)),
                                                  (numericInput("cnoe5",label="Enfermos no expuestos",value = 25)),
                                                  
                                                  h4(div("Estrato 3",style="color:grey")),
                                                  (numericInput("ce6",label="Enfermos en expuestos",value = 50)),
                                                  (numericInput("cnoe6",label="Enfermos no expuestos",value = 25))),
                                           
                                           column(3,h4(br()),
                                                  (numericInput("pte3",label=" Personas/tiempo expuestos",value = 1500)),
                                                  (numericInput("ptnoe3",label=" Personas/tiempo no expuestos",value = 1600)),
                                                  h4(br()),
                                                  (numericInput("pte4",label=" Personas/tiempo expuestos",value = 500)),
                                                  (numericInput("ptnoe4",label=" Personas/tiempo no expuestos",value = 500)),
                                                  h4(br()),
                                                  (numericInput("pte5",label=" Personas/tiempo expuestos",value = 500)),
                                                  (numericInput("ptnoe5",label=" Personas/tiempo no expuestos",value = 500)),
                                                  h4(br()),
                                                  (numericInput("pte6",label=" Personas/tiempo expuestos",value = 500)),
                                                  (numericInput("ptnoe6",label=" Personas/tiempo no expuestos",value = 500))),
                                           
                                           column(6,h4(br()),
                                                  strong(textOutput(("tcmp3"))),
                                                  (tableOutput("rcmp3")),
                                                  strong(textOutput(("tcmp4aa"))),
                                                  (tableOutput("rcmp4")),
                                                  
                                                  strong(textOutput(("tcmp4"))),
                                                  (tableOutput("rcmp4aa"))))
                                  
                                  
                                  
                                  
                                  
                                  
                                ))))),
############################################################################################
tabPanel("Pruebas diagnósticas",
         tabsetPanel(type = "tabs",
                     tabPanel("Validez de la prueba",                 
                              wellPanel(
                                fluidPage(                              
                                  fluidRow(
                                    h4(div("Completar la tabla",style="color:grey")),                             
                                    
                                    column(2,(numericInput("vp",label="Verdaderos Positivos",value = 89)),
                                           (numericInput("fn",label="Falsos negativos",value = 3))),
                                    column(2,(numericInput("fp",label=" Falsos positivos",value = 7)),
                                           (numericInput("vn",label="Verdaderos negativos",value = 99))),
                                    column(8,strong(textOutput(("cmp1")))),
                                    column(8,(tableOutput("cmp2")))
                                    
                                  )))))
),
###########################################################################################################
tabPanel("Estandarización de tasas",
         tabsetPanel(type = "tabs",
                     tabPanel(div("Entrada de datos método directo",style = "color:blue"),  
                              fixedPage(
                                wellPanel(
                                  fixedRow(width = 3,
                                           column(width = 12,br(),h3("Tabla de introducción de datos para estandarización de tasas método directo")),br(),br(),
                                           
                                           column(width = 2, h4("GRUPOS ETARIOS"),selectInput("ngrupos",label = "N de grupos de edad",
                                                                                              choices = list("0"=0,"3" = 3, "5" = 5, "7" = 7,"8"=8),selected = 0),
                                                  h4("TIPODE TASA"),selectInput("tipotasa","Tasa por:",choices = list("cien" = 100, "mil" = 1000, "cienmil" = 100000), 
                                                                                selected = 1000),
                                                  br(),br(),paste(" Introducir los casos y los habitantes para cada una de las poblaciones a comparar.                     ")),
                                           column(width = 10, uiOutput("param")) )))), 
                     
                     tabPanel("Método directo Poblacion Europea",
                              br(),fluidRow(
                                h3(column(5,offset=4,paste("Ajuste por población europea"))),br(),
                                
                                h4(column(5,offset=1,paste("población 1"))),
                                h4(column(5,offset=1,paste("población 2"))),br(),
                                h4(column(5,offset=1,textOutput("gedad1"))),
                                h4(column(5,offset=1,textOutput("gedad2"))),
                                br(),fluidRow(
                                  column(5,offset=1,tableOutput("tabla1")),
                                  column(5,offset=1,tableOutput("tabla2"))),
                                h6(column(5,offset=1,textOutput("comentario1"))),
                                h6(column(5,offset=1,textOutput("comentario2"))),
                                br(),fluidRow(
                                  h4(column(5,offset=1,textOutput("brutas1"))),
                                  h4(column(5,offset=1,textOutput("brutas2"))),br(),
                                  h4(column(5,offset=1,textOutput("ajustadas1"))),
                                  h4(column(5,offset=1,textOutput("ajustadas2")))),
                                fluidRow(
                                  h4(column(5,offset=1,textOutput("ajustadas3"))),
                                  h4(column(5,offset=1,textOutput("ajustadas4")))),
                                br(),fluidRow(wellPanel(
                                  h4(column(5,offset=3,textOutput("ric")))))
                              )), 
                     tabPanel("Método directo Poblacion Mundial",
                              br(),fluidRow(
                                h3(column(5,offset=4,paste("Ajuste por población mundial"))),br(),
                                h4(column(5,offset=1,paste("población 1"))),
                                h4(column(5,offset=1,paste("población 2"))),br(),
                                h4(column(5,offset=1,textOutput("gedad1a"))),
                                h4(column(5,offset=1,textOutput("gedad2a"))),
                                br(),fluidRow(
                                  column(5,offset=1,tableOutput("tabla1a")),
                                  column(5,offset=1,tableOutput("tabla2a"))),
                                h6(column(5,offset=1,textOutput("comentario1a"))),
                                h6(column(5,offset=1,textOutput("comentario2a"))),
                                br(),fluidRow(
                                  h4(column(5,offset=1,textOutput("brutas1a"))),
                                  h4(column(5,offset=1,textOutput("brutas2a"))),br(),
                                  h4(column(5,offset=1,textOutput("ajustadas1a"))),
                                  h4(column(5,offset=1,textOutput("ajustadas2a")))),
                                fluidRow(
                                  h4(column(5,offset=1,textOutput("ajustadas3a"))),
                                  h4(column(5,offset=1,textOutput("ajustadas4a")))),
                                br(),fluidRow(wellPanel(
                                  h4(column(5,offset=3,textOutput("rica")))))
                              )
                              
                     ),
                     tabPanel(div("Entrada de datos método indirecto",style = "color:blue"),  
                              fixedPage(
                                wellPanel(
                                  fixedRow(width = 3,
                                           column(width = 12,br(),h3("Tabla de introducción de datos para estandarización por el método indirecto")),br(),br(),
                                           
                                           column(width = 2, h4("GRUPOS ETARIOS"),selectInput("ngrupos2",label = "N de grupos de edad",
                                                                                              choices = list("0"=0,"3" = 3, "5" = 5, "7" = 7,"8"=8),selected = 0),
                                                  h4("TIPODE TASA"),selectInput("tipotasa2","Tasa por:",choices = list("cien" = 100, "mil" = 1000, "cienmil" = 100000), 
                                                                                selected = 1000),
                                                  br(),br(),paste(" Introducir los casos y los habitantes para la población de referencia")),
                                           column(width = 10, uiOutput("param2")) )))),  
                     tabPanel("método Indirecto",
                              fluidPage(
                                br(),
                                h5(column(5,offset=1,numericInput("casos1","Introducir los casos totales en la población de estudio",value=100))),
                                br(),
                                h4(column(5,offset=1,textOutput("creferencia"))),
                                br(),h4(column(5,offset=1,textOutput("salida"))),
                                
                                br(),br(),h4(column(5,offset=1,textOutput("explicaciÃ³n"))),
                                br(),fluidRow(
                                  column(5,offset=1,tableOutput("tindirec")),
                                  br(), h4(column(5,offset=1,textOutput("texindirect"))),
                                  br(),h4(column(5,offset=1,textOutput("texindirect2")))             
                                )  
                                
                              ))
                     
                     
         ))

))

