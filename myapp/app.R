library(shiny)
library(scorecard)
library(pander)
library(kableExtra)
library(sjmisc)
library(rsconnect)

load("card_nuevo.RData", verbose = TRUE)
load("datos_muestra.RData", verbose = TRUE)

# See above for the definitions of ui and server
ui <- fluidPage(# App title ----
                titlePanel(title = h1("Calcula tu Scorecard!",align='center'),windowTitle = 'Calculadora Scorecard'),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    div(h2("Ingresa tus datos",align='center')),
                    div(
                      textInput("int_rate", h4("Tasa de interés del préstamo"), value = 0),
                      textInput("installment", h4("Cuota mensual del prestatario"), value = 0),
                      textInput("annual_inc", h4("Ingresos anuales declarados por el prestatario"), value = 0),
                      textInput("open_acc", h4("Créditos abiertos del prestatario"), value = 0),
                      selectInput(
                        "verification_status",
                        h4("Ingresos o fuente de ingresos verificada"),
                        choices = list(
                          "Verificada" = "Verified" ,
                          "Ingresos verificados" = "Source Verified",
                          "No verificada" = "Not Verified"
                        ),
                        selected = "Verified"
                      ),
                      
                      selectInput(
                        "purpose",
                        h4("Motivo del préstamo"),
                        choices = list(
                          "Deuda" = "debt_consolidation" ,
                          "Tarjeta de crédito" = "credit_card",
                          "Remodelacion hogar" = "home_improvement",
                          "Otro" = "other",
                          "Compra grande" = "major_purchase",
                          "Pequeña empresa" = "small_business",
                          "Carro" = "car",
                          "Medicina" = "medical",
                          "Mudanza" = "moving",
                          "Vacaciones" = "vacation",
                          "Matrimonio" = "wedding",
                          "Casa" = "house",
                          "Educacion" = "educational",
                          "Energía renovable" = "renewable_energy"
                        ),
                        selected = "debt_consolidation"
                      ),
                    ),
                    br(),
                    div(
                      actionButton("Calculate", label = h4("Calcular",style='color:green')),
                      actionButton("refresh", label = h4("Limpiar",style='color:blue'),onclick ="window.open('https://sebasrendon12.shinyapps.io/myapp2/', '_self')"),
                      align='center'
                    )
                    
                    ,width = 6),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(h2("Tu puntaje es", align='center'),
                            style='position: fixed;right: 1VH;top: 25%; width:50%;',
                            
                            # Output:
                            div(h1(uiOutput("Score",align='center'))),
                            div(img(src = "scorecard.png", height = 150, width = 650,align='center'),align='center'),
                            div(h3("Este es el score promedio de una muestra de la poblacion: 428",align='center'))
                            )
                ))

server <- function(input, output) {

  output$Score <- renderUI({
    if (input$refresh) {
      0
    } else if (input$Calculate) {
      respuesta <- data.frame(
        int_rate=input$int_rate,
        installment=input$installment,
        annual_inc=input$annual_inc,
        open_acc=input$open_acc,
        verification_status=input$verification_status,
        purpose=input$purpose
      )
      

      
      #porpuse
      
      p_debt_consolidation <- dataos_selec2[1,6]
      p_credit_card <- dataos_selec2[3,6]
      p_other <- dataos_selec2[20,6]
      p_moving <- dataos_selec2[110,6]
      p_major_purchase <- dataos_selec2[56,6]
      p_medical <- dataos_selec2[8,6]
      p_small_business <- dataos_selec2[16,6]
      p_home_improvement <- dataos_selec2[18,6]
      p_house <- dataos_selec2[92,6]
      p_wedding <- dataos_selec2[325,6]
      p_car <- dataos_selec2[28,6]
      p_vacation <- dataos_selec2[252,6]
      p_educational <- dataos_selec2[186,6]
      p_renewable_energy <- dataos_selec2[414,6]
      
      #verification
      v_Source_Verified <- dataos_selec2[7,5]
      v_Verified <- dataos_selec2[3,5]
      v_not_Verified <- dataos_selec2[1,5]
      

      respuesta$int_rate <- as.numeric(respuesta$int_rate)
      respuesta$installment <- as.numeric(respuesta$installment)
      respuesta$annual_inc <- as.numeric(respuesta$annual_inc)
      respuesta$open_acc <- as.numeric(respuesta$open_acc)

      
      
      
      
      # respuesta$verification_status <- v_Source_Verified
      
      if ( str_contains(respuesta$verification_status,"Source Verified") ){
        respuesta$verification_status <- v_Source_Verified$verification_status
      }else if ( str_contains(respuesta$verification_status,"Not Verified") ){
        respuesta$verification_status <- v_not_Verified$verification_status
      }else{
        respuesta$verification_status <- v_Verified$verification_status
      }
      
      # respuesta$purpose <- p_other
      
      if ( str_contains(respuesta$purpose,"debt_consolidation") ){
        respuesta$purpose <- p_debt_consolidation$purpose
      }else if ( str_contains(respuesta$purpose,"credit_card") ){
        respuesta$purpose <- p_credit_card$purpose
      }else if ( str_contains(respuesta$purpose,"other") ){
        respuesta$purpose <- p_other$purpose
      }else if ( str_contains(respuesta$purpose,"moving") ){
        respuesta$purpose <- p_moving$purpose
      }else if ( str_contains(respuesta$purpose,"major_purchase") ){
        respuesta$purpose <- p_major_purchase$purpose
      }else if ( str_contains(respuesta$purpose,"medical") ){
        respuesta$purpose <- p_medical$purpose
      }else if ( str_contains(respuesta$purpose,"small_business") ){
        respuesta$purpose <- p_small_business$purpose
      }else if ( str_contains(respuesta$purpose,"home_improvement") ){
        respuesta$purpose <- p_home_improvement$purpose
      }else if ( str_contains(respuesta$purpose,"house") ){
        respuesta$purpose <- p_house$purpose
      }else if ( str_contains(respuesta$purpose,"wedding") ){
        respuesta$purpose <- p_wedding$purpose
      }else if ( str_contains(respuesta$purpose,"car") ){
        respuesta$purpose <- p_car$purpose
      }else if ( str_contains(respuesta$purpose,"vacation") ){
        respuesta$purpose <- p_vacation$purpose
      }else if ( str_contains(respuesta$purpose,"educational") ){
        respuesta$purpose <- p_educational$purpose
      }else if ( str_contains(respuesta$purpose,"renewable_energy") ){
        respuesta$purpose <- p_renewable_energy$purpose
      }
      
      
      Nuevo_score = scorecard_ply(respuesta, card_nuevo)
      Nuevo_score
      
    } else{
      0
    }
  })
  
  refresh <- function() {
    fxn <- "refresh"
    params <- list()
    jsFuncHelper(fxn, params)
  }
}

shinyApp(ui = ui, server = server)

