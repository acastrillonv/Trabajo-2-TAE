library(shiny)
library(scorecard)
library(pander)
library(kableExtra)
library(sjmisc)
library(rsconnect)

load("card.RData", verbose = TRUE)
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
                      textInput("loan_amnt", h4("Monto del préstamo"), value = 0),
                      textInput("funded_amnt", h4("Cantidad total comprometida con el préstamo"), value = 0),
                      textInput("funded_amnt_inv", h4("Monto total invertido por los inversionistas para el préstamo"), value = 0),
                      textInput("int_rate", h4("Tasa de interés del préstamo"), value = 0),
                      textInput("installment", h4("Cuota mensual del prestatario"), value = 0),
                      textInput("annual_inc", h4("Ingresos anuales declarados por el prestatario"), value = 0),
                      textInput("dti", h4("DTI"), value = 0),
                      textInput("open_acc", h4("Número de operaciones abiertas en los ultimos 6 meses"), value = 0),
                      textInput("revol_bal", h4("Saldo rotatorio de crédito total"), value = 0),
                      textInput("revol_util", h4("Saldo utilizado de préstamo disponible"), value = 0),
                      textInput("out_prncp", h4("Capital restante pendiente por el monto total financiado"), value = 0),
                      textInput("out_prncp_inv", h4("Capital pendiente restante por parte del monto total financiado por los inversores"), value = 0),
                      textInput("total_pymnt", h4("Pagos recibidos hasta la fecha por el monto total financiado"), value = 0),
                      textInput("total_pymnt_inv", h4("Pagos recibidos hasta la fecha por parte del monto total financiado por los inversores"), value = 0),
                      textInput("total_rec_prncp", h4("Capital recibido hasta la fecha"), value = 0),
                      textInput("total_rec_int", h4("Intereses recibidos hasta la fecha"), value = 0),
                      textInput("last_pymnt_amnt", h4("Valor último pago recibido"), value = 0),
                      selectInput(
                        "term",
                        h4("Número de cuotas"),
                        choices = list("36 cuotas" = "36 months" ,
                                       "60 cuotas" = "60 months"),
                        selected = "36 months"
                      ),
                      selectInput(
                        "grade",
                        h4("Grado de préstamo"),
                        choices = list(
                          "A" = "A" ,
                          "B" = "B",
                          "C" = "C",
                          "D" = "D",
                          "E" = "E",
                          "F" = "F",
                          "G" = "G"
                        ),
                        selected = "A"
                      ),
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
                        h4("Motivo préstamo"),
                        choices = list(
                          "Deuda" = "debt_consolidation" ,
                          "Tarjeta de crédito" = "credit_card",
                          "Remodelacion hogar" = "home_improvement",
                          "Otro" = "other",
                          "Compra grande" = "major_purchase",
                          "Pequeña empresa" = "small_business",
                          "Carro" = "car",
                          "Medinal" = "medical",
                          "Mudanza" = "moving",
                          "Vacaciones" = "vacation",
                          "Matrimonio" = "wedding",
                          "Casa" = "house",
                          "Educacion" = "educational",
                          "Energía renovable" = "renewable_energy"
                        ),
                        selected = "debt_consolidation"
                      ),
                      selectInput(
                        "initial_list_status",
                        h4("Estado de cotización del préstamo"),
                        choices = list("F" = "f" ,
                                       "W" = "w"),
                        selected = "f"
                      )
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
        loan_amnt=input$loan_amnt,
        funded_amnt=input$funded_amnt,
        funded_amnt_inv=input$funded_amnt_inv,
        int_rate=input$int_rate,
        installment=input$installment,
        annual_inc=input$annual_inc,
        dti=input$dti,
        open_acc=input$open_acc,
        revol_bal=input$revol_bal,
        revol_util=input$revol_util,
        out_prncp=input$out_prncp,
        out_prncp_inv=input$out_prncp_inv,
        total_pymnt=input$total_pymnt,
        total_pymnt_inv=input$total_pymnt_inv,
        total_rec_prncp=input$total_rec_prncp,
        total_rec_int=input$total_rec_int,
        last_pymnt_amnt=input$last_pymnt_amnt,
        term=input$term,
        grade=input$grade,
        verification_status=input$verification_status,
        purpose=input$purpose,
        initial_list_status=input$initial_list_status
      )
      
      #term
      t36 <- datos_muestra[2,27]
      t60 <- datos_muestra[1,27]
      
      #grade
      gA <- datos_muestra[13,28]
      gB <- datos_muestra[4,28]
      gC <- datos_muestra[3,28]
      gD <- datos_muestra[1,28]
      gE <- datos_muestra[9,28]
      gF <- datos_muestra[8,28]
      gG <- datos_muestra[27,28]
      
      #initial
      
      i_f <- datos_muestra[5,34]
      i_w <- datos_muestra[1,34]
      
      #porpuse
      
      p_debt_consolidation <- datos_muestra[1,33]
      p_credit_card <- datos_muestra[2,33]
      p_other <- datos_muestra[4,33]
      p_moving <- datos_muestra[6,33]
      p_major_purchase <- datos_muestra[7,33]
      p_medical <- datos_muestra[16,33]
      p_small_business <- datos_muestra[23,33]
      p_home_improvement <- datos_muestra[32,33]
      p_house <- datos_muestra[68,33]
      p_wedding <- datos_muestra[107,33]
      p_car <- datos_muestra[119,33]
      p_vacation <- datos_muestra[214,33]
      p_educational <- datos_muestra[2136,33]
      p_renewable_energy <- datos_muestra[892,33]
      
      #verification
      v_Source_Verified <- datos_muestra[1,31]
      v_Verified <- datos_muestra[2,31]
      v_not_Verified <- datos_muestra[5,31]
      
      
      respuesta$loan_amnt <- as.numeric(respuesta$loan_amnt)
      respuesta$funded_amnt <- as.numeric(respuesta$funded_amnt)
      respuesta$funded_amnt_inv <- as.numeric(respuesta$funded_amnt_inv)
      respuesta$int_rate <- as.numeric(respuesta$int_rate)
      respuesta$installment <- as.numeric(respuesta$installment)
      respuesta$annual_inc <- as.numeric(respuesta$annual_inc)
      respuesta$dti <- as.numeric(respuesta$dti)
      respuesta$open_acc <- as.numeric(respuesta$open_acc)
      respuesta$revol_bal <- as.numeric(respuesta$revol_bal)
      respuesta$revol_util <- as.numeric(respuesta$revol_util)
      respuesta$out_prncp <- as.numeric(respuesta$out_prncp)
      respuesta$out_prncp_inv <- as.numeric(respuesta$out_prncp_inv)
      respuesta$total_pymnt <- as.numeric(respuesta$total_pymnt)
      respuesta$total_pymnt_inv <- as.numeric(respuesta$total_pymnt_inv)
      respuesta$total_rec_prncp <- as.numeric(respuesta$total_rec_prncp)
      respuesta$total_rec_int <- as.numeric(respuesta$total_rec_int)
      respuesta$last_pymnt_amnt <- as.numeric(respuesta$last_pymnt_amnt)
      
      
      # respuesta$term <- t36
      if ( str_contains(respuesta$term,"36 months") ){
        respuesta$term <- t36
      }else{
        respuesta$term <- t60
      }
      
      # respuesta$grade <- gA
      if ( str_contains(respuesta$grade,"A") ){
        respuesta$grade <- gA
      }else if ( str_contains(respuesta$grade,"B") ){
        respuesta$grade <- gB
      }else if ( str_contains(respuesta$grade,"C") ){
        respuesta$grade <- gC
      }else if ( str_contains(respuesta$grade,"D") ){
        respuesta$grade <- gD
      }else if ( str_contains(respuesta$grade,"E") ){
        respuesta$grade <- gE
      }else if ( str_contains(respuesta$grade,"F") ){
        respuesta$grade <- gF
      }else if ( str_contains(respuesta$grade,"G") ){
        respuesta$grade <- gF
      }
      
      # respuesta$verification_status <- v_Source_Verified
      
      if ( str_contains(respuesta$verification_status,"Source Verified") ){
        respuesta$verification_status <- v_Source_Verified
      }else if ( str_contains(respuesta$verification_status,"Not Verified") ){
        respuesta$verification_status <- v_not_Verified
      }else{
        respuesta$verification_status <- v_Verified
      }
      
      # respuesta$purpose <- p_other
      
      if ( str_contains(respuesta$purpose,"debt_consolidation") ){
        respuesta$purpose <- p_debt_consolidation
      }else if ( str_contains(respuesta$purpose,"credit_card") ){
        respuesta$purpose <- p_credit_card
      }else if ( str_contains(respuesta$purpose,"other") ){
        respuesta$purpose <- p_other
      }else if ( str_contains(respuesta$purpose,"moving") ){
        respuesta$purpose <- p_moving
      }else if ( str_contains(respuesta$purpose,"major_purchase") ){
        respuesta$purpose <- p_major_purchase
      }else if ( str_contains(respuesta$purpose,"medical") ){
        respuesta$purpose <- p_medical
      }else if ( str_contains(respuesta$purpose,"small_business") ){
        respuesta$purpose <- p_small_business
      }else if ( str_contains(respuesta$purpose,"home_improvement") ){
        respuesta$purpose <- p_home_improvement
      }else if ( str_contains(respuesta$purpose,"house") ){
        respuesta$purpose <- p_house
      }else if ( str_contains(respuesta$purpose,"wedding") ){
        respuesta$purpose <- p_wedding
      }else if ( str_contains(respuesta$purpose,"car") ){
        respuesta$purpose <- p_car
      }else if ( str_contains(respuesta$purpose,"vacation") ){
        respuesta$purpose <- p_vacation
      }else if ( str_contains(respuesta$purpose,"educational") ){
        respuesta$purpose <- p_educational
      }else if ( str_contains(respuesta$purpose,"renewable_energy") ){
        respuesta$purpose <- p_renewable_energy
      }
      
      # respuesta$initial_list_status <- i_f
      if ( str_contains(respuesta$initial_list_status,"f") ){
        respuesta$initial_list_status <- i_f
      }else{
        respuesta$initial_list_status <- i_w
      }
      
      Nuevo_score = scorecard_ply(respuesta, card)
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

