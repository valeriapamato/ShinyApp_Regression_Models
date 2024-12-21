library(shiny)
library(sjmisc)
library(BayesFactor)
library(DT)

library(interplot)
library(ggplot2)

library(effects)
library(sjPlot)

ui<-(fluidPage(
  
  titlePanel(h1(strong("MODELLO REGRESSIONE LINEARE SEMPLICE"), align = "center")),  
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file"),
      helpText("Default max. file size is 5MB"),
      radioButtons("separator","Separatore: ", choices= c("Si","No"),selected="No",inline=TRUE),
      radioButtons("separator_type","Tipo separatore: ",choices = c(";",",",":"), selected=";",inline=TRUE),
      tags$hr(),
      h4(strong("Seleziona le variabili:")),
      selectInput("selected_y", "Seleziona variabile Dipendente Y",""),  # y
      selectInput("selected_x", "Seleziona variabile Indipendente X",""), # x
      
      tags$hr(),
      withMathJax(
        h4(strong("Ipotesi")),
      strong("\\(\ H_0 \\) : \\( \\beta_1 \\) = 0"),
      br(),
      span(
        strong("\\(\ H_1 \\) : \\( \\beta_1 \\) "),
        selectInput("sign", label = NULL , choices = c(">","<","!=")),
        strong("   0"))
        
      ),
      
      
      #### selectinput style ###
      tags$head(
        tags$style(type="text/css", 
                   "label.control-label, .selectize-control.single { 
         display: table-cell; 
         text-align: center; 
         vertical-align: middle; 
      } 
      label.control-label {
        padding-right: 15px;
      }
      .form-group { 
        display: table-row;
      }
      .selectize-control.single div.item {
        padding-right: 25px;
      }")
      )
      
    ),
    mainPanel(
      uiOutput("tb"),
      hr(),
      p(em("Created and Developed by"),br("Valeria Pamato"),style="text-align:center; font-family: times"),
      p(em("Bibliography"),br("Pastore, M. (2015) . “Analisi dei dati in psicologia con applicazioni in R”. Bologna: Il Mulino"),style="text-align:center; font-family: times")
    )
  )
))


################### SERVER ################### 


server<-(function(input,output,session){
  
  
  #####################################################################
  #####     ## INSERISCO IL FILE IN VARIABILE data ##              ####
  #####################################################################
  # input$file will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    
    if(is.null(file1)){return(NULL)} 
    if(input$separator == "No")
    { read.csv(file=file1$datapath, sep='', header = TRUE)}
    else{
      read.csv(file=file1$datapath, sep=input$separator_type, header = TRUE)
    }
  })
  
  
  ####################################################################
  #####             ## GESTIONE VARIABILI X E Y ##                ####
  ####################################################################
  
  # Output selected_x e selected_y aggiorna selectInput inserendo le voci del file
  observe({    
    updateSelectInput(session, "selected_y", choices = names(data())) #default la prima
    updateSelectInput(session, "selected_x", choices = names(data()),selected = tail(colnames(data()),1))  # default l'ultima
  })
  
  # inizializzare var che contiene i valori delle due variabili x e y
  var<-reactiveValues(x=0,
                      y=0)
  nomi_var<- reactiveValues(x="",
                            y="")
  
  #observeEvent per inserire i valori di x e y in var
  observeEvent({input$selected_x
    input$selected_y}, {
      var$x<-data()[, input$selected_x]
      var$y<-data()[, input$selected_y]
      
      nomi_var$x <- input$selected_x
      nomi_var$y <- input$selected_y
      
    })
  
  ######################################################################
  #####                   ## GENERO LE VARIE TAB ##                 ####
  ######################################################################
  
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h5(" ")
    else
      tabsetPanel(tabPanel("Data Summary", uiOutput("info_summary"),
                           checkboxInput('check_summ', 'Comando in r', FALSE),
                           uiOutput('r_summ'),
                           tableOutput("summ"),
                           uiOutput("info_table"),
                           DT::dataTableOutput("table")
                           ),
                  tabPanel("Correlazione", 
                           uiOutput("info_cor"),
                           tags$head(tags$style("#info_cor{font-size: 18px;}" )),
                           checkboxInput('check_scatterplot', 'Comando in r', FALSE),
                           uiOutput('r_scatterplot'),
                           plotOutput("plot"),
                           uiOutput("titolo_cor_test"),
                           checkboxInput('check_corr', 'Comando in r', FALSE),
                           uiOutput('r_corr'),
                           verbatimTextOutput("cor_test"),
                           ),
                  tabPanel("Modello di Regressione",uiOutput("mod_lin"),
                           tags$head(tags$style("#mod_lin{font-size: 18px;}" )),
                           checkboxInput('check_regr', 'Comando modello in r', FALSE),
                           uiOutput('r_regr'),
                           checkboxInput('check_abline', 'Comando grafico in r', FALSE),
                           uiOutput('r_abline'),
                           plotOutput("abline")),
                  tabPanel("Residui",uiOutput("info_assunti"),
                           tags$head(tags$style("#info_assunti{font-size: 18px;}" )),
                           checkboxInput('check_residui', 'Comando in r', FALSE),
                           uiOutput('r_residui'),
                           plotOutput("plot_residui"),
                           uiOutput("info_residui"),
                           tags$head(tags$style("#info_residui{font-size: 18px;}" ))),
                  tabPanel("Inferenza",uiOutput("info_inferenza"),
                           tags$head(tags$style("#info_inferenza{font-size: 18px;}" )),
                           checkboxInput('check_inferenza', 'Comando in r', FALSE),
                           uiOutput('r_inferenza'),
                           verbatimTextOutput("lm_summary"),
                           uiOutput("inferenza"),
                           tags$head(tags$style("#inferenza{font-size: 18px;}" )),
                           uiOutput("info_anova"),
                           tags$head(tags$style("#info_anova{font-size: 18px;}" )),
                           checkboxInput('check_anova', 'Comando grafico in r', FALSE),
                           uiOutput('r_anova'),
                           tableOutput("anova"),
                           tags$head(tags$style("#anova{margin-left:30%; 
                                                        margin-right:30%}" ))),
                  tabPanel("BIC e BF",uiOutput("info_BIC"),
                           tags$head(tags$style("#info_BIC{font-size: 18px;}" )),
                           checkboxInput('check_BIC', 'Comando in r', FALSE),
                           uiOutput('r_BIC'),
                           uiOutput("info_BF"),
                           tags$head(tags$style("#info_BF{font-size: 18px;}" )),
                           numericInput("BIC_2", 
                                        h3(" "), 
                                        value = 1),
                           tags$head(tags$style(type = "text/css", ".form-control.shiny-bound-input,.selectize-input {height: 30px; width: 80px}")),
                           uiOutput("BF"),
                           checkboxInput('check_BF', 'Comando in r', FALSE),
                           uiOutput('r_BF'))
                  
                  )
  })
  
  
  ################### DATA SUMMARY ################### 
  
  output$info_table <- renderUI({
    tags$div(
      br(),
      h3(strong("Tabella Dati")),
      br()
    )
    
  })
  
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- DT::renderDataTable({
    if(is.null(data())){return ()}
    data()
    
  })
  
  output$info_summary <- renderUI({
    tags$div(
      h3(strong("Summary delle variabili selezionate"))
    )
   
  })
  
  output$r_summ <- renderUI({
    if (!input$check_summ) return()
    helpText("summary(var_x)")
    
  })
  
     # output summary delle due variabili selezionate
  output$summ <- renderTable({
    
    if(is.null(data())){return ()}
    else{
      nomi <- c("","Min","1sr Qu.","Median","Mean","3rd Qu.","Max")
      summary_x <- c(nomi_var$x,min(var$x),quantile(var$x)[2],median(var$x),mean(var$x),quantile(var$x)[4],max(var$x)) 
      summary_y <- c(nomi_var$y,min(var$y),quantile(var$y)[2],median(var$y),mean(var$y),quantile(var$y)[4],max(var$y)) 
      summary <- rbind.data.frame(summary_x,summary_y)
      colnames(summary) <- nomi
      
      summary
    }
    
  })

  ###################  PLOT PIU' INFO RIGUARDO AL PLOT E LM ################### 
  
  # scatterplot + abline
  output$plot <- renderPlot({
    if(is.null(data())){return ()}
    plot(var$x,var$y, pch = 20, cex = 1, col = "black", xlab=nomi_var$x, ylab=nomi_var$y)
    #abline(lm(var$y~var$x),col="red")
  })
  
  #info cor
  output$info_cor<- renderUI({
    withMathJax(
      h3(strong("Coefficiente di Correlazione")),
      paste0("Il coefficiente di correlazione misura l'intensità della relazione lineare tra due variabili quantitative. Non permette di valutare, però, la relazione causa-effetto."), 
      br(),
      paste0("La formula del coefficiente di correlazione di Pearson è: 
             $$ \\ r_{xy} = \\frac { \\sum_{i=1}^n ( \ x_i - \\bar x ) \\cdot ( \ y_i - \\bar y )} 
             { \\sqrt { \\sum_{i=1}^n { ( \ x_i - \\bar x )^2 }} \\cdot \\sqrt { \\sum_{i=1}^n { ( \ y_i - \\bar y )^2 }}} = \\frac { \ Cov_{xy} } { \\ S_x \\cdot \ S_y}$$"),
      br(),
      paste0("Il coefficiente va da \\( [ -1 ; +1 ]\\) : "),
      tags$ul(
        tags$li("\\( -1  \\Rightarrow \\) perfetta relazione lineare negativa, ovvero all'aumentare di una variabile l'altra diminuisce"),
        tags$li("\\( +1  \\Rightarrow \\) perfetta relazione lineare positiva, ovvero all'aumentare di una variabile aumenta anche l'altra;"),
        tags$li("\\( 0  \\Rightarrow \\) non esiste una relazione lineare;")
      ),br(),
      
      h4(strong("Rappresentazione grafica dei dati: "))
    )
    
  })
  
  output$r_scatterplot <- renderUI({
    if (!input$check_scatterplot) return()
      helpText("plot(var_x,var_y, pch = 20, cex = 1, col = 'black', xlab= var_x, ylab= var_y)")
    
  })
  
  #titolo cor test
  output$titolo_cor_test <- renderUI({
    tags$div(
      br(),
      h4(strong("Correlazione tra ", nomi_var$x, " e ", nomi_var$y))
    )
  })
  
  output$r_corr <- renderUI({
    if (!input$check_corr) return()
    helpText("cor.test(var_y,var_x)")
  })
  
  #test correlazione
  output$cor_test <- renderPrint({
    br()
        print(cor.test(var$y,var$x))
  })
  
  
  ################## MOD. REGRESSIONE LINEARE ##################
  
  #teoria MRL
  output$mod_lin <- renderUI({
    ml<-lm(var$y~var$x)
    withMathJax(
      h3(strong("Modello Regressione Lineare Semplice")),
      paste0("La regressione mira a stabilire se tra due variabili vi sia una dipendenza funzionale, in particolare
             a quantificare la misura in cui una variabile (chiamata predittore) agisca su un'altra (chiamata dipendente)."), br(),
      br(),
      paste0("La regressione lineare consiste nel determinare il legame tra le due variabili attraverso una funzione lineare del tipo:"),
      paste0("$$\\ Y = \\beta_0 + \\beta_1 X $$"),
      paste("dove "),
      tags$ul(
        tags$li("\\( \ Y \\) è la variabie dipendente,"),
        tags$li("\\( \ X \\) è il predittore,"),
        tags$li("\\( \\beta_0 \\) rappresenta l'intercetta (cioè il valore che assume \\( \ Y \\) quando \\( \ X \\) vale zero),"),
        tags$li("\\( \\beta_1 \\) rappresenta il coefficiente angolare (cioè l'incremento di \\( \ Y \\) per ogni incremento unitario di \\( \ X \\)")
      ),
      br(),
      h4(strong("Retta di Regressione dei dati")),
      paste0("La retta di regressione riguardo i dati inseriti è: 
             $$\\ Y = \\ ", round(ml$coef[[1]], 3), " + ", round(ml$coef[[2]], 3)," \ X $$")
      
    )
  })
  
  output$r_regr <- renderUI({
    if (!input$check_regr) return()
    helpText("lm(var_y ~ var_x, data = 'nome_dataframe')")
  })
  
  #grafico dati + abline
  output$abline <- renderPlot({
    if(is.null(data())){return ()}
    plot(var$x,var$y, pch = 20, cex = 1, col = "black", xlab=nomi_var$x, ylab=nomi_var$y)
    abline(lm(var$y~var$x),col="red")
  })
  
  output$r_abline <- renderUI({
    if (!input$check_abline) return()
      helpText("plot( var_x, var_y, pch = 20, cex = 1, col = 'black', xlab = var_x, ylab = var_y)", br(),
               "abline( lm(var_y ~ var_x), col = 'red')")
  })
  
  
  ################### TAB RESIDUI ################### 
  
  #info assunti
  output$info_assunti <- renderUI({
    withMathJax(
      h3(strong("Assunti Modello di Regressione Lineare")),
      "È importante controllare che gli assunti del modello di regressione siano rispettati prima di fare inferenze sui dati.", br(),
      "Gli assunti sono:", br(),
      
      tags$ul(
        tags$li(strong("Linearità: "),
                paste0("Il valore attesso dell'errore per un dato valore di \\( \ X \\) è zero: $$ \ E  ( \\epsilon_i) =  \ E ( \\epsilon | \ x_i ) = 0 $$"),
                paste0("In pratica significa che il valore atteso della variabile dipendente, \ E ( \ Y ), è una funzione lineare del predittore")),br(),
        tags$li(strong("Normalità: "),
                paste0("Gli errori sono distribuiti normalmente intorno lo zero: $$ \\epsilon_i ~ \ N ( 0, \\sigma^2) $$")
        ),
        tags$li(strong("Omogeneità delle varianze: "),
                paste0("La varianza degli errori è costante per qualunque valori di \ X : $$ \ V ( \\epsilon | \ x_i ) = \\sigma^2 $$")
        ),
        tags$li(strong("Indipendenza: "),
                paste0("Tutte le coppie di errori \\( \\epsilon_i \\) ed \\( \\epsilon_j \\) sono tra loro indipendenti per ogni \\( \ i \\) \\( \\neq \\) \\( \ j \\)")
                
        )
      ),
      
      br(),
      h3(strong("Grafici Residui"))
    )
  })
  
  output$r_residui <- renderUI({
    if (!input$check_residui) return()
      helpText("par(mfrow = c(2,2))", br(),
            "plot(lm(var_x ~ var_y))")
    
    
  })
  
 #Plot residui 
  output$plot_residui <- renderPlot({
    par(mfrow = c(2,2))
    plot(lm(var$x~var$y))
  })
  
  #Info su residui
  output$info_residui <- renderUI({
    withMathJax(
      br(),
      h3(strong("Guida per interpretare i grafici:")),
      
      tags$ol(
        tags$li(strong("Residual vs Fitted: "),
                paste0("Rappresenta i residui \\( \\epsilon_i \\) in funzione dei valori attesi \\( \\hat \ y^2 \\)"), br(),
                paste0("Questo grafico permette di valutare l'assunto di indipendenza. Nel caso ideale i residui si distribuiscono
                       normalmente intorno all media zero e la linea continua (che rappresenta il trend stimato delle medie dei residui) risulta sostanzialmente piatta.
                       In presenza di una violazione dell'assunto di indipendenza dei residui, la linea può assumere ad esempio una forma crescente/decrescente.")
        ),br(),
        tags$li(strong("Normal Q-Q: "),
                paste0("Rappresenta i residui standardizzati in funzione dei quantili della normale e serve per valutare l'assunto di normalità."),br(),
                       "Se la distribuzione dei residui fosse perfettamente normale, i punti sarebbero tutti allineati lungo la retta grigia tratteggiata. In presenza
                       di una violazione dell'assunto di normalità, i punti non si ditribuirebbero lungo una retta."
        ),br(),
        tags$li(strong("Scale-Location: "),
                paste0("Rappresenta la radice quadrata dei residui standardizzati in funzione dei valori attesi \\( \\hat \ y^2 \\)."), br(),
                       "Questo grafico permette di valutare l'assunto di omogeneità delle varianze. La linea continua indica il trend stimato e non dovrebbe
                       esprimere alcun tipo di trend, indicando così l'omogeneità delle varianze dei residui."
          
        ),br(),
        tags$li(strong("Residual vs Leverage: "),
                "Rappresenta i residui in funzone del valore di leverage.", br(),
                "Con questo grafico si possono individuare casi anomali e casi influenti. Il valore di leverage misura
                la potenziale influenza di un dato sulle stime dei parametri del modello o quanto possa variare l'inclinazione della retta.", br(),
                "Ad esempio, un alto valore di leverage unito ad un alto residuo implica che il dato ha un forte valore sulle stime.", br(),
                "Una buona misura che riassume l'influenza di ciascun dato è la distanza di Cook. Nel grafico viene rappresentata utilizzando
                 le delle linee trattegiate. I punti che cadono nelle aree esterne delimitate da tali linee rappresentano distanze grandi e quindi
                 risultano essere valori influenti."
        )
      ),
      br(),br(),br()
    )
  })
  
  
  ###################  Inferenza ################### 
  
  output$info_inferenza <- renderUI({
    withMathJax(
      h3(strong("Inferenza")),
    paste0("Ci si pone ora il problema di stabilire se '", nomi_var$x, "' rappresenti un predittore statisticamente significativo di '", nomi_var$y, "'."),br(),
    paste0("L'intensità della relazione tra le due varibili è legata al parametro \\( \\beta_1 \\) chiamato anche coefficiente di regressione."),br(),br(),
    h4(strong("Informazioni sul modello: "))
    )
    
  })
  
  output$r_inferenza <- renderUI({
    if (!input$check_inferenza) return()
    helpText("fit <- lm(var_y ~ var_x)", br(), "summary(fit)")
    
  })
  
  output$lm_summary <- renderPrint({
    fit <- lm(var$y~var$x)
    print(summary(fit))
  })
  
  output$inferenza <- renderUI({
    
    var_segno <- input$sign
    fit <- lm(var$y~var$x)
    t_value <- summary(fit)$coef[2,3]
    p_value <- summary(fit)$coef[2,4]
    h_ok <- FALSE #ipotesi e dati sono congruenti
    n_code <- NULL
    r <- summary(lm(var$y~var$x))$r.squared
    #controllo per vedere se ipotesi e dati sono conguenti
    if(var_segno == ">" & t_value>0){
      h_ok <- TRUE
      n_code <- 1
      p_value <- p_value/2
    }
    if(var_segno == "<" & t_value<0){
      h_ok <- TRUE
      n_code <- 1
      p_value <- p_value/2
    }
    if(var_segno == "!="){
      h_ok <- TRUE
      n_code <- 2
    }
    
    withMathJax(
        ## statistiche sui residui ##
        tags$div(
          br(),
          h4(strong("Info Residual")),
          paste0("Nella prima parte dell'output sono riportate le statistiche generali sui residui (Residuals)."), br(),
          paste0("L'analisi dei residui permette una prima valutazione sulla bontà dell'adattamento ai dati della retta stimata. In generale dovremmo attenderci
                 dei residui non troppo elevati con valori sia positivi che negativi. La mediana dovrebbe essere vicino allo zero e minimo e massimo approssimativamente uguali."),br()
        ),
        
        tags$div(
          br(),
          h4(strong("Info Coefficients")),
          paste0("Nella seconda parte dell'output (Coefficients) sono riportati i risultati relativi ai test sull'intercetta (Intercept)) e sul coefficiente di
                 regressione (",nomi_var$x,")"),br(),
          paste0("Nell'ordine si possono leggere i valori stimati dei parametri (Estimate) con relativo errore standard (Std. Error), la statistica test utilizzata
                 (t value) con relativa probabilità associata ( Pr( > | t | ) )"),br()
        ),
        
        
        tags$div(
          br(),
          h4(strong("Interpretazione t value")),
          paste0("Il valore del t value ci permette di verificare se le nostre ipotesi sono congruenti ai dati."),br(),
          paste0("Guardare il valore del t value e confrontarlo con \\( \ H_1 \\). Se si vuole fare un'ipotesi monodirezionale: "),br(),
          tags$ul(
            tags$li(paste0("Se il valore è \\( \ negativo \\) allora \\( \ H_1 \\) deve ipotizzare \\( \\beta_1 \ < 0 \\)")),
            tags$li(paste0("Se il valore è \\( \ positivo \\) allora \\( \ H_1 \\) deve ipotizzare \\( \\beta_1 \ \ > 0 \\)"))
          ),
          br()
        ),
        
        if(h_ok == FALSE){
          em("Modificare le ipotesi nel pannello a sinistra poichè \\( \ H_1 \\) e \\( \ t-value \\) non sono congruenti.") 
        },
        
       if(h_ok == TRUE){
        
        
          tags$div(
            h4(strong("Interpretazione p value")),
            paste0("Il valore di p value ci permette di determinare se il test è statisticamente significativo e quindi rigettare \\( \ H_0 \\) oppure no. Tre sono gli aspetti essenziali: "),br(),
            tags$ul(
              tags$li("Il p value indica la probabilità di un risultato uguale o più estremo di quello osservato, posto che sia vera \\( \ H_0 \\). Non rappresenta in alcun modo la probabilità che
                   sia vera \\( \ H_0 \\) e pertanto non può essere considerato come un misuratore del grado di falsità della stessa ipotesi"),
              tags$li("Il p value risente, in generale, della numerosità campionaria. Aumentando la dimensione del campione il valore di \\( \ p \\) tende a diminuire"),
              tags$li("Il p value non può essere considerato una misura dell'evidenza statistica, va usato come criterio decisionale per rigettare o meno \\( \ H_0 \\)")
            ),
            br(),
            paste0("In psicologia \\( \\alpha \\) (livello di significatività) è fissato al 5%. Pertanto se il p-value risulta: "),br(),
            tags$ul(
              tags$li("superiore al 0.05 (5%) il test non è statisticamente significativo, di conseguenza non si può rigettare l'ipotesi \\( \ H_0 \\);"),
              tags$li("inferiore al 0.05 (5%) il test è statisticamente significativo, di conseguenza si può rigettare l'ipotesi \\( \ H_0 \\).")
            ),br(),
            strong("Per quanto riguarda questo modello,"),
            if(p_value < 0.05){  
               paste0(" il test risulta statisticamente significativo e quindi si può rigettare \\( \ H_0 \\) perchè p value = ", round(p_value,5), " ed è minore di 0.05")
            }else {
              paste0( " il test \\( \ NON \\) risulta statisticamente significativo e quindi \\( \ NON \\) si può rigettare \\( \ H_0 \\) perchè p value = ", round(p_value,5), " ed e' maggiore di 0.05")
            }
          )
      },
      
      tags$div(
        br(),
        h4(strong("Ultime info ")),
        paste0("Nell'ultima parte dell'output son riportate alcune misure che permettono di valutare l'intensità della relazione tra le variabili ", nomi_var$x," e ",nomi_var$y,"."),br(),
        paste0("Il Residual standard error chiamato anche "),em("errore standard della regressione "), paste0(", indica la variabilità media dei punti intorno alla retta di regressione.
                                                                                                              Pertanto, quanto maggiore sarà questo valore tanto minore sarà il potere predittivo della retta di regressione."),br(),
        br(),
        paste0("R-Squared chiamato anche "),em("coefficiente di determinazione"), paste0(" ( \\( \ R^2 \\) ) rappresenta la porzione di devianza spiegata dalla relazione lineare. Ajusted R-Squared è lo stesso
                                                                                         coefficiente di determinazione corretto per i gradi di libertà."), br(),br(),
        
        strong("Il valore di \\( \ r^2 \\) ="),
        paste0(round(r,4)),
        br(),br(),
        tags$hr()
        )
    )
    })
  
  
  ##### Anova #####
  
  # info anova
  output$info_anova <- renderUI({
    withMathJax(
      br(),
      h3(strong("ANOVA")),
    paste("L'analisi della varianza (ANOVA) permette di avere informazioni riguardo ai livelli di variabilità all'interno del
          modello di regressione e forma le basi per i test di significatività."), br(),
    paste("Il concetto base della linea di regressione è dato da: $$ \ ( \ y_i - \\bar y \ ) = \ ( \\hat y_i - \\bar y \ ) + \ ( y_i - \\hat y_i \ ) $$ "), br(),
    paste0("Dove:"),
    tags$ul(
      tags$li(paste0("\\( \ y_i - \\bar y \\) : Variazione totale di y")),
      tags$li(paste0("\\( \\hat y_i - \\bar y \\) : Variazione della risposta media")),
      tags$li(paste0("\\( \ y_i - \\hat y_i \\) : Valore residuo."))
    ),br(),
    
    paste0("Mettendo a quadrato ciascuno di questi termini e sommando tutte le n osservazioni si ottiene l'equazione: 
           $$ \\sum \ ( \ y_i - \\bar y \ )^2 = \\sum \ ( \\hat y_i - \\bar y \ )^2 + \\sum \ ( y_i - \\hat y_i \ )^2 $$."), br(),
    paste0("che può anche essere scritta come \\( \ SST = \ SSM + \ SSE \\), dove \\( \ SST \\) è la devianza totale, \\( \ SSM \\) è la devianza di regressione,
           \\( \ SSE \\) è la devianza residua."), br(),br(),
    paste("Il quadrato della correlazione del campione è uguale al rapporto tra la somma del modello dei quadrati e la somma totale dei quadrati:  \\( \ r^2 \\) = \\( \ SSM \\) / \\( \ SST \\)."), br(),
    paste0("Ciò formalizza l'interpretazione di \\( \ r^2 \\) come spiegazione della frazione di variabilità nei dati spiegati dal modello di regressione."), br(),br(),
    paste("La varianza del campione \\( \ {s_y}^2 \\) è uguale a \\( ( \ y_i - \\bar y )^2 / ( \ n - 1) =  \ SST / \ DFT \\), la somma totale dei quadrati divisa per i gradi di libertà totali \\( \ DFT \\)."), br(),br(),
    paste0("Per la regressione lineare semplice, \\( \ MSM \\) (mean square model): $$ \ MSM = \\sum \ ( \\hat y_i - \\bar y \ )^2 / ( \ 1 ) = \ SSM / \ DFM $$"),br(),
    paste0("L' \\( \ MSE \\) corrispondente (errore quadrato medio) \\( = \\sum \ ( y_i - \\hat y_i \ )^2 / ( \ n - \ 2) = \ SSE / \ DFE \\), la stima della varianza attorno alla linea di regressione della popolazione \\( \\sigma^2 \\)."),
    
    br(),br(),br(),
    h4(strong("Anova del modello"))
      
    )
  })
  
  
  output$r_anova <- renderUI({
    if (!input$check_anova) return()
    helpText("anova(lm(var_y ~ var_x))")
    
  })
  
  
  output$anova <- renderTable({
    anova(lm(var$y~var$x))
  }, 
  striped = TRUE,
  hover = TRUE,
  align = "c")
  
  
  ######### BIC e BF #########
  
  
  ### info BIC ###
  output$info_BIC <- renderUI({
    BIC_1 <- BIC(lm(var$y~var$x))
    
    withMathJax(
    h3(strong("Bayesian Information Criterion (BIC)")),
    paste0("Il Bayesian Information Criterion (BIC; Schwarz, 1978) è un criterio utile per la selezione di modelli."),br(),
    paste0("Dato un modello \\( \ M \\), viene definito con $$  \ BIC(M)  =  \ k \\cdot \ ln(n) - \ 2 \ ln \ ( \ L \ ) $$ "),br(),
    paste0("in cui \\( \ L \\) è il valore di massima verosimiglianza del modello, \\( \ k \\) il numero di parametri e \\( \ n \\) il numero di osservazioni."),br(),
    paste0("In generale, migliore è il modello più basso risulta essere il valore di \\( \ BIC \\), pertanto possiamo utilizzare tale statistica per scegliere il modello migliore."),
    br(),br(),
    span(strong("Il \\( \ BIC \\) di questo modello è: "),
    paste0(round(BIC_1,3)))
    )
    
  })
  
  output$r_BIC <- renderUI({
    if (!input$check_BIC) return()
    helpText("BIC(lm(var_y ~ var_x))")
    
  })
  
  ### info BF ###
  output$info_BF <- renderUI({
    BIC_1 <- BIC(lm(var$y~var$x))
    BIC_2 <- input$BIC_2
    
    withMathJax(
      br(),br(),
      h3(strong("Bayesian Factor (BF)")),
      paste0("Una misura di quantificazione dell'evidenza di un'ipotesi rispetto ad un'altra è il Bayes Factor."),br(),
      paste0("Nel caso classico con \\( \ H_0 \\) e \\( \ H_1 \\) contrapposte: $$ \ BF_10 = \ f \ ( \ x | \ H_1 \ ) \\cdot \ f \ ( \ x | \ H_0 \ ) $$"),br(),
      paste("Quando il valore supera 1 allora è più evidente H1. In generale BF consente di valutare quanto un'ipotesi (o un modello) sia più evidente di un'altra/o."),
      br(), br(),
      paste0("\\( \ BIC_1 \\) = ", round(BIC_1)), br(),
      paste0("\\( \ BIC_2 \\) = ")
    )
  })
  
  ## calcolo BF ##
  output$BF<- renderUI({
    BIC_1 <- BIC(lm(var$y~var$x))
    BIC_2 <- input$BIC_2
    BF <- exp((BIC_1-BIC_2)/2)
    
    withMathJax(
      br(),br(),
      h4("\\( \ BF \\) = ", round(BF,5))
    )
  })
  
  output$r_BF <- renderUI({
    if (!input$check_BF) return()
    helpText("library(BayesFactor)",br(),"exp( (BIC_1 - BIC_2) / 2)")
    
  })
  
})

shinyApp(ui, server)