library(shiny)
library(shinyjs)
library('V8')
library(ggplot2)
library(cowplot)
library(ggpubr)
library(MASS)
library(mvtnorm)
library(bde)

ui <- fluidPage(
  tags$style("#no_data {font-size:6px;
               color:white }"),
  tags$style("#invalid_place1 {font-size:1px;
               color:white }"),
  tags$style("#invalid_place2 {font-size:1px;
               color:white }"),
  tags$style("#invalid_psum {font-size:1px;
               color:white }"),
  tags$style("#invalid_xi {font-size:1px;
               color:white }"),
  tags$style("#invalid_Z1Range {font-size:1px;
               color:white }"),
  tags$style("#invalid_Z2Range {font-size:1px;
               color:white }"),
  tags$style("#invalid_Z3Range {font-size:1px;
               color:white }"),
  tags$style("#invalid_Z4Range {font-size:1px;
               color:white }"),
  tags$style("#invalid_Z5Range {font-size:1px;
               color:white }"),
  tags$style("#invalid_Z6Range {font-size:1px;
               color:white }"),
  # tags$style("#invalid_gaussrho {font-size:1px;
  #              color:white }"),
  # tags$style("#invalid_trho {font-size:1px;
  #              color:white }"),
  # tags$style("#invalid_gaussphi {font-size:1px;
  #              color:white }"),
  # tags$style("#invalid_tphi {font-size:1px;
  #              color:white }"),
  # tags$style("#invalid_tdf {font-size:1px;
  #              color:white }"),
  tags$style(type="text/css", "
             #loadmessage {
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #d0e8fb;
               z-index: 105;
             },
          "),
  tags$style(type="text/css", "
             #errormessage {
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #ff7355;
               z-index: 105;
             },
          "),
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  shinyjs::useShinyjs(),
  # shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }",
  #                        functions = c("refresh")),
  # shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  
  sidebarLayout(
    sidebarPanel(width=3, 
                 style = "overflow-y:scroll; max-height: 950px; position:relative;",
                 fluidRow(column(12, style= "margin-bottom: -20px", 
                                 h4(tags$strong("Initialize Prior Specification:")))),         
                 tags$hr(style="border-color: black;border:solid 1px black"), 
                 h5(tags$strong("Download Reference Materials:")),
                 fluidRow(column(6, style = "margin-bottom: 10px",
                                 downloadButton("userGuide", label="User Guide")),
                          column(6, style = "margin-bottom: 10px",
                                 # downloadButton("videoTutorial", label="Video Tutorial")
                          )),
                 conditionalPanel(condition = "input.update == '0'",
                                  fluidRow(
                                    column(1, style = "margin-top: -8px;", textOutput("invalid_place1"))
                                  ),
                                  fluidRow(
                                    column(1, style = "margin-top: -8px;", textOutput("invalid_place2")),
                                  )),
                 conditionalPanel(condition = "input.update > '0'",
                   fluidRow(
                   column(1, style = "margin-top: -8px;", textOutput("invalid_psum")),
                   column(1, style = "margin-top: -8px;", textOutput("invalid_xi")),
                   column(1, style = "margin-top: -8px;", textOutput("invalid_Z1Range")),
                   column(1, style = "margin-top: -8px;", textOutput("invalid_Z2Range")),
                   column(1, style = "margin-top: -8px;", textOutput("invalid_Z3Range")),
                   column(1, style = "margin-top: -8px;", textOutput("invalid_Z4Range")),
                   column(1, style = "margin-top: -8px;", textOutput("invalid_Z5Range")),
                   column(1, style = "margin-top: -8px;", textOutput("invalid_Z6Range"))
                 ),
                 fluidRow(
                   column(1, style = "margin-top: -8px;", textOutput("invalid_trho")),
                 )),
                 # fluidRow(column(12, style= "margin-bottom: -20px; margin-top: 5px", 
                 #                 h4(tags$strong("Initialize Prior Specification:")))),         
                 # tags$hr(style="border-color: black;border:solid 1px black"),
                 # fluidRow(column(12, style = "margin-top: -8px; margin-bottom: -10px",
                 #                 sliderInput("ncat", "Number of Categories",
                 #                             3,7,5, step=1))),
                 fluidRow(column(12, style = "margin-top: 6px; margin-bottom: -10px",
                                    sliderInput("ncat", "Number of Categories",
                                                      3,7,5, step=1))),
                 conditionalPanel(condition="output.invalid_psum == '1' && input.update > '0'",
                                  tags$div("Error: Estimates (> 0) must sum to 1.",id="errormessage")),
                 fluidRow(column(12, style= "margin-bottom: -5px; margin-top: -1px", 
                                 h5(tags$strong("Estimates for Categorical Probabilities")))),
                 fluidRow(
                   column(4, style= "margin-top: 5px", 
                          textInput(inputId = "phat1", label = HTML(paste0(tags$em("p"),tags$sub("1"))),
                                    placeholder = "Enter")),
                   column(4, style= "margin-top: 5px", 
                          textInput(inputId = "phat2", label = HTML(paste0(tags$em("p"),tags$sub("2"))),
                                    placeholder = "Enter")),
                   column(4, style= "margin-top: 5px", 
                          textInput(inputId = "phat3", label = HTML(paste0(tags$em("p"),tags$sub("3"))),
                                    placeholder = "Enter"))),
                 fluidRow(
                   conditionalPanel(
                     condition = "input.ncat == '4' || input.ncat == '5'
                   || input.ncat == '6' || input.ncat == '7'",
                   column(4, style= "margin-top: 0px", 
                          textInput(inputId = "phat4", label = HTML(paste0(tags$em("p"),tags$sub("4"))),
                                    placeholder = "Enter"))),
                   conditionalPanel(
                     condition = "input.ncat == '5'
                   || input.ncat == '6' || input.ncat == '7'",
                   column(4, style= "margin-top: 0px", 
                          textInput(inputId = "phat5", label = HTML(paste0(tags$em("p"),tags$sub("5"))),
                                    placeholder = "Enter"))),
                   conditionalPanel(
                     condition = "input.ncat == '6' || input.ncat == '7'",
                   column(4, style= "margin-top: 0px", 
                          textInput(inputId = "phat6", label = HTML(paste0(tags$em("p"),tags$sub("6"))),
                                    placeholder = "Enter")))),
                 fluidRow(
                   conditionalPanel(
                     condition = "input.ncat == '7'",
                     column(4, style= "margin-top: 0px", 
                            textInput(inputId = "phat7", label = HTML(paste0(tags$em("p"),tags$sub("7"))),
                                      placeholder = "Enter")))),
                 # fluidRow(
                 #   column(12, style = "margin-left: 0px; margin-top: -2px",
                 #          checkboxInput("medp", HTML(paste0("Show ",tags$strong(tags$em("p")), "estimates on lower plots")),
                 #                        value = TRUE))),
                 fluidRow(
                   column(12, style = "margin-left: 0px; margin-top: 0px",
                          radioButtons("mode", h5(tags$strong("Mode")), choices = c("Exploration", "Calibration"),
                                       inline = TRUE))),
                 fluidRow(column(12, style= "margin-bottom: -20px; margin-top: 5px", 
                                 h4(tags$strong("Marginal Prior Specification:")))),  
                 tags$hr(style="border-color: black;border:solid 1px black"),
                 conditionalPanel(condition="output.invalid_xi == '1' && input.update > '0'",
                                  fluidRow(
                                    column(12, style = "margin-top: -10px; margin-bottom: 10px",
                                  tags$div("Error: Invalid \u03BE value.",id="errormessage")))),
                 fluidRow(
                   column(12, style= "margin-top: -8px", 
                          textInput(inputId = "quantZ", label = "Beta Quantile (\u03BE)",
                                    placeholder = "Enter value in (0,1) excluding 0.5"))),
                 # fluidRow(
                 #   column(12, style = "margin-left: 0px; margin-top: -2px",
                 #          checkboxInput("medz", HTML(paste0("Show ",tags$strong(tags$em("Z")), "medians on upper left plots")),
                 #                        value = TRUE))),
                 conditionalPanel(condition="output.invalid_Z1Range == '1' && input.update > '0'",
                                  fluidRow(column(12, style= "margin-bottom: 1px", 
                                                  tags$div(HTML(paste0("Error: Invalid quantiles for ",tags$em("Z"),tags$sub("1"))),id="errormessage")))),
                 fluidRow(
                   column(6, style= "margin-top: 5px", 
                          uiOutput("Z1medUI")),
                   column(6, style= "margin-top: 5px", 
                          textInput(inputId = "Z1xi", label = HTML(paste0("\u03BE-Quantile: ",tags$em("Z"),tags$sub("1"))),
                                    placeholder = "Enter value"))),
                 conditionalPanel(condition="output.invalid_Z2Range == '1' && input.update > '0'",
                                  fluidRow(column(12, style= "margin-bottom: 1px", 
                                                  tags$div(HTML(paste0("Error: Invalid quantiles for ",tags$em("Z"),tags$sub("2"))),id="errormessage")))),
                   fluidRow(
                     column(6, style= "margin-top: 5px", 
                            uiOutput("Z2medUI")),
                     column(6, style= "margin-top: 5px", 
                            textInput(inputId = "Z2xi", label = HTML(paste0("\u03BE-Quantile: ",tags$em("Z"),tags$sub("2"))),
                                      placeholder = "Enter value"))),
                 conditionalPanel(
                   condition = "input.ncat == '4' || input.ncat == '5'
                   || input.ncat == '6' || input.ncat == '7'",
                   conditionalPanel(condition="output.invalid_Z3Range == '1' && input.update > '0'",
                                    fluidRow(column(12, style= "margin-bottom: 1px", 
                                                    tags$div(HTML(paste0("Error: Invalid quantiles for ",tags$em("Z"),tags$sub("3"))),id="errormessage")))),
                   fluidRow(
                     column(6, style= "margin-top: 5px", 
                            uiOutput("Z3medUI")),
                     column(6, style= "margin-top: 5px", 
                            textInput(inputId = "Z3xi", label = HTML(paste0("\u03BE-Quantile: ",tags$em("Z"),tags$sub("3"))),
                                      placeholder = "Enter value")))),
                 conditionalPanel(
                   condition = "input.ncat == '5'
                   || input.ncat == '6' || input.ncat == '7'",
                   conditionalPanel(condition="output.invalid_Z4Range == '1' && input.update > '0'",
                                    fluidRow(column(12, style= "margin-bottom: 1px", 
                                                    tags$div(HTML(paste0("Error: Invalid quantiles for ",tags$em("Z"),tags$sub("4"))),id="errormessage")))),
                   fluidRow(
                     column(6, style= "margin-top: 5px", 
                            uiOutput("Z4medUI")),
                     column(6, style= "margin-top: 5px", 
                            textInput(inputId = "Z4xi", label = HTML(paste0("\u03BE-Quantile: ",tags$em("Z"),tags$sub("4"))),
                                      placeholder = "Enter value")))),
                 conditionalPanel(
                   condition = "input.ncat == '6' || input.ncat == '7'",
                   conditionalPanel(condition="output.invalid_Z5Range == '1' && input.update > '0'",
                                    fluidRow(column(12, style= "margin-bottom: 1px", 
                                                    tags$div(HTML(paste0("Error: Invalid quantiles for ",tags$em("Z"),tags$sub("5"))),id="errormessage")))),
                   fluidRow(
                     column(6, style= "margin-top: 5px", 
                            uiOutput("Z5medUI")),
                     column(6, style= "margin-top: 5px", 
                            textInput(inputId = "Z5xi", label = HTML(paste0("\u03BE-Quantile: ",tags$em("Z"),tags$sub("5"))),
                                      placeholder = "Enter value")))),
                 conditionalPanel(
                   condition = "input.ncat == '7'",
                   conditionalPanel(condition="output.invalid_Z6Range == '1' && input.update > '0'",
                                    fluidRow(column(12, style= "margin-bottom: 1px", 
                                                    tags$div(HTML(paste0("Error: Invalid quantiles for ",tags$em("Z"),tags$sub("6"))),id="errormessage")))),
                   fluidRow(
                     column(6, style= "margin-top: 5px", 
                            uiOutput("Z6medUI")),
                     column(6, style= "margin-top: 5px", 
                            textInput(inputId = "Z6xi", label = HTML(paste0("\u03BE-Quantile: ",tags$em("Z"),tags$sub("6"))),
                                      placeholder = "Enter value"))))
                 # fluidRow(column(12, style= "margin-bottom: -20px; margin-top: 5px", 
                 #                 h4(tags$strong("Copula Specification:")))),  
                 # tags$hr(style="border-color: black;border:solid 1px black"),
                 # fluidRow(
                 #   column(12, style = "margin-left: 0px; margin-top: 0px",
                 #          radioButtons("cop_model", tags$strong("Model"), choices = c("Independence", "Gaussian", "t"),
                 #                       inline = TRUE))),
                 # conditionalPanel(
                 #   condition = "input.cop_model == 'Gaussian'",
                 #   fluidRow(
                 #     column(12, style= "margin-top: 5px", 
                 #            radioButtons("gaussR", tags$strong("Correlation Structure"), choices = c("Symmetric", "AR(1)"),
                 #                         inline = TRUE)))),
                 # conditionalPanel(
                 #   condition = "input.cop_model == 'Gaussian' && input.gaussR == 'Symmetric'",
                 #   conditionalPanel(condition="output.invalid_gaussrho == '1' && input.update > '0'",
                 #                    fluidRow(column(12, style= "margin-bottom: 2px", 
                 #                                    tags$div(HTML(paste0("Error: Invalid value for \u03C1.")),id="errormessage")))),
                 #   fluidRow(
                 #     column(12, style= "margin-top: 5px", 
                 #            uiOutput("gaussrhoUI")))),
                 # conditionalPanel(
                 #   condition = "input.cop_model == 'Gaussian' && input.gaussR == 'AR(1)'",
                 #   conditionalPanel(condition="output.invalid_gaussphi == '1' && input.update > '0'",
                 #                    fluidRow(column(12, style= "margin-bottom: 2px", 
                 #                                    tags$div(HTML(paste0("Error: Invalid value for \u03D5.")),id="errormessage")))),
                 #   fluidRow(
                 #     column(12, style= "margin-top: 5px", 
                 #            textInput(inputId = "gaussphi", label = HTML(paste0("Correlation Parameter (\u03D5)")),
                 #                      placeholder = "Enter value in (-1,1)")))),
                 # conditionalPanel(
                 #   condition = "input.cop_model == 't'",
                 #   conditionalPanel(
                 #     condition = "input.cop_model == 't'",
                 #     conditionalPanel(condition="output.invalid_tdf == '1' && input.update > '0'",
                 #                      fluidRow(column(12, style= "margin-bottom: 2px", 
                 #                                      tags$div(HTML(paste0("Error: Invalid value for \u03BD.")),id="errormessage"))))),
                 #   fluidRow(
                 #     column(12, style= "margin-top: 5px", 
                 #            textInput(inputId = "tdf", label = HTML(paste0("Degrees of Freedom (\u03BD)")),
                 #                      placeholder = "Enter integer \u2265 1"))),
                 #   fluidRow(
                 #     column(12, style= "margin-top: 5px", 
                 #            radioButtons("tR", tags$strong("Correlation Structure"), choices = c("Symmetric", "AR(1)"),
                 #                         inline = TRUE)))),
                 # conditionalPanel(
                 #   condition = "input.cop_model == 't' && input.tR == 'Symmetric'",
                 #   conditionalPanel(condition="output.invalid_trho == '1' && input.update > '0'",
                 #                    fluidRow(column(12, style= "margin-bottom: 2px",
                 #                                    tags$div(HTML(paste0("Error: Invalid value for \u03C1.")),id="errormessage")))),
                 #   fluidRow(
                 #     column(12, style= "margin-top: 5px", 
                 #            uiOutput("trhoUI")))),
                 # conditionalPanel(
                 #   condition = "input.cop_model == 't' && input.tR == 'AR(1)'",
                 #   conditionalPanel(condition="output.invalid_tphi == '1' && input.update > '0'",
                 #                    fluidRow(column(12, style= "margin-bottom: 2px",
                 #                                    tags$div(HTML(paste0("Error: Invalid value for \u03D5.")),id="errormessage")))),
                 #   fluidRow(
                 #     column(12, style= "margin-top: 5px", 
                 #            textInput(inputId = "tphi", label = HTML(paste0("Correlation Parameter (\u03D5)")),
                 #                      placeholder = "Enter value in (-1,1)")))),
                 
    ),
    mainPanel(
      fluidRow(style= "margin-bottom: 10px;",
               column(2, align="left", style = "margin-top: 25px; margin-bottom: 10px", actionButton(inputId = "update", label = "Update",
                                                                                                     style="color: #2E8B57; background-color: #fff")),
               column(8, offset=0, 
                      titlePanel(h1("Prior Elicitation for Ordinal Models", align="center"), windowTitle = "Ordinal Prior App")),
               
               column(2, align="right", style = "margin-top: 25px; margin-bottom: 10px", actionButton(inputId = "reset", label = "Reset",
                                                                                                      style="color: #8B1A1A; background-color: #fff"))
      ),
      tabsetPanel(id = "tabs",
                  tabPanel("Prior Visualization",
                           conditionalPanel(condition = "input.update == 0",
                                            fluidRow(
                                              column(12, style = "margin-top: 7px;", 
                                                     wellPanel(
                                                       h4(tags$strong("Instructions to Elicit Joint Prior:")),
                                                       fluidRow(
                                                         column(12, style = "margin-left: 10px",
                                                                h5("1. Complete the Initialize Prior Specification section and click Update to generate a blank grid of prior plots.", br(),
                                                                   "2. Complete the Marginal Prior Specification section to populate the plots, adjusting as needed.")))))),
                                            conditionalPanel(condition="$('html').hasClass('shiny-busy') && input.update == 0",
                                                             tags$div("Loading app. This make take a few seconds. Thank you for your patience.",id="loadmessage"))),
                           conditionalPanel(condition = "input.update > 0",
                                            fluidRow(
                                              column(12, style = "margin-top: 7px;", 
                                                     wellPanel(
                                                       fluidRow(
                                                        column(10, h4(tags$strong("Instructions to Elicit Joint Prior:"))),
                                                        column(2, downloadButton('downloadData', 'Save Session'))
                                                       ),
                                                       fluidRow(
                                                         column(10, style = "margin-left: 20px", 
                                                                h5("1. Complete the Marginal Prior Specification section in stages, working from top to bottom.", br(),
                                                                       "2. Press the Update button to have the changes from the sidebar panel reflected in the plots.", br(),
                                                                       HTML(paste0("3. Tweak the inputs on the sidebar panel until satisfied with the marginal priors for ",tags$strong(tags$em("Z")),", ",tags$strong(tags$em("p")),", and ", tags$em("\u03B8"),"."))))),
                                                       # fluidRow(
                                                       #   column(12, style = "margin-top: 5px", 
                                                       #          checkboxInput("customize", tags$strong("Show Plot Customization Options")))),
                                                       conditionalPanel(condition = "input.customize == true",
                                                                        fluidRow(column(3, style = "margin-top: 5px",
                                                                                        textInput(inputId = "group1", label = "Label for Reference",
                                                                                                  value = "", placeholder="Reference")),
                                                                                 column(3, style = "margin-top: 5px",
                                                                                        textInput(inputId = "group2", label = "Label for Comparison",
                                                                                                  value = "", placeholder="Comparison")),
                                                                                 column(3, style = "margin-top: 5px",
                                                                                        textInput(inputId = "responseVar", label = "Label for Response",
                                                                                                  value = "", placeholder="Response")),
                                                                                 column(3, style = "margin-top: 5px",
                                                                                        textInput(inputId = "bins", label = "Binwidth",
                                                                                                  value = "", placeholder="Enter Binwidth")))
                                                       ))
                                              )
                                            ),
                                            conditionalPanel(condition="$('html').hasClass('shiny-busy') && input.update > 0",
                                                             tags$div("Loading plots. Please note this may take up to a minute. Thank you for your patience.",id="loadmessage")),
                                                             fluidRow(
                                                               column(12, offset =0, style = "margin-top: 15px;", 
                                                                      plotOutput("plotfull", width = "100%", height = "750px"))
                                                             )))
      )
      
    )
  )
)

server <- function(input, output) {
  
  # observeEvent(input$reset, {
  #   shinyjs::js$refresh()
  # })
  
  observeEvent(input$reset, {
    refresh()
  })
  
  output$userGuide <- downloadHandler(
    filename = "ordinal_priors_user_guide.pdf",
    content = function(file) {
      file.copy("www/ordinal_priors_guide.pdf", file)
    })
  
  ncat <- reactive({as.numeric(input$ncat)})
  mode <- reactive({tolower(input$mode)})
  phat1 <- reactive({as.numeric(input$phat1)})
  phat2 <- reactive({as.numeric(input$phat2)})
  phat3 <- reactive({as.numeric(input$phat3)})
  phat4 <- reactive({as.numeric(input$phat4)})
  phat5 <- reactive({as.numeric(input$phat5)})
  phat6 <- reactive({as.numeric(input$phat6)})
  phat7 <- reactive({as.numeric(input$phat7)})
  phats <- reactive({as.numeric(head(c(phat1(), phat2(), phat3(), phat4(), phat5(), phat6(), phat7()), ncat()))})
  
  medp <- reactive(0)
  medz <- reactive(0)
  
  # cop_model <- reactive({tolower(input$cop_model)})
  # gaussR <- reactive({tolower(input$gaussR)})
  # gaussrho <- reactive({as.numeric(input$gaussrho)})
  # gaussphi <- reactive({as.numeric(input$gaussphi)})
  # tR <- reactive({tolower(input$tR)})
  # tdf <- reactive({as.numeric(input$tdf)})
  # trho <- reactive({as.numeric(input$trho)})
  # tphi <- reactive({as.numeric(input$tphi)})
  # 
  # observeEvent(0, {
  #     lowerbound_temp <- as.numeric(substr(as.character(-(ncat()-2)^{-1}), 1, 6))
  #     output$gaussrhoUI <- renderUI({textInput(inputId = "gaussrho", label = HTML(paste0("Correlation Parameter (\u03C1)")),
  #                                              placeholder = HTML(paste0("Enter value in (",lowerbound_temp, ", 1)")))})
  #     output$trhoUI <- renderUI({textInput(inputId = "trho", label = HTML(paste0("Correlation Parameter (\u03C1)")),
  #                                              placeholder = HTML(paste0("Enter value in (",lowerbound_temp, ", 1)")))})
  # }, ignoreNULL = FALSE)
  
  n_sim <- reactive({
    if(mode() == "exploration"){
      7500
    }
    else if (mode() == "calibration"){
      40000
    }
  })
  
  zhats <- reactive({head(phats(), length(phats())-1)/(1- cumsum(c(0,head(phats(), length(phats())-2))))})
  
  check_psum <- reactive({
    if (sum(is.na(phats()))>0) {1}
    else if (sum(phats() <= 0)>0) {1}
    else if (sum(phats() >= 1)>0) {1}
    else if (round(sum(phats()),4) == 1) {0}
    else {1}
  })
  output$phats <- renderText({check_psum()})
  
  output$invalid_place1 <- renderText({1})
  output$invalid_place2 <- renderText({2})
  
  xi <- reactive({as.numeric(input$quantZ)})
  
  Z1_med <- reactive({
    if (is.null(input$Z1med)){NA}
    else if (check_psum()){NA}
    else if (input$Z1med == ""){zhats()[1]}
    else {as.numeric(input$Z1med)} 
  })
  Z1_xi <- reactive({as.numeric(input$Z1xi)})
  
  Z2_med <- reactive({
    if (is.null(input$Z2med)){NA}
    else if (check_psum()){NA}
    else if (input$Z2med == ""){zhats()[2]}
    else {as.numeric(input$Z2med)} 
  })
  Z2_xi <- reactive({as.numeric(input$Z2xi)})
  
  Z3_med <- reactive({
    if (is.null(input$Z3med)){NA}
    else if (check_psum()){NA}
    else if (input$Z3med == ""){zhats()[3]}
    else {as.numeric(input$Z3med)} 
  })
  Z3_xi <- reactive({as.numeric(input$Z3xi)})
  
  Z4_med <- reactive({
    if (is.null(input$Z4med)){NA}
    else if (check_psum()){NA}
    else if (input$Z4med == ""){zhats()[4]}
    else {as.numeric(input$Z4med)} 
  })
  Z4_xi <- reactive({as.numeric(input$Z4xi)})
  
  Z5_med <- reactive({
    if (is.null(input$Z5med)){NA}
    else if (check_psum()){NA}
    else if (input$Z5med == ""){zhats()[5]}
    else {as.numeric(input$Z5med)} 
  })
  Z5_xi <- reactive({as.numeric(input$Z5xi)})
  
  Z6_med <- reactive({
    if (is.null(input$Z6med)){NA}
    else if (check_psum()){NA}
    else if (input$Z6med == ""){zhats()[6]}
    else {as.numeric(input$Z6med)} 
  })
  Z6_xi <- reactive({as.numeric(input$Z6xi)})
  
  check_xi <- reactive({
    if (input$quantZ == "" & input$Z1xi == "" & input$Z2xi == "" & !(input$Z3xi != "" & ncat() > 3)
        & !(input$Z4xi != "" & ncat() > 4) & !(input$Z5xi != "" & ncat() > 5) & !(input$Z6xi != "" & ncat() > 6)) {0}
    else if (is.na(xi())) {1}
    else if (xi() <= 0) {1}
    else if (xi() >= 1) {1}
    else if (xi() == 0.5) {1}
    else {0}
  })
  
  # check_gaussrho <- reactive({
  #   if (is.na(gaussrho())) {1}
  #   else if (max(gaussrho() >= 1, gaussrho() <= -(ncat()-2)^{-1})) {1}
  #   else {0}
  # })
  # 
  # check_gaussphi <- reactive({
  #   if (is.na(gaussphi())) {1}
  #   else if (max(gaussphi() >= 1, gaussphi() <= -1)) {1}
  #   else {0}
  # })
  # 
  # check_tdf <- reactive({
  #   if (is.na(tdf())) {1}
  #   else if (!is.finite(as.numeric(tdf()))) {1}
  #   else if (max(tdf() < 1, tdf()%%1 != 0)) {1}
  #   else {0}
  # })
  # 
  # check_trho <- reactive({
  #   if (is.na(trho())) {1}
  #   else if (max(trho() >= 1, trho() <= -(ncat()-2)^{-1})) {1}
  #   else {0}
  # })
  # 
  # check_tphi <- reactive({
  #   if (is.na(tphi())) {1}
  #   else if (max(tphi() >= 1, tphi() <= -1)) {1}
  #   else {0}
  # })
  
  check_Z1Range <- reactive({
    if (input$Z1med == "" & input$Z1xi == "")
    {if (input$Z2med == "" & input$Z2xi == "") {
        if (ncat() == 3) {-1}
        else if (input$Z3med == "" & input$Z3xi == "") {
          if (ncat() == 4) {-1}
          else if (input$Z4med == "" & input$Z4xi == "") {
            if (ncat() == 5) {-1}
            else if (input$Z5med == "" & input$Z5xi == "") {
              if (ncat() == 6) {-1}
              else if (input$Z6med == "" & input$Z6xi == "") {-1}
              else {1}
            }
            else {1}
          }
          else {1}
        }
        else {1}
      }
        else {1}} 
    else if (check_xi())
    {-1}
    else if (is.na(Z1_med()) | is.na(as.numeric(Z1_xi())))
    {1}
    else if (Z1_med() <= 0 | Z1_xi() <= 0)
    {1}
    else if (Z1_med() >= 1 | Z1_xi() >= 1)
    {1}
    else if (xi() > 0.5)
    {as.numeric(Z1_med() > Z1_xi())}
    else
    {as.numeric(Z1_med() < Z1_xi())}
  })
  
  check_Z2Range <- reactive({
    if (input$Z2med == "" & input$Z2xi == "")
    {if (ncat() == 3) {-1}
      else if (input$Z3med == "" & input$Z3xi == "") {
        if (ncat() == 4) {-1}
        else if (input$Z4med == "" & input$Z4xi == "") {
          if (ncat() == 5) {-1}
          else if (input$Z5med == "" & input$Z5xi == "") {
            if (ncat() == 6) {-1}
            else if (input$Z6med == "" & input$Z6xi == "") {-1}
            else {1}
          }
          else {1}
        }
        else {1}
      }
      else {1}}
    else if (check_xi())
    {-1}
    else if (is.na(Z2_med()) | is.na(as.numeric(Z2_xi())))
    {1}
    else if (Z2_med() <= 0 | Z2_xi() <= 0)
    {1}
    else if (Z2_med() >= 1 | Z2_xi() >= 1)
    {1}
    else if (xi() > 0.5)
    {as.numeric(Z2_med() > Z2_xi())}
    else
    {as.numeric(Z2_med() < Z2_xi())}
  })
  
  check_Z3Range <- reactive({
    if (input$Z3med == "" & input$Z3xi == "") {
      if (ncat() == 4) {-1}
      else if (input$Z4med == "" & input$Z4xi == "") {
        if (ncat() == 5) {-1}
        else if (input$Z5med == "" & input$Z5xi == "") {
          if (ncat() == 6) {-1}
          else if (input$Z6med == "" & input$Z6xi == "") {-1}
          else {1}
        }
        else {1}
      }
      else {1}
    }
    else if (check_xi())
    {-1}
    else if (is.na(Z3_med()) | is.na(as.numeric(Z3_xi())))
    {1}
    else if (Z3_med() <= 0 | Z3_xi() <= 0)
    {1}
    else if (Z3_med() >= 1 | Z3_xi() >= 1)
    {1}
    else if (xi() > 0.5)
    {as.numeric(Z3_med() > Z3_xi())}
    else
    {as.numeric(Z3_med() < Z3_xi())}
  })
  
  check_Z4Range <- reactive({
    if (input$Z4med == "" & input$Z4xi == "") {
      if (ncat() == 5) {-1}
      else if (input$Z5med == "" & input$Z5xi == "") {
        if (ncat() == 6) {-1}
        else if (input$Z6med == "" & input$Z6xi == "") {-1}
        else {1}
      }
      else {1}
    }
    else if (check_xi())
    {-1}
    else if (is.na(Z4_med()) | is.na(as.numeric(Z4_xi())))
    {1}
    else if (Z4_med() <= 0 | Z4_xi() <= 0)
    {1}
    else if (Z4_med() >= 1 | Z4_xi() >= 1)
    {1}
    else if (xi() > 0.5)
    {as.numeric(Z4_med() > Z4_xi())}
    else
    {as.numeric(Z4_med() < Z4_xi())}
  })
  
  check_Z5Range <- reactive({
    if (input$Z5med == "" & input$Z5xi == "") {
      if (ncat() == 6) {-1}
      else if (input$Z6med == "" & input$Z6xi == "") {-1}
      else {1}
    }
    else if (check_xi())
    {-1}
    else if (is.na(Z5_med()) | is.na(as.numeric(Z5_xi())))
    {1}
    else if (Z5_med() <= 0 | Z5_xi() <= 0)
    {1}
    else if (Z5_med() >= 1 | Z5_xi() >= 1)
    {1}
    else if (xi() > 0.5)
    {as.numeric(Z5_med() > Z5_xi())}
    else
    {as.numeric(Z5_med() < Z5_xi())}
  })
  
  check_Z6Range <- reactive({
    if (input$Z6med == "" & input$Z6xi == "") {-1}
    else if (check_xi())
    {-1}
    else if (is.na(Z6_med()) | is.na(as.numeric(Z6_xi())))
    {1}
    else if (Z6_med() <= 0 | Z6_xi() <= 0)
    {1}
    else if (Z6_med() >= 1 | Z6_xi() >= 1)
    {1}
    else if (xi() > 0.5)
    {as.numeric(Z6_med() > Z6_xi())}
    else
    {as.numeric(Z6_med() < Z6_xi())}
  })
  
  anyIssues <- reactive({max(check_xi(), check_Z1Range() == 1, check_Z2Range() == 1, check_Z3Range() == 1,
                             check_Z4Range() == 1, check_Z5Range() == 1, check_Z6Range() == 1, check_psum())})
  
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  last_phats <- reactiveVal()
  
  observeEvent(input$update, {
    new_phats <- phats()
    last_phats(new_phats)
  })
  
  last_phats <- reactiveVal()
  
  observeEvent(input$update, {
    new_phats <- phats()
    last_phats(new_phats)
  }, ignoreNULL = FALSE)
  
  phatsChange <- reactive({
    if (is.na(min(last_phats() == phats()))) {0}
    else {1 - min(last_phats() == phats())}})
  output$phatsChange <- renderText({phatsChange()})
  
  last_mode <- reactiveVal()
  
  observeEvent(0, {
    new_mode <- mode()
    last_mode(new_mode)
  }, ignoreNULL = FALSE)
  
  last_ncat <- reactiveVal()
  
  observeEvent(0, {
    new_ncat <- ncat()
    last_ncat(new_ncat)
  }, ignoreNULL = FALSE)
  
  last_xi <- reactiveVal()
  
  observeEvent(0, {
    new_xi <- xi()
    last_xi(new_xi)
  }, ignoreNULL = FALSE)
  
  uChange <- reactive({1 - min(c(last_ncat(), last_mode()) == 
                                 c(ncat(), mode()))})
  output$uChange <- renderText({uChange()})
  
  xiChange <- reactive({
    if (min(is.na(last_xi()), !is.na(xi()))) {1}
    else if (min(!is.na(last_xi()), is.na(xi()))) {1}
    else if (is.na(last_xi() == xi())) {0}
    else {1 - as.numeric(last_xi() == xi())}})
  
  # output$c9 <- renderText({last_gaussrho()})
  # output$c10 <- renderText({last_trho()})
  # 
  # output$c11 <- renderText({gaussrhoChange()})
  # output$c12 <- renderText({trhoChange()})
  
 uRun <- reactiveVal(0)
  
  observeEvent(input$update, {
    if (!anyIssues()){
      if (max(uChange())){
        uIncrease <- uRun() + 1
        uRun(uIncrease)
      }
    }
    })
  
  last_medp <- reactiveVal()
  
  observeEvent(0, {
    new_medp <- medp()
    last_medp(new_medp)
  }, ignoreNULL = FALSE)
  
  last_medz <- reactiveVal()
  
  observeEvent(0, {
    new_medz <- medz()
    last_medz(new_medz)
  }, ignoreNULL = FALSE)
  
  medsChange <- reactive({1 - min(c(last_medp(), last_medz()) == c(medp(), medz()))})
  
  plotRun <- reactiveVal(0)
  
  observeEvent(input$update, {
    if (!anyIssues()){
      plotIncrease <- plotRun() + 1
      plotRun(plotIncrease)
    }
  })
  output$plotRun <- renderText({plotRun()})
  
  postRun <- reactiveVal(0)
  
  observeEvent(input$update, {
    if (!anyIssues()){
      if(max(max(uChange()), 
             medsChange(), xiChange(), Z1Change(), Z2Change(), 
             Z3Change() & ncat() > 3, Z4Change() & ncat() > 4,
             Z5Change() & ncat() > 5, Z6Change() & ncat() > 6)){
        postIncrease <- postRun() + 1
        postRun(postIncrease)
      }
    }
  })
  output$postRun <- renderText({postRun()})
  
  obs <- eventReactive(uRun(), {
      cop_mat <- diag(ncat() - 1)
      pnorm(mvrnorm(n = n_sim(), mu = rep(0, ncat() - 1), Sigma = cop_mat))
    
  }, ignoreNULL = FALSE)
  
  observeEvent(uRun(), {
    new_ncat <- ncat()
    last_ncat(new_ncat)

    new_mode <- mode()
    last_mode(new_mode)
    
  })
  
  observeEvent(postRun(), {
    new_xi <- xi()
    last_xi(new_xi)
    
  })

  last_Z1_med <- reactiveVal()

  observeEvent(0, {
    new_Z1_med <- Z1_med()
    last_Z1_med(new_Z1_med)
  }, ignoreNULL = FALSE)

  last_Z1_xi <- reactiveVal()

  observeEvent(0, {
    new_Z1_xi <- Z1_xi()
    last_Z1_xi(new_Z1_xi)
  }, ignoreNULL = FALSE)
  
  Z1Change <- reactive({
    if (min(sum(is.na(c(last_Z1_med(), last_Z1_xi()))) > 0, sum(is.na(c(Z1_med(), Z1_xi()))) == 0)) {1}
    else if (min(sum(is.na(c(Z1_med(), Z1_xi()))) > 0, sum(is.na(c(last_Z1_med(), last_Z1_xi()))) == 0)) {1}
    else if (is.na(min(c(last_Z1_med(), last_Z1_xi()) == c(Z1_med(), Z1_xi())))) {0}
    else {1 - min(c(last_Z1_med(), last_Z1_xi()) == c(Z1_med(), Z1_xi()))}})
  
  output$Z1Change <- renderText({Z1Change()})
  
  last_Z2_med <- reactiveVal()
  
  observeEvent(0, {
    new_Z2_med <- Z2_med()
    last_Z2_med(new_Z2_med)
  }, ignoreNULL = FALSE)
  
  last_Z2_xi <- reactiveVal()
  
  observeEvent(0, {
    new_Z2_xi <- Z2_xi()
    last_Z2_xi(new_Z2_xi)
  }, ignoreNULL = FALSE)

  Z2Change <- reactive({
    if (min(sum(is.na(c(last_Z2_med(), last_Z2_xi()))) > 0, sum(is.na(c(Z2_med(), Z2_xi()))) == 0)) {1}
    else if (min(sum(is.na(c(Z2_med(), Z2_xi()))) > 0, sum(is.na(c(last_Z2_med(), last_Z2_xi()))) == 0)) {1}
    else if (is.na(min(c(last_Z2_med(), last_Z2_xi()) == c(Z2_med(), Z2_xi())))) {0}
    else {1 - min(c(last_Z2_med(), last_Z2_xi()) == c(Z2_med(), Z2_xi()))}})

  output$Z2Change <- renderText({Z2Change()})
  
  last_Z3_med <- reactiveVal()
  
  observeEvent(0, {
    new_Z3_med <- Z3_med()
    last_Z3_med(new_Z3_med)
  }, ignoreNULL = FALSE)
  
  last_Z3_xi <- reactiveVal()
  
  observeEvent(0, {
    new_Z3_xi <- Z3_xi()
    last_Z3_xi(new_Z3_xi)
  }, ignoreNULL = FALSE)
  
  Z3Change <- reactive({
    if (min(sum(is.na(c(last_Z3_med(), last_Z3_xi()))) > 0, sum(is.na(c(Z3_med(), Z3_xi()))) == 0)) {1}
    else if (min(sum(is.na(c(Z3_med(), Z3_xi()))) > 0, sum(is.na(c(last_Z3_med(), last_Z3_xi()))) == 0)) {1}
    else if (is.na(min(c(last_Z3_med(), last_Z3_xi()) == c(Z3_med(), Z3_xi())))) {0}
    else {1 - min(c(last_Z3_med(), last_Z3_xi()) == c(Z3_med(), Z3_xi()))}})
  
  output$Z3Change <- renderText({Z3Change()})
  
  last_Z4_med <- reactiveVal()
  
  observeEvent(0, {
    new_Z4_med <- Z4_med()
    last_Z4_med(new_Z4_med)
  }, ignoreNULL = FALSE)
  
  last_Z4_xi <- reactiveVal()
  
  observeEvent(0, {
    new_Z4_xi <- Z4_xi()
    last_Z4_xi(new_Z4_xi)
  }, ignoreNULL = FALSE)
  
  Z4Change <- reactive({
    if (min(sum(is.na(c(last_Z4_med(), last_Z4_xi()))) > 0, sum(is.na(c(Z4_med(), Z4_xi()))) == 0)) {1}
    else if (min(sum(is.na(c(Z4_med(), Z4_xi()))) > 0, sum(is.na(c(last_Z4_med(), last_Z4_xi()))) == 0)) {1}
    else if (is.na(min(c(last_Z4_med(), last_Z4_xi()) == c(Z4_med(), Z4_xi())))) {0}
    else {1 - min(c(last_Z4_med(), last_Z4_xi()) == c(Z4_med(), Z4_xi()))}})
  
  output$Z4Change <- renderText({Z4Change()})
  
  last_Z5_med <- reactiveVal()
  
  observeEvent(0, {
    new_Z5_med <- Z5_med()
    last_Z5_med(new_Z5_med)
  }, ignoreNULL = FALSE)
  
  last_Z5_xi <- reactiveVal()
  
  observeEvent(0, {
    new_Z5_xi <- Z5_xi()
    last_Z5_xi(new_Z5_xi)
  }, ignoreNULL = FALSE)
  
  Z5Change <- reactive({
    if (min(sum(is.na(c(last_Z5_med(), last_Z5_xi()))) > 0, sum(is.na(c(Z5_med(), Z5_xi()))) == 0)) {1}
    else if (min(sum(is.na(c(Z5_med(), Z5_xi()))) > 0, sum(is.na(c(last_Z5_med(), last_Z5_xi()))) == 0)) {1}
    else if (is.na(min(c(last_Z5_med(), last_Z5_xi()) == c(Z5_med(), Z5_xi())))) {0}
    else {1 - min(c(last_Z5_med(), last_Z5_xi()) == c(Z5_med(), Z5_xi()))}})
  
  output$Z5Change <- renderText({Z5Change()})
  
  last_Z6_med <- reactiveVal()
  
  observeEvent(0, {
    new_Z6_med <- Z6_med()
    last_Z6_med(new_Z6_med)
  }, ignoreNULL = FALSE)
  
  last_Z6_xi <- reactiveVal()
  
  observeEvent(0, {
    new_Z6_xi <- Z6_xi()
    last_Z6_xi(new_Z6_xi)
  }, ignoreNULL = FALSE)
  
  Z6Change <- reactive({
    if (min(sum(is.na(c(last_Z6_med(), last_Z6_xi()))) > 0, sum(is.na(c(Z6_med(), Z6_xi()))) == 0)) {1}
    else if (min(sum(is.na(c(Z6_med(), Z6_xi()))) > 0, sum(is.na(c(last_Z6_med(), last_Z6_xi()))) == 0)) {1}
    else if (is.na(min(c(last_Z6_med(), last_Z6_xi()) == c(Z6_med(), Z6_xi())))) {0}
    else {1 - min(c(last_Z6_med(), last_Z6_xi()) == c(Z6_med(), Z6_xi()))}})
  
  output$Z6Change <- renderText({Z6Change()})

  Z1Run <- reactiveVal(0)
  Z2Run <- reactiveVal(0)
  Z3Run <- reactiveVal(0)
  Z4Run <- reactiveVal(0)
  Z5Run <- reactiveVal(0)
  Z6Run <- reactiveVal(0)
  
  meanPlotRun <- reactiveVal(0)
  p3Run <- reactiveVal(0)
  p4Run <- reactiveVal(0)
  p5Run <- reactiveVal(0)
  p6Run <- reactiveVal(0)

  highComplete <- reactive({
    if (is.na(check_Z1Range())) {0}
    else if (check_Z1Range() != 0)  {0}
    else if (check_Z2Range() != 0)  {1}
    else if (ncat() == 3) {2}
    else if (check_Z3Range() != 0)  {2}
    else if (ncat() == 4) {3}
    else if (check_Z4Range() != 0)  {3}
    else if (ncat() == 5) {4}
    else if (check_Z5Range() != 0)  {4}
    else if (ncat() == 6) {5}
    else if (check_Z6Range() != 0)  {5}
    else {6}
  })
  
  lowChange <- reactive({
    if (highComplete() == 0) {0}
    else if (max(uChange())) {1}
    else if (Z1Change()) {1}
    else if (Z2Change()) {2}
    else if (Z3Change() & ncat() > 3) {3}
    else if (Z4Change() & ncat() > 4) {4}
    else if (Z5Change() & ncat() > 5) {5}
    else if (Z6Change() & ncat() > 6) {6}
    else {7}
  })
  
  observeEvent(input$update, {
    if (!anyIssues()){
      if (lowChange() == 1){
        Z1Increase <- Z1Run() + 1
        Z1Run(Z1Increase)
        if (highComplete() > 1){
          Z2Increase <- Z2Run() + 1
          Z2Run(Z2Increase)
          if (ncat() == 3){
            p3Increase <- p3Run() + 1
            p3Run(p3Increase)
            meanPlotIncrease <- meanPlotRun() + 1
            meanPlotRun(meanPlotIncrease)
          }
        }
        if (highComplete() > 2){
          Z3Increase <- Z3Run() + 1
          Z3Run(Z3Increase)
          p3Increase <- p3Run() + 1
          p3Run(p3Increase)
          if (ncat() == 4){
            p4Increase <- p4Run() + 1
            p4Run(p4Increase)
            meanPlotIncrease <- meanPlotRun() + 1
            meanPlotRun(meanPlotIncrease)
          }
        }
        if (highComplete() > 3){
          Z4Increase <- Z4Run() + 1
          Z4Run(Z4Increase)
          p4Increase <- p4Run() + 1
          p4Run(p4Increase)
          if (ncat() == 5){
            p5Increase <- p5Run() + 1
            p5Run(p5Increase)
            meanPlotIncrease <- meanPlotRun() + 1
            meanPlotRun(meanPlotIncrease)
          }
        }
        if (highComplete() > 4){
          Z5Increase <- Z5Run() + 1
          Z5Run(Z5Increase)
          p5Increase <- p5Run() + 1
          p5Run(p5Increase)
          if (ncat() == 6){
            p6Increase <- p6Run() + 1
            p6Run(p6Increase)
            meanPlotIncrease <- meanPlotRun() + 1
            meanPlotRun(meanPlotIncrease)
          }
        }
        if (highComplete() > 5){
          Z6Increase <- Z6Run() + 1
          Z6Run(Z6Increase)
          p6Increase <- p6Run() + 1
          p6Run(p6Increase)
          meanPlotIncrease <- meanPlotRun() + 1
          meanPlotRun(meanPlotIncrease)
        }
      }
      if (lowChange() == 2){
        Z2Increase <- Z2Run() + 1
        Z2Run(Z2Increase)
        if (ncat() == 3){
          p3Increase <- p3Run() + 1
          p3Run(p3Increase)
          meanPlotIncrease <- meanPlotRun() + 1
          meanPlotRun(meanPlotIncrease)
        }
        if (highComplete() > 2){
          Z3Increase <- Z3Run() + 1
          Z3Run(Z3Increase)
          p3Increase <- p3Run() + 1
          p3Run(p3Increase)
          if (ncat() == 4){
            p4Increase <- p4Run() + 1
            p4Run(p4Increase)
            meanPlotIncrease <- meanPlotRun() + 1
            meanPlotRun(meanPlotIncrease)
          }
        }
        if (highComplete() > 3){
          Z4Increase <- Z4Run() + 1
          Z4Run(Z4Increase)
          p4Increase <- p4Run() + 1
          p4Run(p4Increase)
          if (ncat() == 5){
            p5Increase <- p5Run() + 1
            p5Run(p5Increase)
            meanPlotIncrease <- meanPlotRun() + 1
            meanPlotRun(meanPlotIncrease)
          }
        }
        if (highComplete() > 4){
          Z5Increase <- Z5Run() + 1
          Z5Run(Z5Increase)
          p5Increase <- p5Run() + 1
          p5Run(p5Increase)
          if (ncat() == 6){
            p6Increase <- p6Run() + 1
            p6Run(p6Increase)
            meanPlotIncrease <- meanPlotRun() + 1
            meanPlotRun(meanPlotIncrease)
          }
        }
        if (highComplete() > 5){
          Z6Increase <- Z6Run() + 1
          Z6Run(Z6Increase)
          p6Increase <- p6Run() + 1
          p6Run(p6Increase)
          meanPlotIncrease <- meanPlotRun() + 1
          meanPlotRun(meanPlotIncrease)
        }
      }
      if (lowChange() == 3){
        Z3Increase <- Z3Run() + 1
        Z3Run(Z3Increase)
        p3Increase <- p3Run() + 1
        p3Run(p3Increase)
        if (ncat() == 4){
          p4Increase <- p4Run() + 1
          p4Run(p4Increase)
          meanPlotIncrease <- meanPlotRun() + 1
          meanPlotRun(meanPlotIncrease)
        }
        if (highComplete() > 3){
          Z4Increase <- Z4Run() + 1
          Z4Run(Z4Increase)
          p4Increase <- p4Run() + 1
          p4Run(p4Increase)
          if (ncat() == 5){
            p5Increase <- p5Run() + 1
            p5Run(p5Increase)
            meanPlotIncrease <- meanPlotRun() + 1
            meanPlotRun(meanPlotIncrease)
          }
        }
        if (highComplete() > 4){
          Z5Increase <- Z5Run() + 1
          Z5Run(Z5Increase)
          p5Increase <- p5Run() + 1
          p5Run(p5Increase)
          if (ncat() == 6){
            p6Increase <- p6Run() + 1
            p6Run(p6Increase)
            meanPlotIncrease <- meanPlotRun() + 1
            meanPlotRun(meanPlotIncrease)
          }
        }
        if (highComplete() > 5){
          Z6Increase <- Z6Run() + 1
          Z6Run(Z6Increase)
          p6Increase <- p6Run() + 1
          p6Run(p6Increase)
          meanPlotIncrease <- meanPlotRun() + 1
          meanPlotRun(meanPlotIncrease)
        }
      }
      if (lowChange() == 4){
        Z4Increase <- Z4Run() + 1
        Z4Run(Z4Increase)
        p4Increase <- p4Run() + 1
        p4Run(p4Increase)
        if (ncat() == 5){
          p5Increase <- p5Run() + 1
          p5Run(p5Increase)
          meanPlotIncrease <- meanPlotRun() + 1
          meanPlotRun(meanPlotIncrease)
        }
        if (highComplete() > 4){
          Z5Increase <- Z5Run() + 1
          Z5Run(Z5Increase)
          p5Increase <- p5Run() + 1
          p5Run(p5Increase)
          if (ncat() == 6){
            p6Increase <- p6Run() + 1
            p6Run(p6Increase)
            meanPlotIncrease <- meanPlotRun() + 1
            meanPlotRun(meanPlotIncrease)
          }
        }
        if (highComplete() > 5){
          Z6Increase <- Z6Run() + 1
          Z6Run(Z6Increase)
          p6Increase <- p6Run() + 1
          p6Run(p6Increase)
          meanPlotIncrease <- meanPlotRun() + 1
          meanPlotRun(meanPlotIncrease)
        }
      }
      if (lowChange() == 5){
        Z5Increase <- Z5Run() + 1
        Z5Run(Z5Increase)
        p5Increase <- p5Run() + 1
        p5Run(p5Increase)
        if (ncat() == 6){
          p6Increase <- p6Run() + 1
          p6Run(p6Increase)
          meanPlotIncrease <- meanPlotRun() + 1
          meanPlotRun(meanPlotIncrease)
        }
        if (highComplete() > 5){
          Z6Increase <- Z6Run() + 1
          Z6Run(Z6Increase)
          p6Increase <- p6Run() + 1
          p6Run(p6Increase)
          meanPlotIncrease <- meanPlotRun() + 1
          meanPlotRun(meanPlotIncrease)
        }
      }
      if (lowChange() == 6){
        Z6Increase <- Z6Run() + 1
        Z6Run(Z6Increase)
        p6Increase <- p6Run() + 1
        p6Run(p6Increase)
        meanPlotIncrease <- meanPlotRun() + 1
        meanPlotRun(meanPlotIncrease)
      }
    }
  })
  
  Z1prep <- eventReactive(Z1Run(),{
    Z1_results <- NULL
    
    ## helper functions
    f.beta <- function(alpha, beta, x, lower=0, upper=1) {
      p <- pbeta((x-lower)/(upper-lower), alpha, beta)
      log(p/(1-p))
    }

    delta <- function(fit, actual) sum((fit-actual)^2)

    objective <- function(theta, x, prob, ...) {
      ab <- exp(theta) # Parameters are the *logs* of alpha and beta
      fit <- f.beta(ab[1], ab[2], x, ...)
      return (delta(fit, prob))
    }
    
    x_temp <- c(Z1_med(), Z1_xi())
    p.p <- c(0.5, xi())
    x.p <- log(p.p/(1-p.p))
    start <- log(c(1e1, 1e1))
    sol <- nlm(objective, start, x=x_temp, prob=x.p, lower=0, upper=1,
               typsize=c(1,1), fscale=1e-12, gradtol=1e-12)
    parms <- exp(sol$estimate)
    Z1_results[[1]] <- parms
    
    df <- data.frame(x = 0, y = 0, var = "italic(Z)[1]")
    if(round(parms[1],3) < 1 | round(parms[2],3) < 1){
      x_vals <- qbeta(seq(0.01,0.99, by = 0.0025), parms[1], parms[2])
      }
      else{
        x_vals <- qbeta(c(0.0005,seq(0.0025,0.9975, by = 0.0025),0.9995), parms[1], parms[2])
      }
      y_vals <- dbeta(x_vals, parms[1], parms[2])
      df <- rbind(df, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(Z)[",1,"]")))
    df <- df[-1,]
    Z1_results[[2]] <- df
    
    Z1_results[[3]] <- qbeta(obs()[,1],parms[1],parms[2])
    
    Z1_results
  })
  
  Z2prep <- eventReactive(Z2Run(),{
    Z2_results <- NULL
    
    ## helper functions
    f.beta <- function(alpha, beta, x, lower=0, upper=1) {
      p <- pbeta((x-lower)/(upper-lower), alpha, beta)
      log(p/(1-p))
    }
    
    delta <- function(fit, actual) sum((fit-actual)^2)
    
    objective <- function(theta, x, prob, ...) {
      ab <- exp(theta) # Parameters are the *logs* of alpha and beta
      fit <- f.beta(ab[1], ab[2], x, ...)
      return (delta(fit, prob))
    }
    
    x_temp <- c(Z2_med(), Z2_xi())
    p.p <- c(0.5, xi())
    x.p <- log(p.p/(1-p.p))
    start <- log(c(1e1, 1e1))
    sol <- nlm(objective, start, x=x_temp, prob=x.p, lower=0, upper=1,
               typsize=c(1,1), fscale=1e-12, gradtol=1e-12)
    parms <- exp(sol$estimate)
    Z2_results[[1]] <- parms
    
    df <- data.frame(x = 0, y = 0, var = "italic(Z)[2]")
    if(round(parms[1],3) < 1 | round(parms[2],3) < 1){
      x_vals <- qbeta(seq(0.01,0.99, by = 0.0025), parms[1], parms[2])
    }
    else{
      x_vals <- qbeta(c(0.0005,seq(0.0025,0.9975, by = 0.0025),0.9995), parms[1], parms[2])
    }
    y_vals <- dbeta(x_vals, parms[1], parms[2])
    df <- rbind(df, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(Z)[",2,"]")))
    df <- df[-1,]
    Z2_results[[2]] <- df
    
    Z2_results[[3]] <- qbeta(obs()[,2],parms[1],parms[2])
    
    Z2_results
  })
  
  Z3prep <- eventReactive(Z3Run(),{
    Z3_results <- NULL
    
    ## helper functions
    f.beta <- function(alpha, beta, x, lower=0, upper=1) {
      p <- pbeta((x-lower)/(upper-lower), alpha, beta)
      log(p/(1-p))
    }
    
    delta <- function(fit, actual) sum((fit-actual)^2)
    
    objective <- function(theta, x, prob, ...) {
      ab <- exp(theta) # Parameters are the *logs* of alpha and beta
      fit <- f.beta(ab[1], ab[2], x, ...)
      return (delta(fit, prob))
    }
    
    x_temp <- c(Z3_med(), Z3_xi())
    p.p <- c(0.5, xi())
    x.p <- log(p.p/(1-p.p))
    start <- log(c(1e1, 1e1))
    sol <- nlm(objective, start, x=x_temp, prob=x.p, lower=0, upper=1,
               typsize=c(1,1), fscale=1e-12, gradtol=1e-12)
    parms <- exp(sol$estimate)
    Z3_results[[1]] <- parms
    
    df <- data.frame(x = 0, y = 0, var = "italic(Z)[3]")
    if(round(parms[1],3) < 1 | round(parms[2],3) < 1){
      x_vals <- qbeta(seq(0.01,0.99, by = 0.0025), parms[1], parms[2])
    }
    else{
      x_vals <- qbeta(c(0.0005,seq(0.0025,0.9975, by = 0.0025),0.9995), parms[1], parms[2])
    }
    y_vals <- dbeta(x_vals, parms[1], parms[2])
    df <- rbind(df, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(Z)[",3,"]")))
    df <- df[-1,]
    Z3_results[[2]] <- df
    
    Z3_results[[3]] <- qbeta(obs()[,3],parms[1],parms[2])
    
    Z3_results
  })
  
  Z4prep <- eventReactive(Z4Run(),{
    Z4_results <- NULL
    
    ## helper functions
    f.beta <- function(alpha, beta, x, lower=0, upper=1) {
      p <- pbeta((x-lower)/(upper-lower), alpha, beta)
      log(p/(1-p))
    }
    
    delta <- function(fit, actual) sum((fit-actual)^2)
    
    objective <- function(theta, x, prob, ...) {
      ab <- exp(theta) # Parameters are the *logs* of alpha and beta
      fit <- f.beta(ab[1], ab[2], x, ...)
      return (delta(fit, prob))
    }
    
    x_temp <- c(Z4_med(), Z4_xi())
    p.p <- c(0.5, xi())
    x.p <- log(p.p/(1-p.p))
    start <- log(c(1e1, 1e1))
    sol <- nlm(objective, start, x=x_temp, prob=x.p, lower=0, upper=1,
               typsize=c(1,1), fscale=1e-12, gradtol=1e-12)
    parms <- exp(sol$estimate)
    Z4_results[[1]] <- parms
    
    df <- data.frame(x = 0, y = 0, var = "italic(Z)[4]")
    if(round(parms[1],3) < 1 | round(parms[2],3) < 1){
      x_vals <- qbeta(seq(0.01,0.99, by = 0.0025), parms[1], parms[2])
    }
    else{
      x_vals <- qbeta(c(0.0005,seq(0.0025,0.9975, by = 0.0025),0.9995), parms[1], parms[2])
    }
    y_vals <- dbeta(x_vals, parms[1], parms[2])
    df <- rbind(df, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(Z)[",4,"]")))
    df <- df[-1,]
    Z4_results[[2]] <- df
    
    Z4_results[[3]] <- qbeta(obs()[,4],parms[1],parms[2])
    
    Z4_results
  })
  
  Z5prep <- eventReactive(Z5Run(),{
    Z5_results <- NULL
    
    ## helper functions
    f.beta <- function(alpha, beta, x, lower=0, upper=1) {
      p <- pbeta((x-lower)/(upper-lower), alpha, beta)
      log(p/(1-p))
    }
    
    delta <- function(fit, actual) sum((fit-actual)^2)
    
    objective <- function(theta, x, prob, ...) {
      ab <- exp(theta) # Parameters are the *logs* of alpha and beta
      fit <- f.beta(ab[1], ab[2], x, ...)
      return (delta(fit, prob))
    }
    
    x_temp <- c(Z5_med(), Z5_xi())
    p.p <- c(0.5, xi())
    x.p <- log(p.p/(1-p.p))
    start <- log(c(1e1, 1e1))
    sol <- nlm(objective, start, x=x_temp, prob=x.p, lower=0, upper=1,
               typsize=c(1,1), fscale=1e-12, gradtol=1e-12)
    parms <- exp(sol$estimate)
    Z5_results[[1]] <- parms
    
    df <- data.frame(x = 0, y = 0, var = "italic(Z)[5]")
    if(round(parms[1],3) < 1 | round(parms[2],3) < 1){
      x_vals <- qbeta(seq(0.01,0.99, by = 0.0025), parms[1], parms[2])
    }
    else{
      x_vals <- qbeta(c(0.0005,seq(0.0025,0.9975, by = 0.0025),0.9995), parms[1], parms[2])
    }
    y_vals <- dbeta(x_vals, parms[1], parms[2])
    df <- rbind(df, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(Z)[",5,"]")))
    df <- df[-1,]
    Z5_results[[2]] <- df
    
    Z5_results[[3]] <- qbeta(obs()[,5],parms[1],parms[2])
    
    Z5_results
  })
  
  Z6prep <- eventReactive(Z6Run(),{
    Z6_results <- NULL
    
    ## helper functions
    f.beta <- function(alpha, beta, x, lower=0, upper=1) {
      p <- pbeta((x-lower)/(upper-lower), alpha, beta)
      log(p/(1-p))
    }
    
    delta <- function(fit, actual) sum((fit-actual)^2)
    
    objective <- function(theta, x, prob, ...) {
      ab <- exp(theta) # Parameters are the *logs* of alpha and beta
      fit <- f.beta(ab[1], ab[2], x, ...)
      return (delta(fit, prob))
    }
    
    x_temp <- c(Z6_med(), Z6_xi())
    p.p <- c(0.5, xi())
    x.p <- log(p.p/(1-p.p))
    start <- log(c(1e1, 1e1))
    sol <- nlm(objective, start, x=x_temp, prob=x.p, lower=0, upper=1,
               typsize=c(1,1), fscale=1e-12, gradtol=1e-12)
    parms <- exp(sol$estimate)
    Z6_results[[1]] <- parms
    
    df <- data.frame(x = 0, y = 0, var = "italic(Z)[6]")
    if(round(parms[1],3) < 1 | round(parms[2],3) < 1){
      x_vals <- qbeta(seq(0.01,0.99, by = 0.0025), parms[1], parms[2])
    }
    else{
      x_vals <- qbeta(c(0.0005,seq(0.0025,0.9975, by = 0.0025),0.9995), parms[1], parms[2])
    }
    y_vals <- dbeta(x_vals, parms[1], parms[2])
    df <- rbind(df, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(Z)[",6,"]")))
    df <- df[-1,]
    Z6_results[[2]] <- df
    
    Z6_results[[3]] <- qbeta(obs()[,6],parms[1],parms[2])
    
    Z6_results
  })
  
  psprep <- eventReactive(postRun(),{
    
    ps <- matrix(Z1prep()[[3]], nrow = 1)
    if (highComplete() > 1) {
      ps <- rbind(ps, ps[2-1,]*(Z2prep()[[3]])*(1-Z1prep()[[3]])/Z1prep()[[3]])
      if (ncat() == 3){
        ps <- rbind(ps, ps[2,]*(1 - Z2prep()[[3]])/Z2prep()[[3]])
      }
    }
    if (highComplete() > 2) {
      ps <- rbind(ps, ps[3-1,]*(Z3prep()[[3]])*(1-Z2prep()[[3]])/Z2prep()[[3]])
      if (ncat() == 4){
        ps <- rbind(ps, ps[3,]*(1 - Z3prep()[[3]])/Z3prep()[[3]])
      }
    }
    if (highComplete() > 3) {
      ps <- rbind(ps, ps[4-1,]*(Z4prep()[[3]])*(1-Z3prep()[[3]])/Z3prep()[[3]])
      if (ncat() == 5){
        ps <- rbind(ps, ps[4,]*(1 - Z4prep()[[3]])/Z4prep()[[3]])
      }
    }
    if (highComplete() > 4) {
      ps <- rbind(ps, ps[5-1,]*(Z5prep()[[3]])*(1-Z4prep()[[3]])/Z4prep()[[3]])
      if (ncat() == 6){
        ps <- rbind(ps, ps[5,]*(1 - Z5prep()[[3]])/Z5prep()[[3]])
      }
    }
    if (highComplete() > 5) {
      ps <- rbind(ps, ps[6-1,]*(Z6prep()[[3]])*(1-Z5prep()[[3]])/Z5prep()[[3]])
      if (ncat() == 7){
        ps <- rbind(ps, ps[6,]*(1 - Z6prep()[[3]])/Z6prep()[[3]])
      }
    }
    
   ps
  })
  
  p1prep <- eventReactive(Z1Run(),{
    p1_results <- NULL
    
    p1_results[[1]] <- median(psprep()[1,])
    medd_p1 <- round(median(psprep()[1,]), digits=2)
    
    dfp <- data.frame(x = 0, y = 0, var = "italic(p)[1]")
    bde_temp <- bde(psprep()[1,], dataPointsCache = c(0, 0.005,seq(0.01,0.99, length.out = 99),0.995,1), estimator = "betakernel")
    x_vals <- bde_temp@dataPointsCache
    y_vals <- bde_temp@densityCache
    dfp <- rbind(dfp, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(p)[",1,"] ~ (italic(Q)[", 2, "]:'", format(get(paste0("medd_p1")), nsmall = 2), "')")))
    dfp <- dfp[-1,]
    p1_results[[2]] <- dfp
    
    p1_results
  })
  
  p2prep <- eventReactive(Z2Run(),{
    p2_results <- NULL
    
    p2_results[[1]] <- median(psprep()[2,])
    medd_p2 <- round(median(psprep()[2,]), digits=2)
    
    dfp <- data.frame(x = 0, y = 0, var = "italic(p)[2]")
    bde_temp <- bde(psprep()[2,], dataPointsCache = c(0, 0.005,seq(0.01,0.99, length.out = 99),0.995,1), estimator = "betakernel")
    x_vals <- bde_temp@dataPointsCache
    y_vals <- bde_temp@densityCache
    dfp <- rbind(dfp, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(p)[",2,"] ~ (italic(Q)[", 2, "]:'", format(get(paste0("medd_p2")), nsmall = 2), "')")))
    dfp <- dfp[-1,]
    p2_results[[2]] <- dfp
    
    p2_results
  })
  
  p3prep <- eventReactive(p3Run(),{
    p3_results <- NULL
    
    p3_results[[1]] <- median(psprep()[3,])
    medd_p3 <- round(median(psprep()[3,]), digits=2)
    
    dfp <- data.frame(x = 0, y = 0, var = "italic(p)[3]")
    bde_temp <- bde(psprep()[3,], dataPointsCache = c(0, 0.005,seq(0.01,0.99, length.out = 99),0.995,1), estimator = "betakernel")
    x_vals <- bde_temp@dataPointsCache
    y_vals <- bde_temp@densityCache
    dfp <- rbind(dfp, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(p)[",3,"] ~ (italic(Q)[", 2, "]:'", format(get(paste0("medd_p3")), nsmall = 2), "')")))
    dfp <- dfp[-1,]
    p3_results[[2]] <- dfp
    
    p3_results
  })
  
  p4prep <- eventReactive(p4Run(),{
    p4_results <- NULL
    
    p4_results[[1]] <- median(psprep()[4,])
    medd_p4 <- round(median(psprep()[4,]), digits=2)
    
    dfp <- data.frame(x = 0, y = 0, var = "italic(p)[4]")
    bde_temp <- bde(psprep()[4,], dataPointsCache = c(0, 0.005,seq(0.01,0.99, length.out = 99),0.995,1), estimator = "betakernel")
    x_vals <- bde_temp@dataPointsCache
    y_vals <- bde_temp@densityCache
    dfp <- rbind(dfp, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(p)[",4,"] ~ (italic(Q)[", 2, "]:'", format(get(paste0("medd_p4")), nsmall = 2), "')")))
    dfp <- dfp[-1,]
    p4_results[[2]] <- dfp
    
    p4_results
  })
  
  p5prep <- eventReactive(p5Run(),{
    p5_results <- NULL
    
    p5_results[[1]] <- median(psprep()[5,])
    medd_p5 <- round(median(psprep()[5,]), digits=2)
    
    dfp <- data.frame(x = 0, y = 0, var = "italic(p)[5]")
    bde_temp <- bde(psprep()[5,], dataPointsCache = c(0, 0.005,seq(0.01,0.99, length.out = 99),0.995,1), estimator = "betakernel")
    x_vals <- bde_temp@dataPointsCache
    y_vals <- bde_temp@densityCache
    dfp <- rbind(dfp, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(p)[",5,"] ~ (italic(Q)[", 2, "]:'", format(get(paste0("medd_p5")), nsmall = 2), "')")))
    dfp <- dfp[-1,]
    p5_results[[2]] <- dfp
    
    p5_results
  })
  
  p6prep <- eventReactive(p6Run(),{
    p6_results <- NULL
    
    p6_results[[1]] <- median(psprep()[6,])
    medd_p6 <- round(median(psprep()[6,]), digits=2)
    
    dfp <- data.frame(x = 0, y = 0, var = "italic(p)[6]")
    bde_temp <- bde(psprep()[6,], dataPointsCache = c(0, 0.005,seq(0.01,0.99, length.out = 99),0.995,1), estimator = "betakernel")
    x_vals <- bde_temp@dataPointsCache
    y_vals <- bde_temp@densityCache
    dfp <- rbind(dfp, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(p)[",6,"] ~ (italic(Q)[", 2, "]:'", format(get(paste0("medd_p6")), nsmall = 2), "')")))
    dfp <- dfp[-1,]
    p6_results[[2]] <- dfp
    
    p6_results
  })
  
  p7prep <- eventReactive(Z6Run(),{
    p7_results <- NULL
    
    p7_results[[1]] <- median(psprep()[7,])
    medd_p7 <- round(median(psprep()[7,]), digits=2)
    
    dfp <- data.frame(x = 0, y = 0, var = "italic(p)[7]")
    bde_temp <- bde(psprep()[7,], dataPointsCache = c(0, 0.005,seq(0.01,0.99, length.out = 99),0.995,1), estimator = "betakernel")
    x_vals <- bde_temp@dataPointsCache
    y_vals <- bde_temp@densityCache
    dfp <- rbind(dfp, data.frame(x = c(0,x_vals,1), y = c(0,y_vals,0), var = paste0("italic(p)[",7,"] ~ (italic(Q)[", 2, "]:'", format(get(paste0("medd_p7")), nsmall = 2), "')")))
    dfp <- dfp[-1,]
    p7_results[[2]] <- dfp
    
    p7_results
  })
  
  plotzpre <- eventReactive(plotRun(),{
    df <- data.frame(x = 0, y = 1, var = "italic(Z)[1]")
    for (i in 2:(ncat() - 1)){
      df <- rbind(df, data.frame(x = 0, y = 0, var = paste0("italic(Z)[",i,"]")))
    }
    
    pltz <- ggplot(data = df, aes(x = x, y = y)) + theme_bw() +
      geom_polygon(alpha = 0.75, fill=cbPalette[1], col = "black", size = 0.8) +
      coord_flip() +
      facet_wrap(~ var, nrow = 1, labeller = label_parsed) +
      theme(strip.text.x = element_text(size = 14)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.title.x = element_text(size = 16, margin=margin(10,0,0,0))) +
      ylab("Density") +
      theme(axis.text.y = element_text(size = 13)) +
      theme(axis.text.x = element_text(size = 13)) +
      theme(plot.title = element_text(hjust = 0.5,size=24,face="bold", 
                                      margin=margin(0,0,10,0))) +
      labs(title=bquote(bold("Priors for ")*bold(italic(Z)))) + xlim(0,1) +
      ylim(0, 1)
    
    pltbuildz <- ggplot_build(pltz)[["layout"]]$panel_params[[2]]$x$breaks
    if (is.na(tail(pltbuildz,1))){pltbuildz <- pltbuildz[-length(pltbuildz)]}
    
    pltz +
    scale_y_continuous(breaks = pltbuildz, 
                         labels=c(pltbuildz[1:(length(pltbuildz)-1)],rep("", 1)))
  })
  
  plotppre <- eventReactive(plotRun(),{
    dfp <- data.frame(x = 0, y = 0, var = "italic(p)[1]")
    for (i in 1:ncat()){
      dfp <- rbind(dfp, data.frame(x = 0, y = (i-1)/(ncat()-1), var = paste0("italic(p)[",i,"]")))
    }
    dfp <- dfp[-1,]
    
    pltp <- ggplot(data = dfp, aes(x = x, y = y)) + theme_bw() +
      geom_polygon(alpha = 0.75, fill=cbbPalette[6], col = "black", size = 0.8) +
      coord_flip() +
      facet_wrap(~ var, nrow = 1, labeller = label_parsed) +
      theme(strip.text.x = element_text(size = 14)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.title.x = element_text(size = 16, margin=margin(10,0,0,0))) +
      ylab("Density") +
      theme(axis.text.y = element_text(size = 13)) +
      theme(axis.text.x = element_text(size = 13)) +
      theme(plot.title = element_text(hjust = 0.5,size=24,face="bold", 
                                      margin=margin(0,0,10,0))) +
      labs(title=bquote(bold("Priors for ")*bold(italic(p)))) + xlim(0,1) +
      ylim(0, 1)
    
    if(min(check_psum() == 0, medp())){
      for (i in 1:ncat()){
        data.med_p <-data.frame(x=phats()[i],y=0, xend = phats()[i],yend=Inf, var = paste0("italic(p)[",i,"]"))
        pltp <- pltp + geom_segment(data=data.med_p,
                                    aes(x=x,y=y, xend=xend,yend=yend),size = 0.8,
                                    linetype = "longdash", colour=adjustcolor(cbbPalette[1],0.75), inherit.aes=FALSE)
      }
    }
    
    pltbuildp <- ggplot_build(pltp)[["layout"]]$panel_params[[2]]$x$breaks
    if (is.na(tail(pltbuildp,1))){pltbuildp <- pltbuildp[-length(pltbuildp)]}
    
    pltp +
    scale_y_continuous(breaks = pltbuildp, 
                         labels=c(pltbuildp[1:(length(pltbuildp)-1)],rep("", 1)))
  })
  
  plotmupre <- eventReactive(plotRun(),{
    dfmu <- data.frame(x = 0, y = 0)
    dfmu <- rbind(dfmu, data.frame(x = mean(c(1,ncat())), y = c(0)))
    dfmu <- dfmu[-1,]
    
    ggplot(data = dfmu, aes(x = x, y = y)) + theme_bw() +
      geom_polygon(alpha = 0.75, fill=cbbPalette[2], col = "black", size = 0.8) +
      theme(strip.text.x = element_text(size = 14)) +
      theme(axis.title.y = element_blank()) +
      ylab("\nDensity") +
      theme(axis.text.y = element_text(size = 13)) +
      theme(axis.text.x = element_text(size = 13)) +
      labs(title = bquote(bold("Prior for Mean"~ theta))) +
      labs(x=bquote(theta)) +
      theme(plot.title = element_text(hjust = 0.5,size=24,face="bold", 
                                      margin=margin(0,0,10,0))) +
      theme(axis.title.x = element_text(size = 16, margin=margin(10,0,0,0))) + xlim(1,ncat()) +
      ylim(0, 1)
  
  })
  
  plotzpost <- eventReactive(postRun(),{
    df <- Z1prep()[[2]]
    if (highComplete() > 1){df <- rbind(df, Z2prep()[[2]])}
    else {df <- rbind(df, data.frame(x = 0, y = 0, var = paste0("italic(Z)[",2,"]")))}
    if (highComplete() > 2){df <- rbind(df, Z3prep()[[2]])}
    else if (ncat() > 3) {df <- rbind(df, data.frame(x = 0, y = 0, var = paste0("italic(Z)[",3,"]")))}
    if (highComplete() > 3){df <- rbind(df, Z4prep()[[2]])}
    else if (ncat() > 4) {df <- rbind(df, data.frame(x = 0, y = 0, var = paste0("italic(Z)[",4,"]")))}
    if (highComplete() > 4){df <- rbind(df, Z5prep()[[2]])}
    else if (ncat() > 5) {df <- rbind(df, data.frame(x = 0, y = 0, var = paste0("italic(Z)[",5,"]")))}
    if (highComplete() > 5){df <- rbind(df, Z6prep()[[2]])}
    else if (ncat() > 6) {df <- rbind(df, data.frame(x = 0, y = 0, var = paste0("italic(Z)[",6,"]")))}
    
    pltz <- ggplot(data = df, aes(x = x, y = y)) + theme_bw() +
      geom_polygon(alpha = 0.75, fill=cbPalette[1], col = "black", size = 0.8) +
      coord_flip() +
      facet_wrap(~ var, nrow = 1, labeller = label_parsed) +
      theme(strip.text.x = element_text(size = 14)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.title.x = element_text(size = 16, margin=margin(10,0,0,0))) +
      ylab("Density") +
      theme(axis.text.y = element_text(size = 13)) +
      theme(axis.text.x = element_text(size = 13)) +
      theme(plot.title = element_text(hjust = 0.5,size=24,face="bold", 
                                      margin=margin(0,0,10,0))) +
      labs(title=bquote(bold("Priors for ")*bold(italic(Z))))
    
      meds <- c(Z1_med(), Z2_med(), Z3_med(), Z4_med(), Z5_med(), Z6_med())
      if (medz()){
        for (i in 1:highComplete()){
          data.med_z <-data.frame(x=meds[i],y=0, xend = meds[i],yend=Inf, var = paste0("italic(Z)[",i,"]"))
          pltz <- pltz + geom_segment(data=data.med_z,
                                    aes(x=x,y=y, xend=xend,yend=yend),size = 0.8,
                                    linetype = "longdash", colour=adjustcolor(cbbPalette[1],0.75), inherit.aes=FALSE)
        }
      }
    
    pltbuildz <- ggplot_build(pltz)[["layout"]]$panel_params[[2]]$x$breaks
    if (is.na(tail(pltbuildz,1))){pltbuildz <- pltbuildz[-length(pltbuildz)]}
    
    zlim <- ggplot_build(pltz)[["layout"]]$panel_params[[1]]$x.range[2]
    diffz <- mean(diff(pltbuildz))
    plot_last <- (zlim > tail(pltbuildz,1) + 0.5*diffz)
    if (plot_last) {scale_new <- scale_y_continuous(breaks = pltbuildz,
                                                   labels=pltbuildz)}
    else {scale_new <- scale_y_continuous(breaks = pltbuildz,
                                          labels=c(pltbuildz[1:(length(pltbuildz)-1)],rep("", 1)))}
    
    pltz + scale_new
  })
  
  plotppost <- eventReactive(postRun(),{
    
    dfp <- p1prep()[[2]]
    if (highComplete() > 1){dfp <- rbind(dfp, p2prep()[[2]])}
    else {dfp <- rbind(dfp, data.frame(x = 0, y = 0, var = paste0("italic(p)[",2,"]")))}
    if (highComplete() > 2){dfp <- rbind(dfp, p3prep()[[2]])}
    else if (highComplete() > 1 & ncat() == 3){dfp <- rbind(dfp, p3prep()[[2]])}
    else if (ncat() > 2) {dfp <- rbind(dfp, data.frame(x = 0, y = 0, var = paste0("italic(p)[",3,"]")))}
    if (highComplete() > 3){dfp <- rbind(dfp, p4prep()[[2]])}
    else if (highComplete() > 2 & ncat() == 4){dfp <- rbind(dfp, p4prep()[[2]])}
    else if (ncat() > 3) {dfp <- rbind(dfp, data.frame(x = 0, y = 0, var = paste0("italic(p)[",4,"]")))}
    if (highComplete() > 4){dfp <- rbind(dfp, p5prep()[[2]])}
    else if (highComplete() > 3 & ncat() == 5){dfp <- rbind(dfp, p5prep()[[2]])}
    else if (ncat() > 4) {dfp <- rbind(dfp, data.frame(x = 0, y = 0, var = paste0("italic(p)[",5,"]")))}
    if (highComplete() > 5){dfp <- rbind(dfp, p6prep()[[2]])}
    else if (highComplete() > 4 & ncat() == 6){dfp <- rbind(dfp, p6prep()[[2]])}
    else if (ncat() > 5) {dfp <- rbind(dfp, data.frame(x = 0, y = 0, var = paste0("italic(p)[",6,"]")))}
    if (highComplete() > 5){dfp <- rbind(dfp, p7prep()[[2]])}
    else if (ncat() > 6) {dfp <- rbind(dfp, data.frame(x = 0, y = 0, var = paste0("italic(p)[",7,"]")))}
    
    pltp <- ggplot(data = dfp, aes(x = x, y = y)) + theme_bw() +
      geom_polygon(alpha = 0.75, fill=cbbPalette[6], col = "black", size = 0.8) +
      coord_flip() +
      facet_wrap(~ var, nrow = 1, labeller = label_parsed) +
      theme(strip.text.x = element_text(size = 14)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.title.x = element_text(size = 16, margin=margin(10,0,0,0))) +
      ylab("Density") +
      theme(axis.text.y = element_text(size = 13)) +
      theme(axis.text.x = element_text(size = 13)) +
      theme(plot.title = element_text(hjust = 0.5,size=24,face="bold", 
                                      margin=margin(0,0,10,0))) +
      labs(title=bquote(bold("Priors for ")*bold(italic(p))))
    
    if(min(check_psum() == 0, medp())){
      for (i in 1:ncat()){
        data.med_p <-data.frame(x=phats()[i],y=0, xend = phats()[i],yend=Inf, var = paste0("italic(p)[",i,"]"))
        pltp <- pltp + geom_segment(data=data.med_p,
                                    aes(x=x,y=y, xend=xend,yend=yend),size = 0.8,
                                    linetype = "longdash", colour=adjustcolor(cbbPalette[1],0.75), inherit.aes=FALSE)
      }
    }
    
    pltbuildp <- ggplot_build(pltp)[["layout"]]$panel_params[[2]]$x$breaks
    if (is.na(tail(pltbuildp,1))){pltbuildp <- pltbuildp[-length(pltbuildp)]}
    
    plim <- ggplot_build(pltp)[["layout"]]$panel_params[[1]]$x.range[2]
    diffp <- mean(diff(pltbuildp))
    plot_last <- (plim > tail(pltbuildp,1) + 0.5*diffp)
    if (plot_last) {scale_new <- scale_y_continuous(breaks = pltbuildp,
                                                    labels=pltbuildp)}
    else {scale_new <- scale_y_continuous(breaks = pltbuildp,
                                          labels=c(pltbuildp[1:(length(pltbuildp)-1)],rep("", 1)))}
    
    pltp + scale_new
  })
  
  plotmupost <- eventReactive(meanPlotRun(),{
    mus <- 0
    for (i in 1:ncat()){
      mus <- mus + i*psprep()[i,]
    }
    
    dfmu <- data.frame(x = 0, y = 0)
    bde_mu <- bde(mus, estimator = "betakernel", dataPointsCache = seq(1,ncat(),length.out = 151),
                  lower.limit = 1, upper.limit = ncat(), b = 0.66*length(mus)^(-2/5))
    x_vals <- (ncat() - 1)*bde_mu@dataPointsCache + 1
    y_vals <- bde_mu@densityCache/(ncat() - 1)
    dfmu <- rbind(dfmu, data.frame(x = c(1,x_vals,ncat()), y = c(0,y_vals,0)))
    dfmu <- dfmu[-1,]
    
    med_val <- format(round(median(mus), digits=2), nsmall = 2) 
    # med_val <- round(median(mus),2)
    
    pltmu <- ggplot(data = dfmu, aes(x = x, y = y)) + theme_bw() +
      geom_polygon(alpha = 0.75, fill=cbbPalette[2], col = "black", size = 0.8) +
      theme(strip.text.x = element_text(size = 14)) +
      theme(axis.title.y = element_blank()) +
      ylab("\nDensity") +
      theme(axis.text.y = element_text(size = 13)) +
      theme(axis.text.x = element_text(size = 13)) +
      labs(title = bquote(bold("Prior for Mean"~ theta))) +
      labs(x=bquote(theta~"("*italic("Q")[2]*":"~.(med_val)*")")) +
      theme(plot.title = element_text(hjust = 0.5,size=24,face="bold", 
                                      margin=margin(0,0,10,0))) +
      theme(axis.title.x = element_text(size = 16, margin=margin(10,0,0,0)))
    
    pltmu
    
  })
  
  observeEvent(plotRun(), {
    if (tryCatch(
      expr = {highComplete() == 0
      },
      error = function(e){ 
        1
      }
    )){
    fig.row1 <- plot_grid(plotzpre() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")), 
                          plotmupre() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
                          rel_widths = c(0.4375*ncat(),1))
    fig.row2 <- plot_grid(plotppre())
    fig <- plot_grid(fig.row1, fig.row2, nrow = 2)
    }
    else if (highComplete() == (ncat()-1)){
      fig.row1 <- plot_grid(plotzpost() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")), 
                            plotmupost() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
                            rel_widths = c(0.4375*ncat(),1))
      fig.row2 <- plot_grid(plotppost())
      fig <- plot_grid(fig.row1, fig.row2, nrow = 2)
    }
    else{
      fig.row1 <- plot_grid(plotzpost() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")), 
                            plotmupre() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
                            rel_widths = c(0.4375*ncat(),1))
      fig.row2 <- plot_grid(plotppost())
      fig <- plot_grid(fig.row1, fig.row2, nrow = 2)
    }
    
    output$plotfull <- renderPlot({isolate(fig)})})
  
  observeEvent(input$update, {
    if (input$update < 1){
      output$Z1medUI <- renderUI({textInput(inputId = "Z1med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("1"))),
                                            placeholder = "Enter value")})
      output$Z2medUI <- renderUI({textInput(inputId = "Z2med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("2"))),
                                            placeholder = "Enter value")})
      output$Z3medUI <- renderUI({textInput(inputId = "Z3med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("3"))),
                                            placeholder = "Enter value")})
      output$Z4medUI <- renderUI({textInput(inputId = "Z4med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("4"))),
                                            placeholder = "Enter value")})
      output$Z5medUI <- renderUI({textInput(inputId = "Z5med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("5"))),
                                            placeholder = "Enter value")})
      output$Z6medUI <- renderUI({textInput(inputId = "Z6med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("6"))),
                                            placeholder = "Enter value")})
    }
    else if (check_psum() == 0){
      zhats <- head(phats(), length(phats())-1)/(1- cumsum(c(0,head(phats(), length(phats())-2))))
      Z1med_temp <- input$Z1med
      output$Z1medUI <- renderUI({textInput(inputId = "Z1med", value = Z1med_temp,
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("1"))),
                                            placeholder = as.character(round(zhats[1],4)))})
      Z2med_temp <- input$Z2med
      output$Z2medUI <- renderUI({textInput(inputId = "Z2med", value = Z2med_temp,
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("2"))),
                                            placeholder = as.character(round(zhats[2],4)))})
      if (ncat() > 3){
        Z3med_temp <- input$Z3med
        output$Z3medUI <- renderUI({textInput(inputId = "Z3med", value = Z3med_temp,
                                              label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("3"))),
                                              placeholder = as.character(round(zhats[3],4)))})
      }
      if (ncat() > 4){
        Z4med_temp <- input$Z4med
        output$Z4medUI <- renderUI({textInput(inputId = "Z4med", value = Z4med_temp,
                                              label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("4"))),
                                              placeholder = as.character(round(zhats[4],4)))})
      }
      if (ncat() > 5){
        Z5med_temp <- input$Z5med
        output$Z5medUI <- renderUI({textInput(inputId = "Z5med", value = Z5med_temp,
                                              label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("5"))),
                                              placeholder = as.character(round(zhats[5],4)))})
      }
      if (ncat() > 6){
        Z6med_temp <- input$Z6med
        output$Z6medUI <- renderUI({textInput(inputId = "Z6med", value = Z6med_temp,
                                              label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("6"))),
                                              placeholder = as.character(round(zhats[6],4)))})
      }
    }
    else{
      output$Z1medUI <- renderUI({textInput(inputId = "Z1med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("1"))),
                                            placeholder = "Enter value")})
      output$Z2medUI <- renderUI({textInput(inputId = "Z2med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("2"))),
                                            placeholder = "Enter value")})
      output$Z3medUI <- renderUI({textInput(inputId = "Z3med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("3"))),
                                            placeholder = "Enter value")})
      output$Z4medUI <- renderUI({textInput(inputId = "Z4med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("4"))),
                                            placeholder = "Enter value")})
      output$Z5medUI <- renderUI({textInput(inputId = "Z5med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("5"))),
                                            placeholder = "Enter value")})
      output$Z6medUI <- renderUI({textInput(inputId = "Z6med", 
                                            label = HTML(paste0(tags$em("Q"),tags$sub("2"), ": ",tags$em("Z"),tags$sub("6"))),
                                            placeholder = "Enter value")})
    }
  }, ignoreNULL = FALSE)
  
  # observeEvent(input$update, {
  #     gaussrho_temp <- input$gaussrho
  #     lowerbound_temp <- as.numeric(substr(as.character(-(ncat()-2)^{-1}), 1, 6))
  #     output$gaussrhoUI <- renderUI({textInput(inputId = "gaussrho", label = HTML(paste0("Correlation Parameter (\u03C1)")),
  #                                             value = gaussrho_temp,
  #                                             placeholder = HTML(paste0("Enter value in (",lowerbound_temp, ", 1)")))})
  #     
  #     trho_temp <- input$trho
  #     output$trhoUI <- renderUI({textInput(inputId = "trho", label = HTML(paste0("Correlation Parameter (\u03C1)")),
  #                                              value = trho_temp,
  #                                              placeholder = HTML(paste0("Enter value in (",lowerbound_temp, ", 1)")))})
  # })
  
  
  observeEvent(input$update, {
    output$invalid_Z1Range <- renderText({isolate(as.numeric(check_Z1Range()))})
    output$invalid_Z2Range <- renderText({isolate(as.numeric(check_Z2Range()))})
    output$invalid_Z3Range <- renderText({isolate(as.numeric(check_Z3Range()))})
    output$invalid_Z4Range <- renderText({isolate(as.numeric(check_Z4Range()))})
    output$invalid_Z5Range <- renderText({isolate(as.numeric(check_Z5Range()))})
    output$invalid_Z6Range <- renderText({isolate(as.numeric(check_Z6Range()))})
  }, ignoreNULL = FALSE)
  
  observeEvent(input$update, {
    output$invalid_psum <- renderText({isolate(as.numeric(check_psum()))})
    output$invalid_xi <- renderText({isolate(as.numeric(check_xi()))})
  })
  
  observeEvent(Z1Run(), {
    new_Z1_med <- Z1_med()
    last_Z1_med(new_Z1_med)
    
    new_Z1_xi <- Z1_xi()
    last_Z1_xi(new_Z1_xi)
  })
  
  observeEvent(Z2Run(), {
    new_Z2_med <- Z2_med()
    last_Z2_med(new_Z2_med)
    
    new_Z2_xi <- Z2_xi()
    last_Z2_xi(new_Z2_xi)
  })
  
  observeEvent(Z3Run(), {
    new_Z3_med <- Z3_med()
    last_Z3_med(new_Z3_med)
    
    new_Z3_xi <- Z3_xi()
    last_Z3_xi(new_Z3_xi)
  })
  
  observeEvent(Z4Run(), {
    new_Z4_med <- Z4_med()
    last_Z4_med(new_Z4_med)

    new_Z4_xi <- Z4_xi()
    last_Z4_xi(new_Z4_xi)
  })
  
  observeEvent(Z5Run(), {
    new_Z5_med <- Z5_med()
    last_Z5_med(new_Z5_med)

    new_Z5_xi <- Z5_xi()
    last_Z5_xi(new_Z5_xi)
  })
  
  observeEvent(Z6Run(), {
    new_Z6_med <- Z6_med()
    last_Z6_med(new_Z6_med)

    new_Z6_xi <- Z6_xi()
    last_Z6_xi(new_Z6_xi)
  })
  
  quantileInputs <- eventReactive(postRun(),{
    
    qInputs <- data.frame(median = Z1_med(), placeholder = Z1_xi())
    colnames(qInputs)[2] <- paste0(xi(),"-quantile")
    if (highComplete() > 1){
      qInputs <- rbind(qInputs, c(Z2_med(), Z2_xi()))
    }
    else{
      qInputs <- rbind(qInputs, rep(NA,2))
    }
    if (highComplete() > 2 & ncat() > 3){
      qInputs <- rbind(qInputs, c(Z3_med(), Z3_xi())) 
    }
    else if (ncat() > 3){
      qInputs <- rbind(qInputs, rep(NA,2)) 
    }
    if (highComplete() > 3 & ncat() > 4){
      qInputs <- rbind(qInputs, c(Z4_med(), Z4_xi())) 
    }
    else if (ncat() > 4){
      qInputs <- rbind(qInputs, rep(NA,2)) 
    }
    if (highComplete() > 4 & ncat() > 5){
      qInputs <- rbind(qInputs, c(Z5_med(), Z5_xi())) 
    }
    else if (ncat() > 5){
      qInputs <- rbind(qInputs, rep(NA,2)) 
    }
    if (highComplete() > 5 & ncat() > 6){
      qInputs <- rbind(qInputs, c(Z6_med(), Z6_xi())) 
    }
    else if (ncat() > 6){
      qInputs <- rbind(qInputs, rep(NA,2)) 
    }
    
    qInputs
  }, ignoreNULL = FALSE)
  
  betaParamsfull <- eventReactive(postRun(),{
    
    if (highComplete() > 0){
      betaParams <- data.frame(alpha = Z1prep()[[1]][1], beta = Z1prep()[[1]][2])
    }
    else{
      betaParams <- data.frame(alpha = NA, beta = NA)
    }
    if (highComplete() > 1){
      betaParams <- rbind(betaParams, Z2prep()[[1]])
    }
    else{
      betaParams <- rbind(betaParams, rep(NA,2))
    }
    if (highComplete() > 2 & ncat() > 3){
      betaParams <- rbind(betaParams, Z3prep()[[1]])
    }
    else if (ncat() > 3){
      betaParams <- rbind(betaParams, rep(NA,2))
    }
    if (highComplete() > 3 & ncat() > 4){
      betaParams <- rbind(betaParams, Z4prep()[[1]])
    }
    else if (ncat() > 4){
      betaParams <- rbind(betaParams, rep(NA,2))
    }
    if (highComplete() > 4 & ncat() > 5){
      betaParams <- rbind(betaParams, Z5prep()[[1]])
    }
    else if (ncat() > 5){
      betaParams <- rbind(betaParams, rep(NA,2))
    }
    if (highComplete() > 5 & ncat() > 6){
      betaParams <- rbind(betaParams, Z6prep()[[1]])
    }
    else if (ncat() > 6){
      betaParams <- rbind(betaParams, rep(NA,2))
    }
    betaParams
  }, ignoreNULL = FALSE)
  
  plotPDF <- eventReactive(plotRun(), {
    if (tryCatch(
      expr = {highComplete() == 0
      },
      error = function(e){ 
        1
      }
    )){
      fig.row1 <- plot_grid(plotzpre() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")), 
                            plotmupre() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
                            rel_widths = c(0.4375*ncat(),1))
      fig.row2 <- plot_grid(plotppre())
      fig <- plot_grid(fig.row1, fig.row2, nrow = 2)
    }
    else if (highComplete() == (ncat()-1)){
      fig.row1 <- plot_grid(plotzpost() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")), 
                            plotmupost() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
                            rel_widths = c(0.4375*ncat(),1))
      fig.row2 <- plot_grid(plotppost())
      fig <- plot_grid(fig.row1, fig.row2, nrow = 2)
    }
    else{
      fig.row1 <- plot_grid(plotzpost() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")), 
                            plotmupre() + theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
                            rel_widths = c(0.4375*ncat(),1))
      fig.row2 <- plot_grid(plotppost())
      fig <- plot_grid(fig.row1, fig.row2, nrow = 2)
    }
    fig
  }, ignoreNULL = FALSE)
  
  output$downloadData <- downloadHandler(
    filename = 'ordinal_prior.zip',
    content = function(fname) {
      getdir <- getwd()
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())

      fs <- c("01_p_estimates.csv", "02_Zquantile_inputs.csv", "03_marginal_beta_parameters.csv", "04_marginal_plots.pdf")
      write.table(last_phats(), file = "01_p_estimates.csv", sep =",", col.names = "p_est", row.names = FALSE)
      write.csv(quantileInputs(), file = "02_Zquantile_inputs.csv", sep =",", row.names = head(c("Z1", "Z2", "Z3", "Z4", "Z5", "Z6"), ncat()-1))
      write.csv(betaParamsfull(), file = "03_marginal_beta_parameters.csv", sep =",", row.names = head(c("Z1", "Z2", "Z3", "Z4", "Z5", "Z6"), ncat()-1))
      # write.csv(copParams(), file = "04_copula_parameters.csv", sep =",", row.names = FALSE)

      pdf(file = "04_marginal_plots.pdf",   # The directory you want to save the file in
          width = ncat() + 9, # The width of the plot in inches (12.41)
          height = 10) # The height of the plot in inches (10.7)

      plot(plotPDF())

      dev.off()

      print (fs)

      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
      setwd(getdir)
    },
    contentType = "application/zip"
  )
  
}


shinyApp(ui = ui, server = server)
