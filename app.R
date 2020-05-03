#devtools::install_github("OnofriAndreaPG/aomisc")
library(aomisc)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(drc)
library(biophysr)


#get the mean function (drm self-starters) in the 'drc' package
drc_mean_fun <- drc::getMeanFunctions()
mean_fun_abrv <- sapply(drc_mean_fun, "[[", 1)
mean_fun_desc <- lapply(drc_mean_fun, "[[", 2)
names(mean_fun_desc) <- mean_fun_abrv

#get the built-in datasets in the 'drc' package
datasets <- as.data.frame(data(package = "drc")$results)
datasets$Item <- as.character(datasets$Item)

#the 'aomisc' package has additional mean function/self-starts for drm()
aomisc_starters <- lsf.str("package:aomisc")
is_drc <- str_detect(aomisc_starters, "DRC.")
aomisc_starters <- aomisc_starters[which(is_drc == TRUE)]

#get 'biophysr' drc starters
biophysr_starters <- lsf.str("package:biophysr")
is_drc2 <- str_detect(biophysr_starters, "drc.")
biophysr_starters <- biophysr_starters[which(is_drc2 == TRUE)]

#combine names of all self-starters
all_drc_starters <- sort(c(mean_fun_abrv, aomisc_starters, biophysr_starters))


#start of the vanilla shiny app UI
ui <- fluidPage(#theme = shinytheme("flatly"),
  # Application title
  titlePanel("Dose-Response Curves"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      radioGroupButtons(
        inputId = "drc_data",
        label = "Choose Data:",
        choices = c("Upload my tidy data" = "user", 
                    "Use built-in data" = "drc_data"),
        justified = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square", 
                       style = "color: green"),
          no = tags$i(class = "", 
                      style = ""))
      ),
      conditionalPanel("input.drc_data == 'user'",
                       fileInput(inputId = "file_input",
                                 label = "Upload File (.csv)",
                                 multiple = FALSE,
                                 accept = c(".csv"))),
      
      conditionalPanel("input.drc_data == 'drc_data'",
                       pickerInput(inputId = "drc_dataset_picker",
                                   label = "Choose a 'drc' dataset", 
                                   choices = sort(datasets$Item),
                                   selected = NULL,
                                   options = list(
                                     `live-search` = TRUE,
                                     size = 10)
                       )),
      
      uiOutput("dose"),
      uiOutput("response"),
      prettySwitch(
        inputId = "group_switch",
        label = "Multiple groups?", 
        status = "success",
        fill = TRUE
      ),
      uiOutput("curveid"),
      pickerInput(inputId = "equation",
                  label = "Equation (fct)", 
                  choices = all_drc_starters,
                  options = list(
                    `live-search` = TRUE,
                    size = 10)
      ),
      
      awesomeRadio(
        inputId = "log_dose",
        label = "Log Dose", 
        choices = c("NULL", "ln", "10-logarithm"),
        selected = "NULL",
        inline = TRUE, 
        checkbox = TRUE
      ),
      
      
      
      div(style="display:inline-block; width: 150px", 
          actionBttn(
            inputId = "fit_button",
            label = "Fit & Plot",
            style = "unite", 
            color = "danger",
            block = T)
      ),
      div(style="display:inline-block; width: 150px",
          
          downloadBttn("report", 
                       "Download",
                       style = "unite", 
                       color = "default",
                       block = T)
      )
    ),
    
    #mainPanel
    mainPanel(
      h4("Your current drm function:"),
      verbatimTextOutput("mean_fun_desc"),
      
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",
                           plotOutput("plot1"),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               conditionalPanel("input.response != null &&
                                             input.response.length > 0 ",
                                                awesomeRadio(
                                                  inputId = "log_dose_plot1",
                                                  label = "Plot axis log scaling", 
                                                  choices = c("off", "x", "y", "xy"),
                                                  selected = "off",
                                                  inline = TRUE, 
                                                  checkbox = TRUE
                                                ))),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               conditionalPanel("input.response != null &&
                                             input.response.length > 0 ",
                                                sliderInput(
                                                  inputId = "plot1_cex",
                                                  label = "Magnify (cex)", 
                                                  min = 0.5,
                                                  max = 2,
                                                  value = 1,
                                                  step = 0.25
                                                ))),
                  ),
                  
                  tabPanel("Summary",
                           verbatimTextOutput("summary")
                  ),
                  
                  tabPanel("Plot.drm",
                           plotOutput("drm_plot"),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               conditionalPanel("input.response != null &&
                                              input.response.length > 0",
                                                awesomeRadio(
                                                  inputId = "log_dose_plot2",
                                                  label = "drm plot log scaling", 
                                                  choices = c("off", "x", "y", "xy"),
                                                  selected = "off",
                                                  inline = TRUE, 
                                                  checkbox = TRUE
                                                ))),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               conditionalPanel("input.response != null &&
                                             input.response.length > 0 ",
                                                sliderInput(
                                                  inputId = "drm_plot_cex",
                                                  label = "Magnify (cex)", 
                                                  min = 0.5,
                                                  max = 2,
                                                  value = 1,
                                                  step = 0.25
                                                ))),
                           div(style="display: inline-block;vertical-align:top; width: 200px;",
                               conditionalPanel("input.response != null &&
                                             input.response.length > 0 ",
                                                sliderInput(
                                                  inputId = "drm_plot_lwd",
                                                  label = "Line thickness (lwd)", 
                                                  min = 0.5,
                                                  max = 4,
                                                  value = 2,
                                                  step = 0.25
                                                ))),
                  )
      ) #tabsetPanel close 
    )#mainPanel close
  )#sidebar layout close
) #ui close

# Define server logic 
server <- function(input, output) {
  
  #get user uploaded data
  user_data <- eventReactive(input$file_input,{
    read.csv(input$file_input$datapath)
  })
  
  #get selected built-in dataset
  drc_data <- eventReactive(input$drc_dataset_picker,{
    chosen_data <- input$drc_dataset_picker
    get(chosen_data)
  })
  
  #select user data or built-in data
  data <- reactive({
    if(input$drc_data == 'drc_data'){
      drc_data()
    } else {
      req(input$file_input)
      user_data()
    }
  })
  
  #make the selectInputs for 'dose' based on loaded dataset
  output$dose = renderUI({
    selectInput('dose',
                "Dose (independent variable)",
                c(Choose='', colnames(data())),
                selectize = TRUE,
                width ="100%")
  })
  
  
  
  #make the selectInputs for 'response' based on loaded dataset
  output$response <- renderUI({
    selectInput('response',
                "Response (dependent variable)",
                c(Choose='', colnames(data())),
                selectize = TRUE,
                width ="100%")
  })
  
  #make the selectInputs for 'curveid' based on loaded dataset
  output$curveid <- renderUI({
    if(req(input$group_switch) == TRUE){
      selectInput('curveid',
                  "Groups (curveid)",
                  c(Choose='', colnames(data())),
                  selectize = TRUE,
                  width ="100%")
    }
  })
  
  plot_data_no_fit <- reactive({
    if(input$group_switch == FALSE){
      req(input$dose, input$response)
      data() %>% 
        dplyr::select(input$dose, input$response)
    } else {
      req(input$curveid)
      data() %>% 
        dplyr::select(input$dose, input$response, input$curveid)
    }
  })
  
  
  drm_data <- reactive({
    req(input$dose, input$response)
    resp <- input$response
    dos <-  input$dose
    data.frame(resp = data()[,input$response],
               dos = data()[,input$dose])
  })
  
  
  
  plot1_log_scale <- reactive({
    ifelse(input$log_dose_plot1 == "off", "", input$log_dose_plot1)
  })
  
  output$plot1 <- renderPlot({
    req(input$dose, input$response)
    if(input$group_switch == FALSE){
      
      plot(plot_data_no_fit(), 
           log = plot1_log_scale(), 
           cex = as.numeric(input$plot1_cex),
           cex.axis = as.numeric(input$plot1_cex), 
           cex.lab = as.numeric(input$plot1_cex))
      
    } else {
      
      plot(x = plot_data_no_fit()[,1], 
           y = plot_data_no_fit()[,2],
           col = plot_data_no_fit()[,3],
           xlab = input$dose,
           ylab = input$response,
           log = plot1_log_scale(), 
           cex = as.numeric(input$plot1_cex),
           cex.axis = as.numeric(input$plot1_cex), 
           cex.lab = as.numeric(input$plot1_cex))
      
    }
  })
  
  log_dose_number <- reactive({
    if(input$log_dose == "NULL"){
      "NULL"
    } else if(input$log_dose == "ln"){
      "exp(1)"
    } else {
      "10"
    }
  })
  
  log_dose_drm <- reactive({
    if(input$log_dose == "NULL"){
      NULL
    } else if(input$log_dose == "ln"){
      exp(1)
    } else {
      10
    }
  })
  
  output$mean_fun_desc <- renderText({
    req(data())
    if(input$drc_data == 'drc_data'){
      if(input$group_switch == FALSE){
        paste0("drm(", 
               input$response, 
               " ~ ", 
               input$dose,
               ", data = ",
               input$drc_dataset_picker, 
               ",", " fct = ", 
               input$equation, "(), ",
               "logDose = ",
               log_dose_number(),
               ")")
      } else {
        paste0("drm(", 
               input$response, 
               " ~ ", 
               input$dose,
               ", curveid = ",
               input$curveid,
               ", data = ",
               input$drc_dataset_picker, 
               ",", " fct = ", 
               input$equation, "(), ",
               "logDose = ",
               log_dose_number(),
               ")")
      }} else {
        if(input$group_switch == FALSE){
          paste0("drm(", 
                 input$response, 
                 " ~ ", 
                 input$dose,
                 ", data = ",
                 str_sub(input$file_input$name, 1, -5), 
                 ",", " fct = ", 
                 input$equation, "(), ",
                 "logDose = ",
                 log_dose_number(),
                 ")")
        } else {
          paste0("drm(", 
                 input$response, 
                 " ~ ", 
                 input$dose,
                 ", curveid = ",
                 input$curveid,
                 ", data = ",
                 str_sub(input$file_input$name, 1, -5), 
                 ",", " fct = ", 
                 input$equation, "(), ",
                 "logDose = ",
                 log_dose_number(),
                 ")")
          
        }
        
        
      }
  })
  
  
  drm_curve <- reactive({
    c <- sym(input$curveid)
    pull(data(), c)
  })
  
  
  #fit curves
  fit <- eventReactive(input$fit_button, {
    
    fitting <- if(input$group_switch == FALSE){
      
      drm(paste(input$response, "~", input$dose), 
          fct = eval(call(input$equation)),
          data = data(),
          logDose = log_dose_drm())
      
    } else if(input$group_switch == TRUE){
      
      c <- input$curveid
      df <- data() %>% 
        mutate(curveid = data()[,c] )
      
      drm(paste(input$response, "~", input$dose), 
          curveid = curveid,
          data = df,
          fct = eval(call(input$equation)),
          logDose = log_dose_drm())
    }
    
  })
  
  observeEvent(fit(),{
    showNotification("Dose-Response curve fit. See 'Summary' or 'Plot.drm' tab for output",
                     type = "message")
  })
  
  output$summary <- renderPrint({
    req(fit())
    summary(fit())
  })
  
  plot2_log_scale <- reactive({
    ifelse(input$log_dose_plot2 == "off", "", input$log_dose_plot2)
  })
  output$drm_plot <- renderPlot({
    req(fit())
    plot(fit(), col = TRUE, 
         log = plot2_log_scale(),
         cex = as.numeric(input$drm_plot_cex),
         cex.lab = as.numeric(input$drm_plot_cex),
         cex.axis = as.numeric(input$drm_plot_cex),
         lwd = as.numeric(input$drm_plot_lwd)
    )
  })
  
  #Example from https://github.com/rstudio/shiny-examples/blob/master/112-generate-report/app.R
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(drm_obj = fit(),
                     cex = input$drm_plot_cex,
                     lwd = input$drm_plot_lwd,
                     log_scale = plot2_log_scale())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}



# Run the application 
shinyApp(ui = ui, server = server)


