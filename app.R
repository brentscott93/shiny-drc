#devtools::install_github("OnofriAndreaPG/aomisc")
library(aomisc)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
#library(rlang)
library(drc)
library(readxl)
library(biophysr)
library(colourpicker)


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

#make id generator function
generate_id <- function(x, data, input){
  paste0(x, seq_along(unique(data[[input]])))
}

#start of the vanilla shiny app UI
ui <- fluidPage(#theme = shinytheme("flatly"),
    # Application title
    titlePanel("Dose-Response Curves"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(width = 3,
            radioGroupButtons(
                inputId = "drc_data",
                label = "Choose Data:",
                choices = c("Upload" = "user", 
                            "Built-in" = "drc_data"),
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
                choices = c("NULL", "ln", "10-log"),
                selected = "NULL",
                inline = TRUE, 
                checkbox = TRUE
            ),
            
           
            
            div(style="display:inline-block; width: 110px", 
                actionBttn(
                inputId = "fit_button",
                label = "Fit",
                style = "unite", 
                color = "danger",
                block = T)
            ),
            div(style="display:inline-block; width: 110px",
            
            downloadBttn("report", 
                         "Save",
                         style = "unite", 
                         color = "default",
                         block = F)
                     )
        ),

        #mainPanel
        mainPanel(
           h4("Your current drm function:"),
           verbatimTextOutput("mean_fun_desc"),
           
           tabsetPanel(type = "tabs",
                       tabPanel("Plot",
                          plotOutput("plot1"),
                          div(style="display: inline-block;vertical-align:top; width: 250px;",
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
                          div(style="display: inline-block;vertical-align:top; width: 250px;",
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
                              
                               div(style="display: inline-block;vertical-align:top; width: 250px;",
                                   dropdownButton(
                                     
                                     tags$h4("Select Colors"),
                                     radioGroupButtons(
                                       inputId = "use_custom_colors",
                                       label = "Color Scales",
                                       choices = c("B&W", "Default", "Custom"),
                                       justified = TRUE,
                                       checkIcon = list(
                                         yes = icon("ok", 
                                                    lib = "glyphicon"))
                                     ),
                                     
                                     conditionalPanel("input.use_custom_colors == 'Custom'",
                                                      
                                                      
                                                      uiOutput("plot_colors"),
                                                      sliderInput('alpha',
                                                                  'Transparency',
                                                                  min = 0, 
                                                                  max = 1,
                                                                  value = 1,
                                                                  step = 0.01)),
                                     
                                     circle = TRUE, status = "primary",
                                     icon = icon("palette"), width = "250px",
                                     
                                     tooltip = tooltipOptions(title = "Click to choose plot colors")
                                   )
                               ), #divclose
                               
                               div(style="display: inline-block;vertical-align:top; width: 250px;",
                                   dropdownButton(
                                     
                                     tags$h4("Select Aesthetics"),
                                     
                                     #linetype
                                     uiOutput('plot_lty'),
                                     
                                     #line thickness
                                     sliderInput(
                                       inputId = "lwd",
                                       label = "Line Thickness",
                                       min = 0.25,
                                       max = 6, 
                                       value = 1,
                                       step = 0.25
                                     ),
                                     
                                     #Point shape
                                     uiOutput('plot_pch'),
                                     
                                     
                                     #Point size
                                     sliderInput(
                                       inputId = "cex",
                                       label = "Point Size",
                                       min = .25,
                                       max = 6, 
                                       value = 1, 
                                       step = 0.25
                                     ),
                                     
                                     #smooth line
                                     sliderInput(
                                       inputId = "gridsize",
                                       label = "Smooth Line",
                                       min = 50,
                                       max = 5000, 
                                       value = 100, 
                                       step = 50
                                     ),
                                     circle = TRUE, status = "info",
                                     icon = icon("shapes"), width = "250px",
                                     
                                     tooltip = tooltipOptions(title = "Click to choose aesthetics")
                                   )), #divclose
                               
                               div(style="display: inline-block;vertical-align:top; width: 250px;",
                                   dropdownButton(
                                     
                                     tags$h4("Axes & Labels"),
                                     
                                     textInput(
                                       inputId = "xlab",
                                       label = "X-Axis Label",
                                       placeholder = "x-axis label here"
                                     ),
                                     
                                     textInput(
                                       inputId = "ylab",
                                       label = "Y-Axis Label",
                                       placeholder = "x-axis label here"
                                     ),
                                     
                                     sliderInput(
                                       inputId = "cex.lab",
                                       label = "Axis Label Size",
                                       min = 0.25,
                                       max = 6,
                                       value = 1.2,
                                       step = 0.25
                                     ),
                                     
                                     sliderInput(
                                       inputId = "cex.axis",
                                       label = "Axis tick label size",
                                       min = 0.25,
                                       max = 6,
                                       value = 1.25,
                                       step = 0.25
                                     ),
                                     
                                     uiOutput("xlim"),
                                     uiOutput("ylim"),
                                     
                                     
                                     sliderInput(
                                       inputId = "box_lwd",
                                       label = "Axis Thickness",
                                       min = 0.25,
                                       max = 6,
                                       value = 2,
                                       step = 0.25
                                     ),
                                     
                                     radioGroupButtons(
                                       inputId = "log_dose_plot2",
                                       label = "Log Scaling",
                                       choices = c("off", "x", "y", "xy"),
                                       justified = TRUE,
                                       checkIcon = list(
                                         yes = icon("ok", 
                                                    lib = "glyphicon"))
                                     ),
                                     
                                     circle = TRUE, status = "danger",
                                     icon = icon("chart-bar"), width = "250px",
                                     
                                     tooltip = tooltipOptions(title = "Click for axis options")
                                     
                                   )), #divclose
                               
                            
                               plotOutput("drm_plot", width = "100%", height = "100%"),
                               
                               div(style="display: inline-block;vertical-align:top; width: 250px;",
                                   dropdownButton(
                                     
                                     tags$h4("Legend Options"),
                                     
                                     switchInput(
                                       inputId = "legend",
                                       label = "Show Legend", 
                                       labelWidth = "260px",
                                       value = TRUE,
                                       onLabel = "Show",
                                       offLabel = "Hide",
                                       onStatus = "success",
                                       offStatus = "danger"
                                     ), 
                                     
                                     uiOutput("legend_text"),
                                     
                                     sliderInput(
                                       inputId = "cex.legend",
                                       label = "Text Size",
                                       min = 0.25, 
                                       max = 5,
                                       value = 1,
                                       step = 0.25
                                     ),
                                     
                                     
                                     uiOutput('legend_position'),
                                     
                                     circle = TRUE, status = "warning",
                                     icon = icon("info-circle"), width = "250px",
                                     
                                     tooltip = tooltipOptions(title = "Click to choose legend options")
                                   ) #dropdown button close
                               ), #divclose
                               
                               div(style="display: inline-block;vertical-align:top; width: 250px;",
                               dropdownButton(
                                 
                                 tags$h4("Plot Dimensions"),
                                 
                                 sliderInput(inputId = "plot_width",
                                            label = "Plot Width", 
                                              min = 200,
                                              max = 2000,
                                              value = 1000,
                                              step = 25),
                                 
                                 sliderInput( inputId = "plot_height",
                                              label = "Plot Height", 
                                             min = 200,
                                             max = 2000,
                                             value = 500,
                                             step = 25),
                                 
                                 downloadBttn("plot_down", 
                                              "Download Plot",
                                              style = "unite", 
                                              color = "default",
                                              block = T),
                                 
                                 circle = TRUE, status = "success",
                                 icon = icon("ruler-combined"), width = "250px",
                                 
                                 tooltip = tooltipOptions(title = "Click to choose plot colors")
                               )
                               ) #divClose
                    
                               
                        
                        
                      ) #tabPanel close
                         
                       
                         
                         
                     
                      
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
    
    
    ##############plot inputs################
   
    num_groups <- reactive({
      seq_along(unique(data()[[input$curveid]]))
    })
    
  color_id <- reactive({
   generate_id('color', data(), input$curveid)
    })
    
    group_names_label <- reactive({
         unique(data()[[input$curveid]])
      }) 
    
    output$plot_colors <- renderUI({
      map2(color_id(), group_names_label(), ~ colourInput(.x, label = .y, value = "black"))
    })
    
   user_colors <- reactive({
      if(input$use_custom_colors == 'Custom'){
        req(input$color1)
        col <- map_chr(color_id(), ~input[[.x]])
        alpha(col, input$alpha)
      } else if(input$use_custom_colors == 'Default') {
        TRUE
      } else {
        FALSE
      }
    })
    
    lty_id <- reactive({
      generate_id('lty', data(), input$curveid)
      
    })
    
    default_lty <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "blank")
    
    output$plot_lty <- renderUI({
      pmap(list(lty_id(), 
           paste0('Line type (', group_names_label(), ')'), 
           num_groups()),
           function(first, second, third){
             
            sliderTextInput(
                             inputId = first,
                             label = second, 
                             grid = FALSE,
                             force_edges = TRUE,
                             choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "blank"),
                             selected = default_lty[[third]])
           }
          )
      
    })
    
    user_lty <- reactive({
      if(is.null(input$lty1)){
        rep(1:6, 2)
      } else {
     line <-  map_chr(lty_id(), ~input[[.x]])
     
     line_num <- match(line, default_lty) %>% 
       ifelse(. == 7, 0, .)
      }
    })
    
    #pointshape
    
    pch_id <- reactive({
      generate_id('pch', data(), input$curveid)
    })
    
    output$plot_pch <- renderUI({
      pmap(list(pch_id(), 
                paste0('Point shape (', group_names_label(), ')'), 
                num_groups()),
           function(first, second, third){
             
             sliderInput(
               inputId = first,
               label = second, 
               min = 0,
               max = 25,
               value = third)
           }
      )
      
    })
    
    user_pch <- reactive({
      if(is.null(input$pch1)){
        1:25
      } else {
      map_int(pch_id(), ~input[[.x]])
    }
    })
    
    #xlim sliderInput
    
    xlim_max <- reactive({
      max(data()[[input$dose]])
    })
    
    output$xlim <- renderUI({
      sliderInput('xlim',
                  'X-Axis Upper Limit',
                  min = -(xlim_max()/4),
                  max = xlim_max()*1.5,
                  value = c(0, xlim_max()))
    })
    
    #ylim sliderInput
    
    ylim_max <- reactive({
      max(data()[[input$response]])
    })
    
    output$ylim <- renderUI({
      sliderInput('ylim',
                  'Y-Axis Upper Limit',
                  min = -(ylim_max()/4),
                  max = ylim_max()*1.5,
                  value = c(0, ylim_max()))
    })
    
    xlab <- reactive({
      if(is.null(input$xlab)){
        input$dose
        } else if(nchar(input$xlab) == 0){
        input$dose
      } else {
        input$xlab
      }
    })
    
    xlim <- reactive({
      if(is.null(input$xlim)){
        c(0, xlim_max())
      } else {
        input$xlim
      }
    })
    
  ylim <- reactive({
    if(is.null(input$ylim)){
    c(0,  ylim_max())
    } else {
      input$ylim
    }
  
  })


    ylab <- reactive({
      if(is.null(input$ylab)){
        input$dose
     } else if(nchar(input$ylab) == 0){
        input$dose
      } else {
        input$ylab
      }
    })
    #legend textboxes
    
    # legend_text_id <- reactive({
    #   generate_id('legend_text', data(), input$curveid)
    # })
    # 
    # output$legend_text <- renderUI({
    #   pmap(list(legend_text_id(), 
    #        paste0('Legend Label (', group_names_label(), ')'),
    #        group_names_label()),
    #        function(first, second, third){
    #        textInput(first, second, value = third, placeholder = 'Variable Name')
    #          }
    #        )
    # })
    # 
    # user_legendText <- reactive({
    #   if(is.null(legend_text1)){
    #     
    #   } else {
    #   map_chr(legend_text_id(), ~input[[.x]])
    #   }
    # })
    
    output$legend_position <- renderUI({
      tagList(
      sliderInput('legend_position_x', 'Legend Position (X)',
                  min = -(xlim_max()/4),
                  max = xlim_max()*1.5,
                  value = xlim_max()),
      
      sliderInput('legend_position_y', 'Legend Position (Y)',
                  min = -(ylim_max()/4),
                  max = ylim_max()*1.5,
                  value = ylim_max())
                 
      )
    })
    
    legend_position_x <- reactive({
      if(is.null(input$legend_position_x)){
        xlim_max() * 0.9
      } else {
        input$legend_position_x
      }
    })
    
    legend_position_y <- reactive({
      if(is.null(input$legend_position_y)){
        ylim_max() * 0.9
      } else {
        input$legend_position_y
      }
    })
    
    observe({
     output$drm_plot <- renderPlot(height = as.numeric(input$plot_height), width = as.numeric(input$plot_width),{
         req(fit())
         plot(fit(), 
              type = "all",
              #colors 
              col = user_colors(),#user
              #& aesthetics
              lty = user_lty(), #user - linetype
              lwd = as.numeric(input$lwd), #line thickness
              pch = user_pch(), #user - points style
              cex = as.numeric(input$cex), #user
              gridsize = as.numeric(input$gridsize),#, #user - smoothnest of fit
             # #axes & labels
             log = plot2_log_scale(),#user
             cex.lab = as.numeric(input$cex.lab), #user
             cex.axis = as.numeric(input$cex.axis), #user
             xlab = xlab(), #user
             xlim = xlim(), #user
             ylab = ylab(), #user
             ylim = ylim(),#user
              #legend
              legend = input$legend, #user
              #legendText = user_legendText(), #user
              legendPos = c(legend_position_x(), legend_position_y()), #user
              cex.legend = as.numeric(input$cex.legend), #user
              bty = "l") #user
              box(lwd = input$box_lwd, bty = "l") #user
            
    })
    })
  
 
    # downloadHandler contains 2 arguments as functions, namely filename, content
    output$plot_down <- downloadHandler(
      filename =  function() {
        paste("drc.png")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
       
          png(file, height = as.numeric(input$plot_height), width = as.numeric(input$plot_width), units = "px") # open the png device
      
        plot(fit(), 
             type = "all",
             #colors 
             col = user_colors(),#user
             #& aesthetics
             lty = user_lty(), #user - linetype
             lwd = as.numeric(input$lwd), #line thickness
             pch = user_pch(), #user - points style
             cex = as.numeric(input$cex), #user
             gridsize = as.numeric(input$gridsize),#, #user - smoothnest of fit
             # #axes & labels
             log = plot2_log_scale(),#user
             cex.lab = as.numeric(input$cex.lab), #user
             cex.axis = as.numeric(input$cex.axis), #user
             xlab = xlab(), #user
             xlim = xlim(), #user
             ylab = ylab(), #user
             ylim = ylim(),#user
             #legend
             legend = input$legend, #user
             #legendText = user_legendText(), #user
             legendPos = c(legend_position_x(), legend_position_y()), #user
             cex.legend = as.numeric(input$cex.legend), #user
             bty = "l") #user
        box(lwd = input$box_lwd, bty = "l") #user
        
        
        
        
        
          dev.off()  # turn the device off
        
      } 
    )
    
   
     
     
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
                            col = user_colors(),#user
                            #& aesthetics
                            lty = user_lty(), #user - linetype
                            lwd = as.numeric(input$lwd), #line thickness
                            pch = user_pch(), #user - points style
                            cex = as.numeric(input$cex), #user
                            gridsize = as.numeric(input$gridsize),#, #user - smoothnest of fit
                            # #axes & labels
                            log = plot2_log_scale(),#user
                            cex.lab = as.numeric(input$cex.lab), #user
                            cex.axis = as.numeric(input$cex.axis), #user
                            xlab = xlab(), #user
                            xlim = xlim(), #user
                            ylab = ylab(), #user
                            ylim = ylim(),#user
                            #legend
                            legend = input$legend, #user
                            #legendText = user_legendText(), #user
                            legendPos = c(legend_position_x(), legend_position_y()), #user
                            cex.legend = as.numeric(input$cex.legend),
                            box_lwd = input$box_lwd)
             
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


