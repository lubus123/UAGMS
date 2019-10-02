
library(XML)
library(leaflet.extras)
library(leaflet)
library(rgdal)
library(rvest)
library(shiny)
library(readxl)
library(shinyalert)
library(shinyWidgets)

library(readr)
library(dygraphs)

library(shinyjs)
library(shinycssloaders)
library(echarts4r)

library(shinydashboard)

header <- dashboardHeader(title = tags$a(href='http://nationalgrid.com',
                                    tags$img(src='nglogo.jpg',height='60',width='220')))

#tags$head(tags$link(rel="shortcut icon", href="https://ibb.co/p3ChdZP"))

sidebar <- dashboardSidebar(sidebarMenu(id = 'container',
  menuItem("UAG Baseline", tabName = "uag_monitor", icon = icon("bar-chart")),
  menuItem("Causality Detection", tabName = "day_2", icon = icon("calendar")),
  menuItem("LDZ Weather", tabName = "weather", icon = icon("sun")),
  menuItem("Changepoint Analysis", tabName = "cpt_tab", icon = icon("bar-chart")),
menuItem('Reporting', tabName = 'reporting', icon = icon('book')),
  menuItem("Data Configuration", tabName = "data_config", icon = icon("file-excel-o")),
menuItem('Help', tabName = 'about', icon =icon('question')),



############### UAG MONITOR SIDEBAR ########################

conditionalPanel("input.container === 'uag_monitor'",hr(),div(style="text-align:center",class= "h3","Options"),div(style="text-align:center",class= "h5","Daily UAG"),dateRangeInput('dateRange',label = 'Date range',start = Sys.Date() - 200, end = Sys.Date() -30
), 
  pickerInput(inputId = "analchoice", 
              label = "Filtration", 
              choices = c("Bollinger Bands","Fixed Limit","% Throughput", "D'Arpino(2014)",'Anomalize','ETS Forecast'), multiple = TRUE, 
              selected = "Fixed Limit"), hidden(materialSwitch(inputId = "show_d_hack2",value = TRUE,  label = "Display Overlay", 
                                                                status = "primary", right = TRUE)),
switchInput('mode_op_uag',label =  'Limits', value = TRUE, onLabel = 'Min', offLabel = 'Max', width = '150px'),
conditionalPanel("input.show_d_hack2 &&input.analchoice.includes('Bollinger Bands')",sliderInput('bol_sd','Bollinger Band s.d.', min = 0.5,max=5,step = 0.25,value = 2) ),

conditionalPanel("input.show_d_hack2 &&input.analchoice.includes('Fixed Limit') ", sliderInput('gwh_lim_l','Lower Limit:' , min = -50, max = -0.5, step = 0.5, value = -20) ),
conditionalPanel("input.show_d_hack2 &&input.analchoice.includes('Fixed Limit') ", sliderInput('gwh_lim_u','Upper Limit:' , min = 0.5, max = 50, step = 0.5, value = 20) ),
conditionalPanel("input.show_d_hack2 &&input.analchoice.includes('% Throughput') ",
                 switchInput('switch_throughput_calc',label =  'Throughput', value = TRUE, onLabel = 'Supply', offLabel = 'Demand', width = '150px'),
                 sliderInput('throughput_control','Percentage Throughput' , min = 0.005, max = 0.1, step =0.005, value = 0.03) ),
div(
actionBttn('action','Explore Day',style='fill',block = TRUE),style="width: 87%; float:left"),div(
  actionBttn('batch','Batch Analysis',style='fill',block = TRUE),style="width: 87%; float:left"),br(), br(), br(),br(),hr(), div(style="text-align:center",class= "h5","Aggregate UAG")
,pickerInput('aggregate_function', 'Aggregate Function', choices = c('Sum', 'Abs Sum'), selected = 'Sum'),
pickerInput('aggregate_time', 'Aggregate Level', choices = c('Weekly', 'Monthly','Quarterly', 'Yearly'), selected = 'monthly')
),
################## LDZ VIEW SIDEBAR #####################






conditionalPanel(background = 'light-blue',"input.container === 'weather'",hr(),div(style="text-align:center",class= "h3","Options"),
                 pickerInput('LDZ_pick', 'LDZ', unique(dllist$LDZ)[-1]),
                 pickerInput('Offtake_pick', 'Offtake', dllist[dllist$Stype=='NTS Offtake',]$Name, options = list(
                   `live-search` = TRUE,  size =10)	), dateInput('Date_weather', 'Date', '2019-01-01')
                 
                 
                 
                 
                 
                 ),
###################### DAY EXPLORER SIDEBAR ########################


conditionalPanel(background = 'light-blue',"input.container === 'day_2'",hr(),div(style="text-align:center",class= "h3","Options"),
  #materialSwitch(inputId = "atr_filter",value = FALSE,  label = "Display Overlay", status = "primary", right = TRUE),
pickerInput(inputId = "flag_filter", label = 'Flags',
              
              choices = c("All","Interrupts","Anomalies (Fast)",'Quantile','Percentage Change', 'Extreme','Anomalies (Slow)', "LDZ LM"), multiple = TRUE, 
              selected = c("Interrupts",'Quantile','Extreme')), conditionalPanel('input.flag_filter.includes("Percentage Change") ', sliderInput('p_c_control', 'Absolute Percentage', 1, 100, 50)),
dateInput('analysis_date','Select Date',value = Sys.Date() -30)

        ,div(actionBttn('calculate_anom','Calculate',block=TRUE, style = 'fill'),style="width: 87%; float:left")
  , br(),br(),hr(),  pickerInput(inputId = 'past_analis', label = 'Analysis List', choices=c('No Analysis')),
conditionalPanel("!input.past_analis.includes('No Analysis')",
div( downloadBttn('download_analyis','Download Analysis',block=TRUE, style = 'fill'),style="width: 87%; float:left")
)


)
))




#tags$head(tags$style(HTML(".small-box {height: 90px}"))),   solved bug 
#tags$head(tags$style(HTML('.sidebar {overflow-y: scroll}'))),  also adds permanent sidebar scroller :()
body <- dashboardBody(  useShinyalert(),tags$head(tags$script(HTML('
                           Shiny.addCustomMessageHandler("jsCode",
                                                                  function(message) {
                                                                  eval(message.value);
                                                                  });'))),
  useShinyjs(),
  div(align="center", style="border: thin solid black",
    id = "loading_page",
    h1("Loading...")
  ),
  hidden(
    div(
      id = "main_content",
      
      tabItems(
        tabItem(
          ################################################### DAY ANALYSIS ######################################
          
          tabName='day_2',
          
          
          
          
          hidden(div(
          id = "main_content2", materialSwitch(inputId = "show_d_hack",value = FALSE,  label = "Display Overlay", 
            status = "primary", right = TRUE))),conditionalPanel(condition = "!input.show_d_hack",
                      fluidRow(align='center',h3('Please select date for analysis'))), 
          conditionalPanel(condition= "input.show_d_hack",fluidRow(valueBoxOutput('day_i'), valueBoxOutput('uag_v'), valueBoxOutput('excess_u'))
         ,fluidRow( box(width = 8,dygraphOutput('test12')%>%withSpinner()), box(width = 4, leafletOutput("map")%>%withSpinner()))
               
          ,fluidRow(box(title = 'Site Selection',status = 'primary',width = 8,  DT::dataTableOutput('node_select')%>%withSpinner()) 
                ,
                box(title = 'Summary',status= 'primary', width =4,div(align="center",h4('Total Flags')), DT::dataTableOutput('Flag_summary'),div(align="center",h4('Site Statistics')),
                    DT::dataTableOutput('node_info'),
                    conditionalPanel('input.switch_exit && input.switch_entry', div(align="center",h4('Site Info')), 
                                     DT::dataTableOutput('Q_info')) )
                  ,fluidRow(box(title = 'Grouped Sites',
                                                    status = 'info', DT::dataTableOutput('secondary_sel') %>% withSpinner()))
                )        
                )),
        
        ################################################### REPORTING ######################################
        
        
        tabItem(tabName = 'reporting',
                
                
                
                box(status='primary',title ='Reporting',width =12,column(width = 11,fluidRow(
                                                           dateRangeInput('dateRange_reporting',
                                                                          label = 'Date range input: yyyy-mm-dd',
                                                                          start = Sys.Date() - 200, end = Sys.Date() -30
                                                           ),
                pickerInput('report_choices','Select Report Components',selected='All', choices = c('All','Daily Balancing Errors', 'Changepoints','Historic Performance','Flow Visualisations'),multiple = TRUE),
                helpText('Information from the app can be summarised into a downloadable report. Select the desired date range and the required analytical componenets, following which a downloadable file will be made available. Note that this operation may take a while if a large time frame is selected. NOTE: Pop-ups must be enabled to allow downloading.'),
                downloadLink("downloadData",label=""),
                actionButton("button_DL", "Download")))), box(status='info', width = 12,title='Data Export', helpText('You can download the source datasets used by the app, which are updated from the National Grid MIPI service, along with Shrinkage Data'
                ,column(width = 11,fluidRow(downloadButton('dl_balance', 'Download Shrinkage'), downloadButton('dl_exit', 'Download Exit'), downloadButton('dl_entry', 'Download Entry')))))
                
                
                
                
                
                ),
        ################################################### ABOUT ######################################
        
        tabItem(tabName = "about",
                
                
                
                fluidRow( box(status='primary',title ='Help',width =12,column(width = 11,height = 1000,htmlOutput('userguide'))),box(status='primary',title ='About',width =12,column(width = 11, 
                                                                                                                                                                                                         fluidRow(
                                                                                                                                                                                                           align = "center",h1('UAGMS'),h4('Unnacounted for Gas Management Suite'),
                                                                                                                                                                                                           'Version Alpha , build ',br(),'For Queries, contact lubomir.botev@manchester.ac.uk',
                                                                                                                                                                                                           br(), 'Analytics performed in  R (GPL-2 | GPL-3)', br(), 'Developed at the University of Manchester',br(),tags$img(src='mcrlogo.jpg',height='60',width='160'),br(),'2019')))
                )),
        
        ################################## WEATHER/ LDZ/ LM ####################################
        tabItem(tabName = 'weather',fluidRow(),
                
                fluidRow(box('Weather','primary',width = 6,echarts4rOutput("LDZ_W")),box('LDZ Sum', 'primary', width = 6,echarts4rOutput("LDZ_TOT"))),
                fluidRow(box('Node','primary', width = 12, echarts4rOutput('Node_LDZ')))
                
        ),
        
        ################################################### UAG MONITOR ######################################
        
        tabItem(tabName = "uag_monitor",
                
                fluidRow(valueBoxOutput("uag_prc"),valueBoxOutput("uag_num"),valueBoxOutput("uag_t")),
            fluidRow(
                  box(title = 'Daily UAG',status = 'primary',echarts4rOutput("echartsu"), width = NULL)),fluidRow(
     box(height = '540px', title = 'UAG days exceeding limits',width = NULL,status = 'primary',
                               DT::dataTableOutput('ex'))), fluidRow(box(title = 'Aggregate UAG',status  = 'info',width = NULL, echarts4rOutput('aggregate'))))
                 
                 
              ,
     
     ################################################### CHANGEPOINTS TAB ######################################
        tabItem(tabName = "cpt_tab",
                
                fluidRow(box(title = 'Bayesian changepoint analysis', plotOutput('bcp')%>%withSpinner()), box(title = 'Cumulative sum control chart', plotOutput('tso')%>%withSpinner()))
                ,
                fluidRow(box(title = 'Changepoint Plot', (plotOutput('cpt_output')%>%withSpinner())),box(title = 'Summary', tableOutput('Locations'),textOutput('sumr'), selectInput('cpt_MODE','Changepoint Method:',c('PELT (Recommended)','Bin Seg (Allows control)')),sliderInput('control_cpt3', 'Binseg Max Changepoints', 1, 20, 5), sliderInput('control_cpt2', 'BCP p0', 0, 1, 0.2),sliderInput('control_cpt', 'CUSUM SD Shift', 0.3, 3, 1)))
                
        ),
     
     
     ################################################### DATA CONFIG ######################################
        tabItem(tabName = "data_config",
                # fluidPage(
                fluidRow(shinyjs::useShinyjs(),
                         
                         box(title = 'Data Summary', status = "primary", width = 7,
                             "The source, start and end dates along with the date of last synchronisation are displayed below;",
                             tableOutput('tb2'),textOutput('summary_text'))
                         ,
                         
                         box(title = 'Synchronisation', status = 'info', width = 5,p('Entry/Exit Data is sourced from the MIPI. Shrinkage from NG website'),
                             tableOutput('sync_table'),actionButton('syncbutton', "Sync")
                             
                             
                             
                         )
                         
                         
                ),
                fluidRow(
                  column(4, box(title= 'UAG/Shrinkage Source', width = NULL, 
                                materialSwitch(inputId = "id",value = TRUE,  label = "Use Online Data", 
                                               status = "primary", right = TRUE),
                                conditionalPanel(
                                  ### report hidden here##
                                  condition = "!input.id",selectInput('inp2','Select Column',c('UAG')),
                                  fileInput("file1", "Choose Shrinkage .csv File",
                                            accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                                  ),helpText('Select file. This needs to have the Date in the first column, and header names.')
                                )
                  )
                  ),
                  column(4,  box(title= 'Entry Source', width = NULL, 
                                 materialSwitch(inputId = "switch_entry",value = TRUE,  label = "Use Online Data", 
                                                status = "primary", right = TRUE),
                                 conditionalPanel(
                                   condition = "!input.switch_entry",
                                   fileInput("file_entry", "Choose Exit Node .csv File",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")
                                   ),helpText('Select file. This needs to have the Date in the first column, and header names.')
                                 )
                  )
                  ),
                  column(4,  box(title= 'Exit Source', width = NULL, 
                                 materialSwitch(inputId = "switch_exit",value = TRUE,  label = "Use Online Data", 
                                                status = "primary", right = TRUE),
                                 conditionalPanel(
                                   condition = "!input.switch_exit",
                                   fileInput("file_exit", "Choose UAG .csv File",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")
                                   ),helpText('Select file. This needs to have the Date in the first column, and header names.')
                                 )
                  )
                  )
                  
                  
                  
                  
                  
                  
                  #      )
                )
                
                
                
        )
        
        
      )
      
    )
  )
  
  
  
    
    
    
    
  )
  


dashboardPage(header, sidebar, body, title = 'UAGMS')