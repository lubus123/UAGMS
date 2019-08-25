library(leaflet.extras)
library(leaflet)
library(rgdal)
library(XML)
library(rvest)
library(stringi)
library(shiny)
library(kableExtra)
library(pracma)
library(shinyalert)
library(knitr)
library(readxl)
library(shinyWidgets)
library(tinytex)
library(xts)
library(dplyr)
library(purrr)
library(stringr)
library(changepoint)
library(readr)
library(dygraphs)
library(ecp)
library(bcp)
library(countup) ##### EXPERIMENTAL
library(ggplot2)
library(shinyjs)
library(shinycssloaders)
library(echarts4r)
library(readr)
library(tsoutliers)
library(shinydashboard)
library(qcc)
library(tibble)
suppressMessages(library(forecast))
library(timetk)
library(tibbletime)
library(anomalize)
server <- function(input, output,session) {
  show_d<<- 0
  focusday2= list()
  analytics_holder = list()
  firstb = TRUE

  getactive = function(){if(!input$id & !is.null(input$file1)){
   
    
    return(data())
  }
    else{
     
      return(ttx)
    }}
  output$display_day = renderText({
   
    return(show_d)})
  outputOptions(output, "display_day", suspendWhenHidden = FALSE,priority = 2)
  getexit = function(){
    if(!input$switch_exit & !is.null(input$file_exit))
    {
      return(data_exit())
    }
    else{
      return(xxts)
    }
  }
  getentry = function(){
    if(!input$switch_entry & !is.null(input$file_entry))
    {
      return(data_entry())
    }
    else{
      return(nxts)
    }
  }
  
  
  
  
  
  
  r = getOption("repos")
  r["CRAN"] = "http://cran.us.r-project.org"
  options(repos = r)
  
  
  source('functions.R') ## Functions used throughout
  source('download.R') ## Download function
  source('dataloading.R') ## data initialisation
  
  
  highlight <- function(x, value, col.value, col=NA, ...){
    hst <- hist(x, ...)
    idx <- findInterval(value, hst$breaks)
    cols <- rep(col, length(hst$counts))
    cols[idx] <- col.value
    hist(x, col=cols, ...)
  }
   pipe = readOGR('.', 'pipedata')
    
    iconSet = pulseIconList(
      red = makePulseIcon(color = "#ff0000",heartbeat = 1),
      blue = makePulseIcon(color = "#0000ff", heartbeat =1, iconSize= 20)
    )
    iconSet[c("red", "blue")]
    m =leaflet() %>% addTiles()%>% addPolylines(data= pipe)
    
    output$map <- renderLeaflet({
     m
    })
    
    observeEvent(input$node_select_rows_selected,{
      
     if(getmode())
     {
       leafletProxy("map") %>%  clearMarkers() %>% addPulseMarkers(
         lng = as.numeric(dllist[which(match(dllist$Name,get_analytics()$Table[input$node_select_rows_selected,]$Name) ==1)[1],'lon']), lat = as.numeric(dllist[which(match(dllist$Name,get_analytics()$Table[input$node_select_rows_selected,]$Name) ==1)[1],'lat']),
         label = get_analytics()$Table[input$node_select_rows_selected,]$Name,
         icon = iconSet[1]  ) %>% setView(lng = as.numeric(dllist[which(match(dllist$Name,get_analytics()$Table[input$node_select_rows_selected,]$Name) ==1)[1],'lon']), lat = as.numeric(dllist[which(match(dllist$Name,get_analytics()$Table[input$node_select_rows_selected,]$Name) ==1)[1],'lat']), zoom = 6)  
     
     }
      else{
        leafletProxy("map") %>%  clearMarkers() 
      }
    })
    
    
    
    # ,  labelOptions=labelOptions(noHide = TRUE, textOnly = TRUE,style = list(
    #   "overflow" = "hidden",
    #   "border-right" = ".15em solid orange",
    #   "white-space" = "nowrap",rstu
    #   "margin" = "0 auto",
    #   "letter-spacing" = ".15em",
    #   "animation" = "typing 3.5 steps(40,end),blink-caret .75s step-end infinite"
    # )) 
    # 
    
    
      
      
    
 
  activeset = observeEvent(input$id,{
    ##This WILL NOT set range for manual data on first load.
    
    
 
    d = getactive()
    mind =end(d)- start(d)
    updateSelectInput(session, "inp2","Select Columns", choices = colnames(d))
    updateDateRangeInput(session,'dateRange',start = end(d)- min(120, 0.5*mind), end = end(d))
    
    if(firstb)
    {
      hide("loading_page")
      show("main_content",anim = TRUE, animType = "fade", time = 1)
      firstb = FALSE
      
      upd_lim  = 7
      #Check DB age, update if necesary#
      if( max(abs((end(nxts) - Sys.Date())),abs((end(xxts) - Sys.Date()))) >upd_lim & input$container !='data_config')
      {
        
        shinyalert('Database Outdated!','Press OK to go to Data Configuration',showCancelButton = TRUE,callbackR = function(x) { if(x != FALSE)  updateTabItems(session, 'container','data_config') })
      }
    }
  }) 
  
  

  
  
  get_analytics = reactiveVal({
    0
  })
  
  output$day_i = renderValueBox({valueBox(get_analytics()$Date,
                                          "Date", icon = icon("question"),
                                          color = "light-blue") })
  
  output$uag_v = renderValueBox({valueBox(paste0(get_analytics()$UAG,' GWh'),
                                          "UAG", icon = icon("question"),
                                          color = "light-blue") })
  
output$excess_u = renderValueBox({

  
  valueBox(paste0(get_analytics()$ExcessUAG,' GWh')
           
           ,
                                           "Excess", icon = icon("question"),
                                           color = "light-blue")
  
  })

  
  observeEvent(input$analysis_date,{
    if(input$analysis_date > end(getexit()) | input$analysis_date > end(getentry()) | input$analysis_date < start(getexit()) | input$analysis_date < start(getentry()))
    {
      shinyalert('Warning!', 'Selected date is outside of flow database range. Try updating the local DB, or uploading your own data!', type = 'warning')
      updateDateInput(session, 'analysis_date', value = end(getentry())-30)
    }
  })
  
  gentab = observeEvent(input$action,{
    if (is.null(input$ex_rows_selected)) {
      
      
      shinyalert('Warning!','Please select a day!', type = 'warning')
      return()}
    focusday =  updatefilter()[input$ex_rows_selected,1]
    
    focusday2[['Date']] <<-  updatefilter()[input$ex_rows_selected,1]
    focusday2[['UAG']] <<- round(updatefilter()[input$ex_rows_selected,2]/1e6,2)
    focusday2[['ExcessUAG']] <<- round(abs(20-abs(round(updatefilter()[input$ex_rows_selected,2]/1000000,2))),2)
      
    
    
    tv =  updatefilter()[input$ex_rows_selected,2]
    analytics_holder[[as.name(format(updatefilter()[input$ex_rows_selected,1]))]]<<-focusday2
    get_analytics(focusday2)
   Recalc_Tbl()
   get_analytics(analytics_holder[[length(analytics_holder)]])
updatePickerInput(session, 'past_analis',choices=(names(analytics_holder)), selected = names(analytics_holder)[length(names(analytics_holder))])
   
    updateTabItems(session, 'container','day_2')
    
    
  })
  
  

  
  observeEvent(input$past_analis,{
    a=1
    a=2
    if(length(analytics_holder) != 0)
    {
    if(input$past_analis %in% names(analytics_holder))
    {
   get_analytics(analytics_holder[[input$past_analis]])
    }
    }
  })
  
  getfocusperiod = function(x, width=10){
    
     
    v = which(index(getentry()) == updatefilter()[input$ex_rows_selected,1] )
    
    if(x)return(seq(v - width, v + min(width, nrow(getentry()) -v), by = 1))
  seq(v - width, v + min(width, nrow(getexit()) -v), by = 1)
    
  }
  getfocusday = function()
  {
    return(updatefilter()[input$ex_rows_selected,1])
  }
  get_nodes = function(){
    a= 1
    a=2
    if(!exists('FIN_T', envir = .GlobalEnv))
    {
      Recalc_Tbl()
      return(FIN_T)
    }
    else{
      return(FIN_T)
    }
  }
  
  
  output$test12 = renderDygraph( 
    {
      if(length(input$flag_filter) ==0 )
      {
        par(mar = c(0,0,0,0))
        return( {plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.5, paste("No data selected"), 
               cex = 1.6, col = "black")
        } )
      }
      ##HANDLE NULL SELECT CASE##
      ffs = dllist
      if(length(input$node_select_rows_selected)==0  | nrow(get_analytics()$Table)==0){
        par(mar = c(0,0,0,0))
        return( {plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.5, paste("Select a node to view detailed information."), 
               cex = 1.6, col = "black")
        } )
      }
      
      
      
      
      if(length(input$secondary_sel_rows_selected)>0)
      {
        if( input$secondary_sel_rows_selected!= 1){
        typee = rownames( get_secondary_sel())[input$secondary_sel_rows_selected]
        l = 0
        ffs = dllist %>% filter(Stype == rownames( get_secondary_sel())[input$secondary_sel_rows_selected]) 
   if(input$secondary_sel_rows_selected>4){
     ffs = ffs %>% filter(Type == 'Demand')
        ffs = rowSums((getexit()/1000000)[,ffs$Name], na.rm = FALSE, dims = 1)
   }
        else{
          ffs = ffs %>% filter(Type == 'Supplies')
          ffs = rowSums((getentry()/1000000)[,ffs$Name], na.rm = FALSE, dims = 1)
        }
        
        
        ##DRAW GRAPHS WITH ADDITION
        
        if(get_analytics()$Table[input$node_select_rows_selected,]$Type == 'Demand'){
          
          l = (getexit()[,get_analytics()$Table[input$node_select_rows_selected,]$Name])
          l = l/1000000
          if(length(ffs)<length(l))
          {
            ffs[(length(ffs)):(length(l))] = 0
          }
          l$ffs=ffs
          colnames(l)[2] = typee
          cat(file=stderr(),str(l))
          return(dygraph(l, main = 'Time Series', ylab = 'Energy (GWh)')%>%
            dyAxis("y", label = "GWh Node", valueRange = c(min(l[,1]), max(l[,1])), independentTicks = TRUE)%>%
            dyAxis("y2", label = "GWh Group ", valueRange = c(min(l[,2]), max(l[,2])), independentTicks = TRUE) %>%
            dySeries(typee, axis=('y2')) %>%dyRangeSelector(dateWindow = c(as.Date(get_analytics()$Date)-90, as.Date(get_analytics()$Date)+10)) 
            %>%   dyShading(from = as.Date(get_analytics()$Date)-1, to = as.Date(get_analytics()$Date)+1, color = "#FFE6E6")
          )# grid()labelLoc = "bottom"
        }
        if(get_analytics()$Table[input$node_select_rows_selected,]$Type == 'Supplies'){
          
          
          l = (getentry()[,get_analytics()$Table[input$node_select_rows_selected,]$Name])
          l = l/1000000
          l$ffs=ffs
          colnames(l)[2] = typee
          cat(file=stderr(),str(l))
          return(dygraph(l, main = 'Time Series', ylab = 'Energy (GWh)')%>%
                   dyAxis("y", label = "GWh Node", valueRange = c(min(l[,1]), max(l[,1])), independentTicks = TRUE)%>%
                   dyAxis("y2", label = "GWh Group ", valueRange = c(min(l[,2]), max(l[,2])), independentTicks = TRUE) %>%
                   dySeries(typee, axis=('y2')) %>% dyRangeSelector(dateWindow = c(as.Date(get_analytics()$Date)-90, as.Date(get_analytics()$Date)+10))
                 %>%   dyShading(from = as.Date(get_analytics()$Date)-1, to = as.Date(get_analytics()$Date)+1, color = "#FFE6E6")
                 )# grid()labelLoc = "bottom"
        }
        
        
      }
      }
      
      
      ##DRAW SINGLE GRAPH
      
      
      if(get_analytics()$Table[input$node_select_rows_selected,]$Type == 'Demand'){
        
        l = (getexit()[,get_analytics()$Table[input$node_select_rows_selected,]$Name])
        cat(file=stderr(),str(l))
        return(dygraph(l/1000000, main = 'Time Series', ylab = 'Energy (GWh)')%>%dyRangeSelector(dateWindow = c(as.Date(get_analytics()$Date)-30, as.Date(get_analytics()$Date)+10))
               %>% dyShading(from = as.Date(get_analytics()$Date)-1, to = as.Date(get_analytics()$Date)+1, color = "#FFE6E6")
               )
       
        
         # grid()labelLoc = "bottom"
      }
      if(get_analytics()$Table[input$node_select_rows_selected,]$Type == 'Supplies'){
        return(dygraph((getentry()[,get_analytics()$Table[input$node_select_rows_selected,]$Name])/1000000, main = 'Time Series')%>%dyRangeSelector(dateWindow = c(as.Date(get_analytics()$Date)-30, as.Date(get_analytics()$Date)+10))
               %>%     dyShading(from = as.Date(get_analytics()$Date)-1, to = as.Date(get_analytics()$Date)+1, color = "#FFE6E6")
               )
      }
      
   
    }
  )
  output$node_select = DT::renderDataTable({
    
  print('Requsting FINT')
    #get_nodes_smp()
    a=1
    a=2
    get_analytics()$Table
    
  },selection = list(mode ='single', selected = 1), filter = list(position = "bottom")) 
  
  get_nodes_smp = reactive({
    g = input$calculate_anom
    get_nodes()
  })

  output$secondary_sel = DT::renderDataTable({

    get_secondary_sel()
 
   }, 
    
    selection = list(mode = 'single', selected = 1), options = list(dom = 't')
    
  )
  

  
  
  output$node_info =DT::renderDataTable({

    get_node_info()}, options = list(dom = 't')
  )
 
  
    get_node_info= reactive({
     
    if(length(input$ex_rows_selected)==0)
    {
      selectCells(dp,1)
    }
    if(length(input$node_select_rows_selected) ==0 | nrow(get_analytics()$Table)==0)
    {
      return(data.frame(error='Error node_stats 1'))
    }
   
    if(get_analytics()$Table[input$node_select_rows_selected,]$Type == 'Supplies')
    {
      wkv=getentry()[,get_analytics()$Table[input$node_select_rows_selected,]$Name]
   tval = getentry()[get_analytics()$Date,get_analytics()$Table[input$node_select_rows_selected,]$Name]
      
      }
    else{
      wkv=getexit()[,get_analytics()$Table[input$node_select_rows_selected,]$Name]
      tval = getexit()[get_analytics()$Date,get_analytics()$Table[input$node_select_rows_selected,]$Name]
    }
    percentage_online_total=round(sum(wkv>0)/ length(wkv),2)
    percentage_online_historic=round(sum(wkv[(length(wkv)-30):length(wkv)]>0)/ 30,2)
    node_sd = sd(wkv[wkv>0])/1e6
      node_nz_mean=mean(wkv[wkv!=0])/1e6
      node_type = 0
      node_max=max(wkv)/1e6
    node_pmax= (tval/1e6)/node_max
    valQ = ecdf(as.numeric(wkv[wkv>0]))(tval)
    return(data.frame(Statistic = c('Total Online %', '30 Day Online %', 'Standard Deviation (GWh)', 'Mean (GWh)', 'Type','Max (GWh)', '% Max', 'Quantile'),Values = c(round(percentage_online_total,2),round(percentage_online_historic,2), round(node_sd,2), round(node_nz_mean,2), node_type, round(node_max,2), round(node_pmax,2), round(valQ,2))))
  })
  
  
  
  
  
  get_secondary_sel = reactive({
    
   if(getmode())
   {
    if(length(input$ex_rows_selected)==0)
    {
    selectCells(dp,1)
    }
    if(length(input$node_select_rows_selected) ==0 | nrow(get_analytics()$Table)==0)
    {
      return(data.frame())
    }
    p=1
    p=2
  if(length(input$flag_filter) ==0)
  {
    return(data.frame())
  }
    if(get_analytics()$Table[input$node_select_rows_selected,]$Type == 'Supplies')
    {
      vals =round(getentry()[get_analytics()$Date + c(-1,0,1),get_analytics()$Table[input$node_select_rows_selected,]$Name]/1000000,2)
    }
  else{
    vals = round(getexit()[get_analytics()$Date + c(-1,0,1),get_analytics()$Table[input$node_select_rows_selected,]$Name]/1000000,2)
    }
     f1= dllist %>% filter(Type == 'Supplies') %>% split(.$Stype) %>% map(~rowSums((getentry()/1000000)[get_analytics()$Date + c(-1,0,1),.x$Name]))%>% as_tibble()
    f2=dllist %>% filter(Type == 'Demand') %>% split(.$Stype) %>% map(~rowSums((getexit()/1000000)[get_analytics()$Date + c(-1,0,1),.x$Name]))  %>% as_tibble()
   r = rbind(t((vals)),(t(cbind(f1,f2))))
  r = round(r,2)
    return(r)
   }
    else{
      return(tibble('Group Statistics not available'))
    }
    })
  
  
  observeEvent(input$batch,{
    {
      withProgress(message = 'Batch Processing', value = 0, {
        
    for(i in 1:nrow(updatefilter()))
    {
     
          incProgress(1/nrow(updatefilter()), message = paste("Processing",i,'/',nrow(updatefilter())))
      
      
      
      
      focusday2[['Date']] <<-  updatefilter()[i,1]
      focusday2[['UAG']] <<- round(updatefilter()[i,2]/1e6,2)
      focusday2[['ExcessUAG']] <<- round(abs(20-abs(round(updatefilter()[i,2]/1000000,2))),2)
      
      
      analytics_holder[[as.name(format(updatefilter()[i,1]))]]<<-focusday2
      get_analytics(focusday2)
      Recalc_Tbl()
      get_analytics(analytics_holder[[length(analytics_holder)]])
      updatePickerInput(session, 'past_analis',choices=(names(analytics_holder)), selected = names(analytics_holder)[length(names(analytics_holder))])
      
    }
      }
        )
      shinyalert('Batch join Complete!','You can download the analysis in the day explorer', 'success')
      
      }
  }
  )
  
  vlm = observeEvent(input$node_select_rows_selected,{
    
    #selectCells(sel_prox,which(row.names(r)==get_analytics()$Table[input$node_select_rows_selected,]$Stype))
    
  }) 

  output$uag_prc <- renderValueBox({

    valueBox(countupOutput('c_1'),
      "Over Threshold", icon = icon("list"),
      color = 'light-blue')
  })
  
  c1_old = reactiveVal(0)
  c2_old = reactiveVal(0)
  
  output$c_1 = renderCountup({
    opts = list(
      useEasing = TRUE, 
      useGrouping = TRUE, 
      separator = ',', 
      decimal = '.', 
      
      suffix = '%' 
    )
    v=round(nrow(updatefilter())/ length(seq(from = input$dateRange[1], to = input$dateRange[2], by = 'day')),2)*100
    lp = isolate(c1_old())
    cc= countup(count = v, start =lp, options = opts)
    isolate(c1_old(v))
    cc
  })
  output$uag_num <- renderValueBox({

    valueBox(countupOutput('c_2')
      ,"Days over threshold", icon = icon("list"),
      color = 'light-blue')
  })
  
  output$c_2 = renderCountup({
    opts = list(
      useEasing = TRUE, 
      useGrouping = TRUE, 
      separator = ',', 
      decimal = '.'
    )
    v2= paste0(nrow(updatefilter()))
    
    et = isolate(c2_old())
    cc = countup(count = v2, start =et, options = opts)
   
    isolate(c2_old(v2))
    cc
  })
  output$uag_t <- renderValueBox({
   get_cpt_box()
  })
  
  get_cpt_box = reactive({
    cc = length(d()@cpts)-1
    valueBox(
      paste0( cc, ' locations'),
      ,subtitle = "Changepoints", icon = icon("list"),
      color = 'light-blue')
  })
  
  
  

  
  
  output$sync_table =renderTable({
    
last_up()
  })
  
  last_up = reactiveVal({
   last_update
  })
 
  output$dl_balance <- downloadHandler(
    #update primary range selector ue to poor design
    
    
    
    
    # For PDF output, change this to "report.pdf"
    filename = "Shrinkage.csv",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      write.csv(tk_tbl(getactive()),file)
      
    }
  )

  output$dl_entry <- downloadHandler(
    #update primary range selector ue to poor design
    
    
    
    
    # For PDF output, change this to "report.pdf"
    filename = "Entry.csv",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      write.csv(tk_tbl(nxts),file)
      
    }
  )
  output$dl_exit <- downloadHandler(
    #update primary range selector ue to poor design
    
    
    
    
    # For PDF output, change this to "report.pdf"
    filename = "Exit.csv",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      write.csv(tk_tbl(xxts),file)
      
    }
  )
  
  
 getmode = function(){
   if(input$switch_entry & input$switch_exit){return(TRUE)}
   else{return(FALSE)}
 }
  
 
 
 observeEvent(input$dateRange,{
   if(input$dateRange[1]>input$dateRange[2])
      {
     shinyalert('Warning!','Start date after end date!')
        updateDateRangeInput(session, 'dateRange', start = end(getactive())-30, end = end(getactive())-5)
      }
 })
 
 observeEvent(input$dateRange_reporting,{
   if(input$dateRange_reporting[1]>input$dateRange_reporting[2])
   {
     shinyalert('Warning!','Start date after end date!')
     updateDateRangeInput(session, 'dateRange_reporting', start = end(getactive())-30, end = end(getactive())-5)
   }
 })

  observeEvent(input$calculate_anom,{
    
    date = input$analysis_date
    
    focusday2[['Date']] <<-  date
    focusday2[['UAG']] <<- round(getactive()[date,input$inp2]/1e6,2)
    focusday2[['ExcessUAG']] <<- round(abs(20-abs(round(getactive()[date,input$inp2]/1000000,2))),2)
    
    
  
    analytics_holder[[as.name(format(date))]]<<-focusday2
    
    get_analytics(focusday2)
  
   
    Recalc_Tbl()
    get_analytics(analytics_holder[[length(analytics_holder)]])
    
    updatePickerInput(session, 'past_analis',choices=(names(analytics_holder)), selected = names(analytics_holder)[length(names(analytics_holder))])
    
    }
  )
  Recalc_Tbl = function(x){
    
    ##Construct DB##
    ENT = tibble(colnames(getentry()))
    
    EXT = tibble(colnames(getexit()))
    
    ENT_F = tibble(.rows = nrow(ENT))
    EXT_F = tibble(.rows = nrow(EXT))
    
    colnames(EXT) = c('Name')
    colnames(ENT) = c('Name')
    ENT$Type = rep('Supplies', nrow(ENT))
    EXT$Type = rep('Demand', nrow(EXT))
    if(getmode())
    {
      
      d1 = dllist %>% filter(Type == 'Supplies')
      d2 = dllist %>% filter(Type == 'Demand')
      a1= d1%>%select(Stype) %>% slice(match(ENT[,1][[1]], d1$Name)) %>% as.data.frame()
      a2 = d2 %>%select(Stype) %>% slice(match(EXT[,1][[1]], d2$Name)) %>% as.data.frame()
      ENT$Stype =a1[,1]
      EXT$Stype = a2[,1]
      
    }
    
    #Conditionally output Dtype, Stype in return Table
    # Also needed: Name, Cor UAG
    
    
    if(length(input$flag_filter) == 0)
    {
      
      
      return()
      
    }
    
    
    width = 600
    v = which(index(getentry()) == get_analytics()$Date )
    f_period_n  = seq(v - width, v + min(width, nrow(getentry()) -v), by = 1)
    f_period_x  = seq(v - width, v + min(width, nrow(getexit()) -v), by = 1)
    
    
    
    ##If not, loading bar is require
    withProgress(message = 'Processing Data', value = 0, {
      
      
      
      selection_size = length(input$flag_filter)+2
      
      
      if('Interrupts' %in% input$flag_filter | 'All' %in% input$flag_filter)
      {
        incProgress(1/selection_size, message = paste("Calculating Interrupts"))
        e1 = sapply(ENT$Name, function(x){
          
          tgs =getentry()[c(-1,0,1) +get_analytics()$Date,x]
          if(as.numeric(tgs[1]) >0 & as.numeric(tgs[3]) > 0 & as.numeric(tgs[2]) ==0)
          {
            return(1)
          }
          else{
            return(0)
          }
          
        })
        ENT_F$Interrupt = e1
        e2 = sapply(EXT$Name, function(x){
          tgs =getexit()[c(-1,0,1) +get_analytics()$Date,x]
          if(as.numeric(tgs[1]) >0 & as.numeric(tgs[3]) > 0 & as.numeric(tgs[2]) ==0)
          {
            return(1)
          }
          else{
            return(0)
          }
        })
        EXT_F$Interrupt = e2
      }
      
      if('Anomalies (Fast)' %in% input$flag_filter  | 'All' %in% input$flag_filter)
      {
        s_step = (1/selection_size) /222
        
        
        e1 = sapply(ENT$Name, function(x){
          
         incProgress(s_step, message = paste("Calculating Anomalies(Fast)"))
          l=tsoutliers(getentry()[f_period_n,x])%>% xts(., order.by= index(getentry()[f_period_n,x]))
          if(sum(is.na(l)) >0){return(0)}
          if(l[get_analytics()$Date,]>0  & getentry()[get_analytics()$Date,x] >0)
          {
            return(1)
          }
          else{
            return(0)
          }
          
        })
        ENT_F$Anomaly_F = e1
        e2 = sapply(EXT$Name, function(x){
          
          incProgress(s_step, message = paste("Calculating Anomalies(Fast)"))
          l=tsoutliers(getexit()[f_period_n,x])%>% xts(., order.by= index(getexit()[f_period_n,x]))
          if(sum(is.na(l)) >0){return(0)}
          if(l[get_analytics()$Date]>0 & getexit()[get_analytics()$Date,x] >0)
          {
            return(1)
          }
          else{
            return(0)
          }
          
        })
        EXT_F$Anomaly_F = e2
      }
      if('Quantile' %in% input$flag_filter  | 'All' %in% input$flag_filter)
      {
        incProgress(1/selection_size, message = paste("Calculating Quantile"))
        e1 = sapply(ENT$Name, function(x){
          
          tgs =getentry()[,x]
          if(sum(tgs[tgs>0]) <1 | tgs[get_analytics()$Date] ==0 )
          {
            return(0)
          }
          valQ = ecdf(as.numeric(tgs[tgs>0]))(tgs[get_analytics()$Date])
          if(tgs[get_analytics()$Date] != 0 & (valQ>0.95) )
          {
            return(1)
          }
          else{
            return(0)
          }
          
        })
        ENT_F$High = e1
        e2 = sapply(EXT$Name, function(x){
          
          tgs =getexit()[,x]
          if(sum(tgs[tgs>0]) <1 | tgs[get_analytics()$Date])
          {
            return(0)
          }
          valQ = ecdf(as.numeric(tgs[tgs>0]))(tgs[get_analytics()$Date])
          if(tgs[get_analytics()$Date] != 0 & (valQ>0.95) )
          {
            return(1)
          }
          else{
            return(0)
          }
        })
        EXT_F$High = e2
        
      }
        if('Quantile' %in% input$flag_filter  | 'All' %in% input$flag_filter)
        {
         
          e1 = sapply(ENT$Name, function(x){
            
            tgs =getentry()[,x]
            if(sum(tgs[tgs>0]) <1 | tgs[get_analytics()$Date] ==0 )
            {
              return(0)
            }
            valQ = ecdf(as.numeric(tgs[tgs>0]))(tgs[get_analytics()$Date])
            if(tgs[get_analytics()$Date] != 0 & (valQ <0.05 ) )
            {
              return(1)
            }
            else{
              return(0)
            }
            
          })
          ENT_F$Low = e1
          e2 = sapply(EXT$Name, function(x){
            
            tgs =getexit()[,x]
            if(sum(tgs[tgs>0]) <1 | tgs[get_analytics()$Date])
            {
              return(0)
            }
            valQ = ecdf(as.numeric(tgs[tgs>0]))(tgs[get_analytics()$Date])
            if(tgs[get_analytics()$Date] != 0 & (valQ <0.05 ) )
            {
              return(1)
            }
            else{
              return(0)
            }
          })
          EXT_F$Low = e2
      }
      if('Extreme' %in% input$flag_filter  | 'All' %in% input$flag_filter)
      {incProgress(1/selection_size, message = paste("Calculating Extreme"))
        e1 = sapply(ENT$Name, function(x){
          
          tgs =getentry()[,x]
          if(max(tgs) == tgs[get_analytics()$Date])
          {
            return(1)
          }
          else{
            return(0)
          }
          
        })
        ENT_F$Extreme = e1
        e2 = sapply(EXT$Name, function(x){
          tgs =getexit()[,x]
          if(max(tgs) == tgs[get_analytics()$Date])
          {
            return(1)
          }
          else{
            return(0)
          }
        })
        EXT_F$Extreme = e2
        
        
      }
      if('Anomalies (Slow)' %in% input$flag_filter  | 'All' %in% input$flag_filter)
      {
       
        s_step = (1/selection_size) /222
        e1 = sapply(ENT$Name, function(x){
          incProgress(s_step, message = paste("Calculating Anomalies(Slow)"))
          tgs =getentry()[,x]
          t_bl= tk_tbl(tgs)
          colnames(t_bl)[1] = 'date'
          t_bl = as_tbl_time(t_bl, date)
          anoms =t_bl %>% time_decompose(colnames(t_bl)[2], method = 'stl') %>% anomalize(remainder, method = 'iqr') 
          anomx = xts(anoms, order.by = anoms$date)[get_analytics()$Date]
          if(anomx$anomaly =='Yes'  & getentry()[get_analytics()$Date,x] >0)
          {
            return(1)
          }
          else{
            return(0)
          }
          
        })
        ENT_F$Anomaly_S = e1
        e2 = sapply(EXT$Name, function(x){
          incProgress(s_step, message = paste("Calculating Anomalies(Slow)"))
          tgs =getexit()[,x]
          t_bl= tk_tbl(tgs)
          colnames(t_bl)[1] = 'date'
          t_bl = as_tbl_time(t_bl, date)
          anoms =t_bl %>% time_decompose(colnames(t_bl)[2], method = 'stl') %>% anomalize(remainder, method = 'iqr') 
          anomx = xts(anoms, order.by = anoms$date)[get_analytics()$Date]
          if(anomx$anomaly =='Yes'  & getexit()[get_analytics()$Date,x] >0)
          {
            return(1)
          }
          else{
            return(0)
          }
        })
      
     EXT_F$Anomaly_S = e2
        
       
      }
      
      
      
    })
    
    
    
    # CONDENSE FLAGS INTO STRINGS
    e_b = rbind(ENT_F, EXT_F)
    Flags = apply(e_b,1,function(x){a =paste(colnames(e_b)[which(x>0)], collapse = ', ')
    if(length(a) ==0){return('')} else{ return(a)}})
    
    EXT_F$R_UAG = e2
    
    #RECALCULATE UAG
    
    e1 = sapply(ENT$Name, function(x){
      
      tgs =getentry()[c(1,-1) + get_analytics()$Date,x]
      nval = as.numeric(mean(tgs))
      val =getentry()[ get_analytics()$Date,x]
      ug = updatefilter()[input$ex_rows_selected,2]
      ug = ug + (nval-val)
      return(ug)
      
    })
    
    e2 = sapply(EXT$Name, function(x){
      
      tgs =getexit()[c(1,-1) + get_analytics()$Date,x]
      nval = as.numeric(mean(tgs))
      val =getexit()[ get_analytics()$Date,x]
      ug = updatefilter()[input$ex_rows_selected,2]
      ug = ug - (nval-val)
      return(ug)
    })
    
    UAG_R = c(e1,e2)
    
    e1 = sapply(ENT$Name, function(x){
      
      tgs =getentry()[get_analytics()$Date,x]
      return(as.numeric(tgs))
      
    })
    
    e2 = sapply(EXT$Name, function(x){
      
      tgs =getexit()[get_analytics()$Date,x]
      return(as.numeric(tgs))
      
    })
    
    total_daily = c(e1,e2)
    
    if(exists('FIN_T', envir = .GlobalEnv))
    {
      rm('FIN_T', envir =.GlobalEnv)
    }
    FIN_Tl = rbind(ENT, EXT)
    FIN_Tl$Flags = Flags

    FIN_Tl$`Energy (GWh)` = round(total_daily/1e6,2)
    FIN_Tl$`Rec. UAG (GWh)` = round(UAG_R/1e6,2)
    FIN_Tl = FIN_Tl %>% arrange(desc(Flags),desc(`Rec. UAG (GWh)`))
    FIN_T <<- FIN_Tl
  updateMaterialSwitch(session,'show_d_hack', TRUE)
  analytics_holder[[as.name(format(get_analytics()$Date))]]$Table <<- FIN_T

  }
  
  
  
  output$download_analyis <- downloadHandler(
    #update primary range selector ue to poor design
    
    
   
    
    
    # For PDF output, change this to "report.pdf"
    filename = "Flagged_values.csv",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      merged_table = analytics_holder %>% map(~mutate(.$'Table', Day = .$'Date') %>% filter(Flags != '')) %>% bind_rows
      
      write.csv2( merged_table,file)
      
    }
  )
  


  observeEvent(input$button_DL, {
updateDateRangeInput(session,'dateRange',start = input$dateRange_reporting[1], end = input$dateRange_reporting[2])
    if('Daily Balancing Errors' %in% input$report_choices  | 'All' %in% input$report_choices )
    {
    updatePickerInput(session,'analchoice', selected = c("Bollinger Bands","Fixed Limit", "D'Arpino(2014)",'Anomalize','ETS Forecast') )
}

    output$downloadData<<-downloadHandler(
      #update primary range selector ue to poor design
      
  
      
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        tempReport3 <- file.path(tempdir(), "nglogo.jpg")
        file.copy("nglogo.jpg", tempReport3, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
       
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file)
        
      } )
    jsinject <- "setTimeout(function(){window.open($('#downloadData').attr('href'))}, 100);"
    session$sendCustomMessage(type = 'jsCode', list(value = jsinject))  
    
    
    
    
    
    })
  ###Get active datasets function####
  
   
 
  ts = reactive({
    d=   getactive()[seq(from = input$dateRange[1], to = input$dateRange[2], by = 'day'),input$inp2]
    return(cusum(d, title = 'Cumulative sum control chart', se.shift = input$control_cpt))
  }) 
  
  d = reactive({
    if(!(input$cpt_MODE == 'PELT (Recommended)'))
    {
    d=   as.numeric(getactive()[seq(from = input$dateRange[1], to = input$dateRange[2], by = 'day'),input$inp2])
    return(cpt.mean(d, method = 'BinSeg',minseglen = 10,Q=input$control_cpt3))
    }
    else{
      d=   as.numeric(getactive()[seq(from = input$dateRange[1], to = input$dateRange[2], by = 'day'),input$inp2])
      return(cpt.meanvar(d, method = 'PELT',minseglen=10))
    }
  })
  
  bc = reactive({
    d=   as.numeric(getactive()[seq(from = input$dateRange[1], to = input$dateRange[2], by = 'day'),input$inp2])
    return(bcp(d,p0 = input$control_cpt2))
  })
  
  output$bcp = renderPlot({plot(bc())})
  output$tso = renderPlot(ts())
  
  output$cpt_output= renderPlot({
    plot(d(), ylab = 'Energy (Gwh)')
  })
  output$sumr = renderText({
    #bcpN = sum(bc()$posterior.prob >0.5,na.rm = TRUE) 
   # cumN = length(ts()$violations[1])+length(ts()$violations[2])
    #cptN = length(d()@cpts-1)
    
   # oT = "BCP Changepoints:" %s+% bcpN %s+% "\n CUSUM violations" %s+% cumN
    
   # return(oT)
  })
  
  
  #output$tso = renderPlot({plot(tso( getactive()[seq(from = input$dateRange[1], to = input$dateRange[2], by = 'day'),input$inp2]))})
  

  output$Locations = renderTable({
    f = d()
    dff=  data.frame(Location = index(getactive()[seq(from = input$dateRange[1], to = input$dateRange[2], by = 'day'),input$inp2])[f@cpts], Means = paste(round(f@param.est$mean/1000000,2),'GWh'))
    dff[,1] =sapply(dff[,1], as.character)
    dff
  })
  
  
  
  statustable = reactive({
    online_status = c(input$id, input$switch_entry, input$switch_exit)
    if(all(online_status)){}
    start = c(start(getactive()), start(getentry()), start(getexit()))
    end = c(end(getactive()), end(getentry()), end(getexit()))
    rnames = c('Shrinkage', 'Entry', 'Exit')
    ttx
    dff = data.frame('Dataset' = rnames, Start= start, End = end, Online = online_status)
    dff[,2] = as.character(dff[,2])
    dff[,3] = as.character(dff[,3])
    if(is.null(input$file1) & !input$id)
    {
      dff[1,c(2,3)] = 'N/A'
    }
    if(is.null(input$file_entry) & !input$switch_entry)
    {
      dff[2,c(2,3)] = 'N/A'
    }
    if(is.null(input$file_exit) & !input$switch_exit)
    {
      dff[3,c(2,3)] = 'N/A'
    }
    dff
  })
  output$tb2 = renderTable({
    
    (statustable())
  })
  
  
  
  output$summary_text = renderText(paste('Available joint timeframe: ',max(statustable()[,2]),' to ', min(statustable()[,3])))
 

  
  
   updatefilter = reactive({
    Filter = u_data()$DB
    if(is.na(Filter[[3]][1]))
    {
      return(Filter[,1:2])
    }
    Filter = Filter %>% dplyr::filter(.[[2]]> Upper | .[[2]] < Lower)
    # Filter = Filter[DT::order('Date',decreasing = TRUE )]  how do you order there things???
    Filter
  })
  
  
  
  
  ##########################UAG MONITOR LIMITS CALCS#####################
  u_data = reactive({
    
    if(length(input$analchoice) ==0)
    {
      updateMaterialSwitch(session, 'show_d_hack2',FALSE)
    }
    else{
      updateMaterialSwitch(session, 'show_d_hack2',TRUE)
    }
    
    selected = seq(from = input$dateRange[1], to = input$dateRange[2], by = 'day')
   
    ga = getactive()[selected,input$inp2]
    pp = as.numeric(getactive()[selected,input$inp2])
    
    
   # rmean = rollmean(getactive()[,input$inp2], 7 )
    uppers =rep(200000000, length = length(pp))
    lowers =rep(-200000000, length = length(pp))
    
    
    w = 30
    ma = rollapply(as.numeric(getactive()[seq(from = input$dateRange[1]-w+1, to = input$dateRange[2], by = 'day'),input$inp2]), width = w, mean)
    sds= rollapply(as.numeric(getactive()[seq(from = input$dateRange[1]-w+1, to = input$dateRange[2], by = 'day'),input$inp2]), width =w,sd)
    pbb = ma+input$bol_sd*sds
    mbb =  ma-input$bol_sd*sds
    
    positive = rep(input$gwh_lim_u*1e6, length = length(pp))
    negative = rep(input$gwh_lim_l*1e6,length = length(pp))
    if(is.null(input$analchoice))
    {
      uppers = cbind(uppers*0,uppers*0)
      lowers = cbind(uppers*0,uppers*0)
    }
    if("Bollinger Bands" %in% input$analchoice)
    {
      uppers = cbind(uppers, pbb)
      lowers = cbind(lowers, mbb)
    }
    if("Fixed Limit" %in% input$analchoice)
    {
      uppers = cbind(uppers, positive)
      lowers = cbind(lowers, negative)
    }
    if("Anomalize" %in% input$analchoice)
    {
      
      positive = rep(20000000, length = length(pp))
      negative = rep(-20000000,length = length(pp))
      lpc = as.numeric(getactive()[seq(from = input$dateRange[1], to = input$dateRange[2], by = 'day'),input$inp2])
      for(i in 1:length(positive))
      {
        if(lpc[i] >0)
        {
          positive[i] = lpc[i]*1.1
          negative[i] = lpc[i]*0.9
        }
        else
        {
          positive[i] = lpc[i]*0.9
          negative[i] = lpc[i]*1.1
        }
      }
      
      se= which(index(getactive()) %in%selected)
      t_bl= tk_tbl(getactive()[seq(from = input$dateRange[1]-300, to = input$dateRange[2], by = 'day'),input$inp2])
    colnames(t_bl)[1] = 'date'
    t_bl = as_tbl_time(t_bl, date)
    anoms =t_bl %>% time_decompose(colnames(t_bl)[2], method = 'stl') %>% anomalize(remainder, method = 'gesd') 
      anomx = xts(anoms, order.by = anoms$date)[selected]
    positive[which(anomx$anomaly == 'Yes')] =1
    negative[which(anomx$anomaly =='Yes')] = -1
      uppers = cbind(uppers, positive)
      lowers = cbind(lowers, negative)
    }
    if("ETS Forecast" %in% input$analchoice)
    {
  width = 600
     se= which(index(getactive()) %in%selected)
     # f =rollapply(as.ts(getactive()[,input$inp2]),se, align='right',function(x){ f=forecast(ets(x),h=1)
      #return(cbind(f$lower[,2],f$upper[,2]))})
      model = ets(as.ts(getactive()[max(se[1]-width,1):se[1],input$inp2]))
      p=cbind(0,0)
      for(i in 1:length(se))
      {
        f=suppressMessages(forecast(ets(as.ts(getactive()[max(se[1]-width,1):se[i],input$inp2]),model),h=1))
        p=rbind(p,cbind(f$lower[,2],f$upper[,2]))
      }
      p=p[-1,]
    f=p
      uppers = cbind(uppers,f[,2])
      lowers = cbind(lowers,f[,1])  
      #Now need selected band.
    }
    if("D'Arpino(2014)" %in% input$analchoice)
    {
      
      r = 0.15 #Set the system correlation
      u_demand = 0.03 #demand side relative uncertainty
      u_supply = 0.01 #supply side relative uncertainty
      
      supplyside = apply(getentry()[selected], 1, function(x) { sum(x>0)}) #supply side number of active nodes per day  
      demandside = apply(getexit()[selected], 1, function(x) { sum(x>0)}) # demand side number of active nodes per day 
      
      f = function(x,y){sqrt(1/x + y*(x-1)/x)}
      
      mods = f(supplyside, r)
      modd = f(demandside,r )
      cint =  mods*u_supply*(rowSums(getentry()[selected])) + modd*u_demand*(rowSums(getexit()[selected]))
      cint[is.nan(cint)] = 2e8
      uppers = cbind(uppers, cint)
      lowers = cbind(lowers, -cint)
      
    }
   ## rmean = rollmean(ga, 7 )

    if(length(input$analchoice) == 0)
    {
      upper = NA
   lower = NA
    }
    if(length(input$analchoice == 1))
      
    {
      upper = round(uppers[,2],2)
      lower = round(lowers[,2],2)
    }
    
    if(length(input$analchoice) >1)
    {
      uppers = uppers[,-1]
      lowers = lowers[,-1]
      if(input$mode_op_uag)
      {
        upper = round(apply(uppers, 1, min),2)
        lower = round(apply(lowers, 1, max),2)
      }
      else{
        upper = round(apply(uppers, 1, max),2)
        lower = round(apply(lowers, 1, min),2)
      }
      
    }
    
    df = data.frame(selected[1:length(pp)], pp, upper, lower,ma)
    
    
    colnames(df) = c('Date', input$inp2, 'Upper', 'Lower', 'Mean')
    
    
    
    return(list(DB =df, uppers = uppers, lowers = lowers))
    
    
   
    
  })
   
 
  output$ex = DT::renderDataTable(
    
    {
      
      if('Mean' %in% colnames(updatefilter()))
      {
        r = which(colnames(updatefilter()) == 'Mean')
         return(updatefilter()[,-r])
      }
   return( updatefilter())
      
      
    }, selection=  list(mode='single'),options = list(scrollX = TRUE)
    )
  dp = dataTableProxy('ex')
  sel_prox = dataTableProxy('secondary_sel')
  #print(outputOptions(output))

  an_observe_func = observe({
    input$ex_rows_selected
    
      #do stuff here
     if(is.null(input$ex_rows_selected))
     {
       show_point(FALSE)
     }
      else{
        show_point(TRUE)
      }
    
  })
  
  show_point = reactiveVal(TRUE)
  
  
  output$echartsu = renderEcharts4r({
    get_e_chart()
  })
  
  ################## AGGREGATE PLOTS ############################
  
  output$aggregate = renderEcharts4r(
    {
      
      opts = list(
        legend =list(
          list(
          show = 'false'
          )
        )
      )
      if(input$aggregate_function == 'Sum')
      {
        if(input$aggregate_time == 'Weekly')
        {
          lp = round(apply.weekly(getactive()[,input$inp2], sum)/1e6,2)
          lp = tk_tbl(lp)
          o = colnames(lp)
          e= e_charts(dispose = FALSE) %>% 
            e_list(list(
              xAxis = list(
                type = "category",
                data = lp[,1][[1]]
              ),
              yAxis = list(
                type = "value"
              )
            )) %>% 
            e_list(list(
              
              series = list(
                list(
                  type = "bar",
                  data = lp[,2][[1]]
                )
              )
            ), TRUE)%>% e_y_axis(name = 'Energy (GWh)')%>% e_tooltip() %>%e_datazoom(x_index = 0,toolbox = FALSE, type = 'slider', minSpan = list('5'), bottom = list('0'))
          return(e)   
        }
        if(input$aggregate_time == 'Monthly')
        {
          lp = round(apply.monthly(getactive()[,input$inp2], sum)/1e6,2)
          lp = tk_tbl(lp)
          o = colnames(lp)
          
        e= e_charts(dispose = FALSE) %>% 
            e_list(list(
              xAxis = list(
                type = "category",
                data = lp[,1][[1]]
              ),
              yAxis = list(
                type = "value"
              )
            )) %>% 
            e_list(list(
              
              series = list(
                list(
                  type = "bar",
                  data = lp[,2][[1]]
                )
              )
            ), TRUE)%>% e_y_axis(name = 'Energy (GWh)') %>%e_datazoom(x_index = 0,toolbox = FALSE, type = 'slider', minSpan = list('5'), bottom = list('0'))
       return(e)
         }
        if(input$aggregate_time == 'Quarterly')
        {
          lp = round(apply.quarterly(getactive()[,input$inp2], sum)/1e6,2)
          lp = tk_tbl(lp)
          o = colnames(lp)
          e= e_charts(dispose = FALSE) %>% 
            e_list(list(
              xAxis = list(
                type = "category",
                data = lp[,1][[1]]
              ),
              yAxis = list(
                type = "value"
              )
            )) %>% 
            e_list(list(
              
              series = list(
                list(
                  type = "bar",
                  data = lp[,2][[1]]
                )
              )
            ), TRUE)%>% e_y_axis(name = 'Energy (GWh)') %>%e_datazoom(x_index = 0,toolbox = FALSE, type = 'slider', minSpan = list('5'), bottom = list('0'))
          return(e)     }
        if(input$aggregate_time == 'Yearly')
        {
          lp = round(apply.yearly(getactive()[,input$inp2], sum)/1e6,2)
          lp = tk_tbl(lp)
          o = colnames(lp)
          return(lp %>% e_charts_(o[1],dispose = FALSE) %>% e_bar_(o[2]) %>% e_y_axis(name = 'Energy (GWh)'))
        }
      }
      if(input$aggregate_function == 'Abs Sum')
      {
        if(input$aggregate_time == 'Weekly')
        {
          lp = round(apply.weekly(getactive()[,input$inp2], function(x) abs(sum(x)))/1e6,2)
          lp = tk_tbl(lp)
          o = colnames(lp)
          e= e_charts(dispose = FALSE) %>% 
            e_list(list(
              xAxis = list(
                type = "category",
                data = lp[,1][[1]]
              ),
              yAxis = list(
                type = "value"
              )
            )) %>% 
            e_list(list(
              
              series = list(
                list(
                  type = "bar",
                  data = lp[,2][[1]]
                )
              )
            ), TRUE)%>% e_y_axis(name = 'Energy (GWh)') %>%e_datazoom(x_index = 0,toolbox = FALSE, type = 'slider', minSpan = list('5'), bottom = list('0'))
          return(e) }
        if(input$aggregate_time == 'Monthly')
        {
          lp = round(apply.monthly(getactive()[,input$inp2], function(x) abs(sum(x)))/1e6,2)
          lp = tk_tbl(lp)
          o = colnames(lp)
          e= e_charts(dispose = FALSE) %>% 
            e_list(list(
              xAxis = list(
                type = "category",
                data = lp[,1][[1]]
              ),
              yAxis = list(
                type = "value"
              )
            )) %>% 
            e_list(list(
              
              series = list(
                list(
                  type = "bar",
                  data = lp[,2][[1]]
                )
              )
            ), TRUE)%>% e_y_axis(name = 'Energy (GWh)') %>%e_datazoom(x_index = 0,toolbox = FALSE, type = 'slider', minSpan = list('5'), bottom = list('0'))
          return(e) }
        if(input$aggregate_time == 'Quarterly')
        {
          lp = round(apply.quarterly(getactive()[,input$inp2], function(x) abs(sum(x)))/1e6,2)
          lp = tk_tbl(lp)
          o = colnames(lp)
          e= e_charts(dispose = FALSE) %>% 
            e_list(list(
              xAxis = list(
                type = "category",
                data = lp[,1][[1]]
              ),
              yAxis = list(
                type = "value"
              )
            )) %>% 
            e_list(list(
              
              series = list(
                list(
                  type = "bar",
                  data = lp[,2][[1]]
                )
              )
            ), TRUE)%>% e_y_axis(name = 'Energy (GWh)') %>%e_datazoom(x_index = 0,toolbox = FALSE, type = 'slider', minSpan = list('5'), bottom = list('0'))
          return(e) }
        if(input$aggregate_time == 'Yearly')
        {
          lp = round(apply.yearly(getactive()[,input$inp2], function(x) abs(sum(x)))/1e6,2)
          lp = tk_tbl(lp)
          o = colnames(lp)
          e= e_charts(dispose = FALSE) %>% 
            e_list(list(
              xAxis = list(
                type = "category",
                data = lp[,1][[1]]
              ),
              yAxis = list(
                type = "value"
              )
            )) %>% 
            e_list(list(
              
              series = list(
                list(
                  type = "bar",
                  data = lp[,2][[1]]
                )
              )
            ), TRUE)%>% e_y_axis(name = 'Energy (GWh)') %>%e_datazoom(x_index = 0,toolbox = FALSE, type = 'slider', minSpan = list('5'), bottom = list('0'))
          return(e)}
      }
      
    }
  )
  ############################# UAG PLOT #######################################
  
get_e_chart = reactive({
  lp = updatefilter()
  g=show_point()
  dat = u_data()$DB
  
  if(nrow(dat) < 200)
  {
    th = 2
  }
  if(nrow(dat) > 200 & nrow(dat) < 900){
    th=1
  }
  if(nrow(dat) >900){th=0.5}
  
  ds = which(dat[,1] %in% lp[,1])
  
  
  if(!('Upper' %in% colnames(dat)))
  {
    dat$Upper = 0
    dat$Lower = 0
  }
  
  dat$Anomalies = NA
  dat$Anomalies[ds] = dat[ds,2]
  #YLS = c(max(c(dat[,2], dat[,3]))*1.1, min(c(dat[,2], dat[,4]))*1.1)/1e6
  dat[,-1]= dat[,-1]/1e6
  o= colnames(dat)
  
  

 e_plot= dat%>% e_charts_(o[1],dispose = FALSE) %>% e_tooltip() %>%
    e_line(Anomalies, symbolSize = '8', symbol = 'circle', connectNulls = FALSE) %>%
    e_line_(o[2],symbol = 'none',lineStyle= list(width =list(th),color=list('rgb(0,0,0'))) %>% 
    e_line_(o[3],symbol ='none', showSymbol = list('false'),hoverAnimation = list('false'),lineStyle= list(type =list('dotted'),color=list('rgb(100,100,100')))%>% 
    e_line_(o[4],symbol ='none',lineStyle= list(type =list('dotted'),color=list('rgb(100,100,100'))) %>%
    e_y_axis(name='Energy (GWh)')
  
  
  s = input$ex_rows_selected
  xAxis = updatefilter()[s,1]
  yAxis = updatefilter()[s,2]/1e6
  value = round(updatefilter()[s,2]/1e6,1)
  l =list(xAxis =xAxis,yAxis = yAxis,value= value)
  e_plot = e_plot %>% e_mark_point('Anomalies', data = l)
  
  ##  dat%>% e_charts_(o[1]) %>% e_line_(o[2],symbol = 'none',lineStyle= list(type = list('dashed')))
  
  e_plot
  
  
  
  
  
})
  observeEvent(input$echartsu_clicked_data,{
    a=1
    a=2
    r = which(updatefilter()[,1] ==input$echartsu_clicked_data[[1]][1] )
    selectRows(dp,r)
    
      selectPage(dp, ceil(r/10))
    
  })

#  outputOptions(output, 'uag',suspendWhenHidden = F)
  
  
  
  ##sync##
  
  

  updatebutton = observeEvent(input$syncbutton,{
    shinyjs::disable('syncbutton')
    withProgress(message = 'Updating Shrinkage', value = 0, {
      # Number of times we'll go through the loop
      
      downloaduag()
      processuag()
      
      
      incProgress(0.33, message = paste("Updating Demand"))
      rnames = c('Shrinkage', 'Entry', 'Exit')
       lp = last_up()
     lp[,2] =as.character(Sys.Date())
      last_up(lp)
      
      
      updatexitDB()
      incProgress(0.33, message = paste("Updating Supply"))
      updatentryDB()
      shinyjs::enable('syncbutton')
      updateDateRangeInput(session, 'dateRange', start = input$dateRange[1], end = end(getactive()))
      write.csv(last_up(), file = 'last_update.csv', row.names = FALSE)
      
      shinyalert("Update complete", "Databases are now synced.", type = "success")
    })
  })
  
  getPage<-function() {
    return(includeHTML("userguide.html"))
  }
  output$userguide<-renderUI({getPage()})
  
  
  
  
  #keep in config if incomplete#
  cobs = observeEvent(input$container,{
    cat(file=stderr(),input$container)
    
    if( !getstatus())
    {
      cat(file=stderr(),'notify')
      updateTabItems(session, 'container','data_config')
      showNotification("Data Setup error. Either upload a valid csv file, or use online data.")
    }
  })
 
  
  
  
  getstatus = function(x)
  {
    
    s = c(input$id, input$switch_entry, input$switch_exit)
    if(all(c(input$id, input$switch_entry, input$switch_exit))){
      
      return(TRUE)}
    
    v = c(is.null( input$file1), is.null(input$file_entry), is.null(input$file_exit))
    cat(file=stderr(),v)
    
    if(sum((s+!v)<1)>0)
    {
      
      return(FALSE)
    }
    else{
      return(TRUE)
    }
  }
  
  # REACTIVE EVENTS UPON LOADING #
  {
    data = eventReactive(input$file1,{
      
      
      
      cat(file=stderr(), 's1')
      inFile <- input$file1
      
      
      req(inFile)
      
      u = read_csv(inFile$datapath)
      index= as.Date(data.frame(u)[,1],"%d/%m/%Y")
      u = xts(u[,-1], order.by = index)
      
        updateSelectInput(session, "inp2","Select UAG Column", choices = colnames(u))
        mind =end(u)- start(u)
        
        updateDateRangeInput(session,'dateRange',start = end(u)- min(60, 0.5*mind), end = end(u))
        
      cat(file=stderr(), 's2')
      u
    })
    
    data_entry = eventReactive(input$file_entry,{
      inFile <- input$file_entry
      req(inFile)
      u = read.csv(inFile$datapath, header = TRUE)
      index = as.Date(data.frame(u)[,1],"%d/%m/%Y")
      u = xts(u[,-1], order.by = index)
      u
    })
    
    data_exit = eventReactive(input$file_exit,{
      inFile <- input$file_exit
      req(inFile)
      u = read.csv(inFile$datapath, header = TRUE)
      index = as.Date(u[,1],"%d/%m/%Y")
      u = xts(u[,-1], order.by = index)
      u
    })
    
    
    
  }
  
  
  
  
  
}