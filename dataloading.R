##Load Local databases##
#supply <- read_csv("supply.csv")

library(XML)
library(rvest)
library(shiny)
library(readxl)
library(shinyWidgets)
library(xts)
library(stringr)
library(changepoint)
library(readr)
library(dygraphs)
library(ecp)
library(bcp)
library(qcc)
library(shinyjs)
library(shinycssloaders)
library(tsoutliers)
library(shinydashboard)
library(DT)
library(readxl)
library(ukgasapi)
library(tidyr)
library(readr)
#library(timeDate)
library(xts)


DL_ENABLE = FALSE ##prevent data updateing by setting FALSE
aux2 <- read_csv("aux2.csv")


#load('Models_R.Rdata')
load('Models.Rdata')



last_update <<-(read_csv("last_update.csv", col_types = cols(`Last Updated` = col_character())))

processuag = function(){  ## UAG Loading function
  ttx <<- NULL
  tt <- read_excel("uag.xlsx")
  tt$'Gas Day' = as.Date(tt$'Gas Day', format = '%d/%m/%Y')
  tt  = tt[,1:5]
  tt = tt %>% drop_na()
  ttx = xts(tt[,2:5], order.by = tt$'Gas Day')
  ttx[is.na(ttx)] <- 0
  colnames(ttx) = c('UAG', 'OUG', 'CV Sh.', 'Total Shrinkage')
  ttx <<- ttx
}


processuag() ##  Load UAG


dllist <- read_csv("dl_keys_xlsx.csv") #relational and qualitative node DB
dl_s= dllist %>% filter(Type == 'Supplies')
dl_d= dllist %>% filter(Type == 'Demand')


##read node flow data##
entryd<- read_csv("entryd.csv", col_types = cols("ApplicableFor" = col_date(format = "%Y-%m-%d")))
exit <- read_csv("exit.csv", col_types = cols("ApplicableFor" = col_date(format = "%Y-%m-%d")))
entrym <- read_csv("entrym.csv", col_types = cols("ApplicableFor" = col_date(format = "%Y-%m-%d")))




updateauxDB = function(){
  last = end(aux_xts)
  if(Sys.Date() - 4 - last > 0)
  {
    entry =getbig(colnames(aux2[-1]),last -40,Sys.Date()-4, 2000)
    entry = entry %>% select(ApplicableFor,Value,PublicationObjectName)
    entry = unique(entry)
    entry = spread(entry, PublicationObjectName,Value)
  if(ncol(entry) != ncol(aux2))
  {
    shinyalert('Error!', 'AUX DB update error: data conformity. Try again later.', type = 'warning')
    return(0)
  }
    colnames(entry)[1]= 'index'
    entry = entry[,colnames(aux2)]
  
    entryd = rbind(aux2, entry)
   ## entryd[is.na(entryd)]= 0
    entryd = unique(entryd)
    write.csv(entryd, 'aux2.csv' , row.names=FALSE)
 
    
    
    
  
  
  }
}




#update DB files#



updatexitDB = function(){
  
  last = exit$ApplicableFor[nrow(exit)]+1
  if(Sys.Date() - 1 - last > 0)
  {
    entry =getbig(dl_d$Vname,last ,Sys.Date()-1, 2000)
    entry = entry %>% select(ApplicableFor,Value,PublicationObjectName)
    entry = unique(entry)
    entry = spread(entry, PublicationObjectName,Value)
    missing = setdiff(colnames(exit), colnames(entry))
    m = matrix(0, nrow = nrow(entry), ncol = length(missing))
    df2 = data.frame(m)
    colnames(df2) = missing
    entry = cbind(entry, df2)
    exit = rbind(exit, entry)
    exit[is.na(exit)]= 0
    exit = unique(exit)
    write.csv(exit, 'exit.csv', row.names=FALSE)
    
  }
  
  
  ##create xts files##
  xxts = xts(exit[,-1], order.by = exit$ApplicableFor)
  colnames(xxts)= dl_d$Name[match(colnames(xxts), dl_d$Vname)]
}

updatentryDB = function(){
  
  last = entryd$ApplicableFor[nrow(entryd)]+1
  if(Sys.Date() - 3 - last > 0)
  {
    entry =getbig(dl_s$Aname,last ,Sys.Date()-3, 2000)
    entry = entry %>% select(ApplicableFor,Value,PublicationObjectName)
    entry = unique(entry)
    entry = spread(entry, PublicationObjectName,Value)
    
    missing = setdiff(colnames(entryd), colnames(entry))
    m = matrix(0, nrow = nrow(entry), ncol = length(missing))
    df2 = data.frame(m)
    colnames(df2) = missing
    entry = cbind(entry, df2)
    
    
    
    entryd = rbind(entryd, entry)
    entryd[is.na(entryd)]= 0
    entryd = unique(entryd)
    write.csv(entryd, 'entryd.csv' , row.names=FALSE)
  }
  
  
  last =entrym$ApplicableFor[nrow(entrym)] ## removed -1 
  if(Sys.Date() - 60 - last > 0)
  {
    entry =getbig(dl_s$Vname,last ,Sys.Date()-60, 2000)
    entry = entry %>% select(ApplicableFor,Value,PublicationObjectName)
    entry = unique(entry)
    entry = spread(entry, PublicationObjectName,Value)
    missing = setdiff(colnames(entrym), colnames(entry))
    m = matrix(0, nrow = nrow(entry), ncol = length(missing))
    df2 = data.frame(m)
    colnames(df2) = missing
    entry = cbind(entry, df2)
    entrym = rbind(entrym, entry)
    entrym = unique(entrym)
    entrym[is.na(entrym)]= 0
    write.csv(entrym, 'entrym.csv' , row.names=FALSE)
    
    
    
  }
  
  nxts_d = xts(entryd[,-1], order.by = entryd$ApplicableFor)
  colnames(nxts_d)= dl_s$Name[match(colnames(nxts_d), dl_s$Aname)]
  nxts_m = xts(entrym[,-1], order.by = entrym$ApplicableFor)
  colnames(nxts_m)= dl_s$Name[match(colnames(nxts_m), dl_s$Vname)]
  monthaplics = index(nxts_m)
  dayaplics = seq(from = (index(nxts_m)[nrow(nxts_m)]+1), to = index(nxts_d)[nrow(nxts_d)], by = 'day')
  rbind(nxts_m, nxts_d[dayaplics,])
  
  
  
}





##create xts files##
xxts = xts(exit[,-1], order.by = exit$ApplicableFor)
colnames(xxts)= dl_d$Name[match(colnames(xxts), dl_d$Vname)]
nxts_d = xts(entryd[,-1], order.by = entryd$ApplicableFor)
colnames(nxts_d)= dl_s$Name[match(colnames(nxts_d), dl_s$Aname)]
nxts_m = xts(entrym[,-1], order.by = entrym$ApplicableFor)
colnames(nxts_m)= dl_s$Name[match(colnames(nxts_m), dl_s$Vname)]
monthaplics = index(nxts_m)
dayaplics = seq(from = (index(nxts_m)[nrow(nxts_m)]+1), to = index(nxts_d)[nrow(nxts_d)], by = 'day')
nxts = rbind(nxts_m, nxts_d[dayaplics,])
nxts[is.na(nxts)] = 0
xxts[is.na(xxts)] = 0

#######



aux_xts = xts(data.frame(aux2[,-1]), order.by = aux2[[1]])
colnames(aux_xts) = colnames(aux2)[-1]


order_ldz = c('EA', 'EM','NE', 'NO','WN','NW','SC','SE','SO','SW','WM','NT','WS')
LDZ_xts = aux_xts[,3:15]
colnames(LDZ_xts) = order_ldz

ldzs = unique(dllist$LDZ)[-1]
day_dummies = dummy_cols(format(index(xxts), '%A'))
colnames(LDZ_xts) = order_ldz
hols = format(index(apply.yearly(xxts, sum)),'%Y') %>% as.numeric%>% timeDate::holidayLONDON()
day_dummiesx = xts(day_dummies[,-1], order.by =index(xxts))
day_dummiesx$isHoliday = 0
day_dummiesx$isHoliday[hols] = 1
df = dllist[which(dllist$Stype == 'NTS Offtake'),]
colnames(day_dummiesx) = c('Tuesday', 'Wednesday','Thursday','Friday','Saturday','Sunday','Monday','Holidays')


u=0 #????
uagloaded = FALSE #?????

