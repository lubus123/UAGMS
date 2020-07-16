library(ukgasapi)
library(dplyr)
library(tidyr)
getbig = function(vector,start, end,smod=3000,prot = 0)
{
  days = difftime(start,end)
  
  size = length(vector)
  days = abs(as.numeric(days))
  stepsize = smod/size
  
  print(days, days/365)
  
  if(days*size<=3000)
  {
    response= dataItemExplorer(vector,
                               fromdate = start,
                               todate=end)
    return(response)
  }
  start = as.Date(start)
  end = as.Date(end)
  mid = start
  splits = ceiling(days/stepsize)
  print(splits)
  print((days*size))
  print(c('Step Size ',stepsize))
  tt = data.frame('ApplicableAt'=as.Date('2011-10-10'),"ApplicableFor"=as.Date('2011-10-10') ,"Value"=0 ,"GeneratedTimeStamp" =0  ,"QualityIndicator"='l',"Substituted"=0,"CreatedDate"=as.Date('2011-10-10') ,"PublicationObjectName"='na')
  for(i in 1:splits){
    mid = start+ (i-1)*stepsize
    eend =start + i*stepsize
    if(eend>Sys.Date())
    {
      eend = as.Date(Sys.Date())-prot
    }
    print(c(mid,eend))
    flush.console()
    if(as.Date(mid)<=Sys.Date()){
      
      
      test = dataItemExplorer(vector,mid,eend)
      #colnames(tt) = colnames(test)
      tt= rbind(tt,test)
    }
  }
  return(tt[-1,])
  
}

#exit=getbig(dl_keys$Vname,'2014-01-01',Sys.Date()-50, 2000)
#exit = exit %>% select(ApplicableFor,Value,PublicationObjectName)
#exit = unique(exit)
#exit = spread(exit, PublicationObjectName,Value)

######################
library(xml2)
library(rvest)
library(readxl)
library(stringr)
library(xts)
processuag = function(){
  
  tt <- read_excel("uag.xlsx")
  tt$'Gas Day' = as.Date(tt$'Gas Day', format = '%d/%m/%Y')
  tt  = tt[,1:5]
   tt = na.omit(tt)
  ttx = xts(tt[,c(2,4,5)], order.by = tt$'Gas Day')
  ttx[is.na(ttx)] <- 0
  colnames(ttx) = c('UAG', 'OUG', 'CV Sh.')
  return(ttx)
}
getuag= function()
{
  downloaduag()
  return(processuag())
}

downloaduag=function(){
  url = 'https://www.nationalgrid.com/uk/gas-transmission/balancing/unaccounted-gas-uag'
  t =read_html(url) %>%
    html_nodes("a") %>%       # find all links
    html_attr("href")%>% # get the url
    str_subset("daily") 
  t=t[1]# look at the first one
  destfile <- "uag.xlsx"
  curl::curl_download(t, destfile)
}



