mscale =  function(x) {(x-min(x))/(max(x)-min(x))}
rescale = function(x,to){
  return((to[2]-to[1])*(x-min(x))/(max(x)-min(x))+to[1])
  
  
  
  
}
customrol = function(x,width,frame)
{
r=frame[1,]
  for (i in 1:(dim(frame)[1]-width)) {
    
    t =sapply(frame,function(y) {
     o= ccf(y[i:(i+width-1)], x[i:(i+width-1)], lag.max =3, plot = FALSE)$acf
     v = o[which.max(abs(o))]
     return(v) 
     }
      )
    r[i,] = as.numeric(t)
  }
  return(r)
}
customroldiff = function(x,width,frame)
{
  r=frame[1,]
  for (i in 1:(dim(frame)[1]-width)) {
    
    t =sapply(frame,function(y) 
    {
      o= ccf(diff(y[i:(i+width-1)]), x[i:(i+width-1)], lag.max =3, plot = FALSE)$acf
      v = o[which.max(abs(o))]
      return(v) 
      
    }
    )
    
     r[i,] = as.numeric(t)
  }
  return(r)
}

crep = function(x, width, frame)
{

  assign('comp',x, envir = .GlobalEnv) 
  assign('targetxts', frame,envir = .GlobalEnv) 
  assign('n',customrol(x, width,as.data.frame(frame)),envir = .GlobalEnv)
  assign('d' ,customroldiff(x,width, as.data.frame(frame)), envir = .GlobalEnv)
  n[is.na(n)] = 0
  d[is.na(d)] = 0 
  assign('nsummary' ,apply(n,2,absmax), envir = .GlobalEnv)
  assign('dsummary' ,apply(d,2,absmax), envir = .GlobalEnv)
  assign('ss' ,colMeans(NA^(abs(n)==0)*abs(n), na.rm=TRUE), envir = .GlobalEnv)
  assign('ds' ,colMeans(NA^(abs(d)==0)*abs(n), na.rm=TRUE), envir = .GlobalEnv)
  assign('gwidth' , width, envir = .GlobalEnv)
  assign('gname' ,deparse(substitute(x)), envir = .GlobalEnv)
  ss[is.na(ss)] = 0
  ds[is.na(ds)] = 0 
  prep()
}

prep = function(){  
  
  fname =  paste(gname,gwidth,format(Sys.time(),'%m %d %H %M %S'),'.pdf')
rmarkdown::render('reportccf.Rmd',output_file = fname)


path = 'C:/phd/year 5/R project/'
pf = paste(path,fname, sep = '')
system(paste0('open "',pf, '"'))
}

absmax <- function(x) { x[which.max( abs(x) )]}
sort_abs <- function(x, na.last = TRUE, decreasing = FALSE) {
  x[order(abs(x), na.last = na.last, decreasing = decreasing)] 
}
initr = function(x)
{
  
  
  offp = x/ rowSums(x)
  offa = apply(offp, 2, function(x) mean(x[which(x!=0)]))
  offp0 = apply(x, 2, function(x) mean(!x))
  o = data.frame(average=offa,cumulative = offa,active = offp0)
  o$mean = apply(x,2,function(x) mean(x[which(x!=0)]))
  o$sd= apply(x,2,function(x) sd(x[which(x!=0)]))
  o[is.na(o)]= 0
  o$active = 1 -o$active
  o$TotalGas = colSums(x)
  o$PropTotal = o$TotalGas/sum(x)
  o = o[order(o$TotalGas),]
  o$cumulative = cumsum(o$TotalGas) / sum(x)
  o = o[order(o$TotalGas, decreasing = TRUE ),]
  o[is.na(o)]= 0
  colnames(o) = c('Average daily flow %', 'Cumulative total of flow', '% active','Mean','Standard deviation','Total Gas in period', 'Proportion of total')
  return(o)
}


pareto.MLE <- function(X)
{
  n <- length(X)
  m <- min(X)
  a <- n/sum(log(X)-log(m))
  return( c(m,a) ) 
}
rollinglag = function(x,width,frame)
{
  r=frame[1,]
  for (i in 1:(dim(frame)[1]-width)) {
    
    t =sapply(frame,function(y) {
      o= ccf(y[i:(i+width-1)], x[i:(i+width-1)], lag.max =30, plot = FALSE)
      v = o$lag[which.max(abs(o$acf))]
      return(v) 
    }
    )
    r[i,] = as.numeric(t)
  }
  return(r)
}

cleanp = function(x)
{
  return(sapply(d910[,2:45], function(y) {
    
    sapply(y, function(x) {
      if(x<0.05*mean(y[which(y!=0)])){
        x=0
      }else{x=x}
    })}))
}



rolling_granger = function(x,width,frame,c)
{
  r=frame[1,]
  for (i in 1:(dim(frame)[1]-width)) {
    
    t =sapply(frame,function(y) {
      if(sum(y[i:(i+width-1)]==0)<0.9*width )
      {
       
      #  w=grangertest(y[i:(i+width-1)]~x[i:(i+width-1)], order = c)$`Pr(>F)`[2]
       w=grangertest(x[i:(i+width-1)]~y[i:(i+width-1)], order = c)$`Pr(>F)`[2]
        return(w)
      }
      
      else{
        
        print('zeros')
        flush.console()
        return(1)
        
        }
    }
    )
    r[i,] = as.numeric(t)
    print(paste(i/(dim(frame)[1]-width),' ',i))
    flush.console()
  }
  return(r)
}


##this function returns a cbind of the overlapping indexes only
xts_merge_exist = function(a,b)
{
  start = max(start(a),start(b))
  end = min(end(a), end(b))
  t = cbind(a,b)
  days = seq(from = start, to = end,by = 'day')
  return(t[days])
}
convert_types <- function(x) {
  stopifnot(is.list(x))
  x[] <- rapply(x, utils::type.convert, classes = "character",
                how = "replace")#, as.is = TRUE)
  return(x)
}
xts_common_days = function(a,b)
{
  start = max(start(a),start(b))
  end = min(end(a), end(b))
  days = seq(from = start, to = end,by = 'day')
  return(days)
}

daychanges = function(x,y)
{
  x = as.data.frame(x)
  which(!(x[y,] >0)&(x[y-1,]>0))
}

## returns number of nonzero rows##
nonzero_rows = function(y)
{
  return(apply(y,1,function(x){sum(x>0)}))
}
library(xml2)
library(rvest)
library(readxl)
# processuag = function(){
#   
#   tt <- read_excel("uag.xlsx")
#   tt$'Gas Day' = as.Date(tt$'Gas Day', format = '%d/%m/%Y')
#   tt  = tt[,1:5]
#   ttx = xts(tt[,2:5], order.by = tt$'Gas Day')
#   ttx[is.na(ttx)] <- 0
#   colnames(ttx) = c('UAG', 'OUG', 'CV Sh.', 'Total Shrinkage')
#   return(ttx)
# }
# getuag= function()
# {
#   downloaduag()
#   return(processuag())
# }
# 
# downloaduag = function(){
#   url = 'https://www.nationalgrid.com/uk/gas/balancing/unaccounted-gas-uag'
#   t =read_html(url) %>%
#     html_nodes("a") %>%       # find all links
#     html_attr("href")%>% # get the url
#     str_subset("nodes")                   # look at the first one
#   destfile <- "uag.xlsx"
#   curl::curl_download(t, destfile)
# }





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
  return(tt[-1])
  
}
tsoutliers <- function(x,plot=FALSE)
{
  x <- as.ts(x)
  if(frequency(x)>1)
    resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
  else
  {
    tt <- 1:length(x)
    resid <- residuals(loess(x ~ tt))
  }
  resid.q <- quantile(resid,prob=c(0.25,0.75))
  iqr <- diff(resid.q)
  limits <- resid.q + 1.5*iqr*c(-1,1)
  score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
  if(plot)
  {
    plot(x)
    x2 <- ts(rep(NA,length(x)))
    x2[score>0] <- x[score>0]
    tsp(x2) <- tsp(x)
    points(x2,pch=19,col="red")
    return(invisible(score))
  }
  else
    return(score)
}

###




