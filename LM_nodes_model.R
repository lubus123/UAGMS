#GET LDZ DEMAND

ldzs = unique(dllist$LDZ)[-1]

LDZ_xts = aux_xts[,4:15]
order_ldz = c('EM','NE', 'NO','WN','NW','SC','SE','SO','SW','WM','NT','WS')
colnames(LDZ_xts) = order_ldz
LDZsums = sapply(order_ldz,function(x) {rowSums(xxts[,dllist[which(dllist$LDZ == x),]$Name]) %>% as.numeric}) %>% xts(order.by = index(xxts))


library(fastDummies)
library(magrittr)


day_dummies = dummy_cols(format(index(xxts), '%A'))

##include storage pumping in model?

#holidays 

hols = format(index(apply.yearly(xxts, sum)),'%Y') %>% as.numeric%>% holidayLONDON()
 day_dummiesx = xts(day_dummies[,-1], order.by =index(xxts))
day_dummiesx$isHoliday = 0
day_dummiesx$isHoliday[hols] = 1
df = dllist[which(dllist$Stype == 'NTS Offtake'),]

  get_LDZs = function(name,ldz)
{
 LDZsums =  rowSums(xxts[,dllist[which(dllist$LDZ == ldz & dllist$Name != name),]$Name]) %>% as.numeric %>% xts(order.by = index(xxts))
  colnames(LDZsums) = 'LDZ Sum'
  return(LDZsums)
}

  colnames(day_dummiesx) = c('Tuesday', 'Wednesday','Thursday','Friday','Saturday','Sunday','Monday','Holidays')
             Models = list()
      for(i in 1:nrow(df))       
             
             {
        x=df[i,]
   o = merge(xxts[,x$Name],day_dummiesx, get_LDZs(x$Name, x$LDZ), LDZ_xts[,x$LDZ]) 
 
     o = o[which(complete.cases(o)),] %>% data.frame 
     colnames(o)[c(ncol(o),ncol(o)-1)]= c('LDZ Demand', 'LDZ Weather')
   p = step(lm(as.formula(paste(colnames(o)[1],'~.',collapse = '')), data=o))
   model = lm(data= o, formula(p))
   Models[[as.name(df[i,]$Name)]] =list(formula = formula(p),r2 =
                 summary(model)$adj.r.squared, model= model )
   
   
   
 } 
