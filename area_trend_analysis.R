#' check(plot) the number of districts over India showing significantly increasing or decreasing 
#' trends in crop statistics (area/yield)
#' The results are in percentage, i.e., the percentage of districts having significantly increasing trend in 
#' crop yield of Rice, out of the total number of districts growing rice for atleast 10 years. [Time period : 2001-2020]

library(readxl)
library(dplyr)
library(magrittr)
library(RColorBrewer)

#functions----------------------------------------------------------------------

calcTrend<- function(x) 
{
  
  x    <- as.numeric(x)
  time <- seq_along(x)
  
  # Remove missing values
  valid <- complete.cases(x, time)
  x <- x[valid]
  time <- time[valid]
  
  # Fit linear model
  model <- lm(x ~ time)
  summary_model <- summary(model)
  
  slope <- coef(model)[2]
  p_value <- summary_model$coefficients[2, 4]  # 2nd row, 4th column (p-value of slope)
  
  return(list(trend = slope[[1]], p_value = p_value))
}

#inputs ------------------------------------------------------------------------

  pc.name <- "C:/Users/Admin"
  setwd(paste0(pc.name,"/OneDrive - iitr.ac.in/PhD_Research/WaterAccounting/AOGS2025")) #set directory  
  
#kharif-------------------------------------------------------------------------
  
  suppressMessages( india.cropStats<- read_excel("crop_statistics_kharif.xlsx") )

#data preparation---------------------------------------------------------------

  #extracting the name of the states from the above file
  lapply(india.cropStats[,1] %>% unlist %>% na.omit(), 
         function(x) if( substr(x,1,1) %>% utf8ToInt() %in% 48:57 ) return(x)) %>% 
    unlist -> states
  
  #extracting data(APY) for each state
  state.cropStats <- list()
  for(s in 2:(length(states)+1))
  {
    if(s <= length(states))
    {
      st.1 <- states[s-1]
      st.2 <- states[s]
      
      pos.1 <- which(india.cropStats[,1]==st.1)
      pos.2 <- which(india.cropStats[,1]==st.2)
      
      state.cropStats[[st.1]]<-india.cropStats[pos.1:(pos.2-1),]
    }
    else
    {
      st.1 <- states[s-1]
      
      pos.1 <- which(india.cropStats[,1]==st.1)
      pos.2 <- nrow(india.cropStats)
      
      state.cropStats[[st.1]]<-india.cropStats[pos.1:(pos.2-1),]
    }
  }
  rm(india.cropStats,st.1,st.2,pos.1,pos.2,s)

  #extracting data(APY) for each district
  cropStats.dist<-list()
  for( state.name in states)
  {
    
    print(state.name)
    
    cropStats <- state.cropStats[[state.name]]
    
    row.no <- which(!is.na(cropStats[,2]))
    row.no <- c(row.no,(nrow(cropStats)+1))
    
    for(c in 1:(length(row.no)-1))
    {
      start <- row.no[c]
      end   <- row.no[c+1]-1
      dist_name <- cropStats[start,2] %>% unlist
      crop.stat <- cropStats[(start):(end),]
      colnames(crop.stat)<-colnames(cropStats)
      
      cropStats.dist[[dist_name]] <- crop.stat
    }
    
  }
  rm(cropStats, row.no, c,dist_name, start,end,crop.stat)

#trend analysis-----------------------------------------------------------------

  kh.per<-list()
  
  kharif <- list()
  for(cropname in c("Bajra","Rice","Jowar","Maize","Ragi","Groundnut","Soyabean","Cotton(lint)","Sugarcane","Sesamum","Arhar/Tur","Moong(Green Gram)"))
  {
    cropArea.ts<-lapply(cropStats.dist, function(df) df[,c(3,which(colnames(df)==cropname))]) #Time series of Rice crop area for each district
    pos<-lapply(cropArea.ts,function(df) ifelse(nrow(na.omit(as.data.frame(df)))>12,1,0)) %>% unlist() %>% is_in(1) %>% which()#selecting districts with more than 12 years of data
    cropArea.ts<-cropArea.ts[pos] #time series of Rice crop ares for districts having more than 12 years of data
    
    tr <- lapply(cropArea.ts,function(df) calcTrend(unlist(df[,2])))%>% do.call(rbind,.) %>% as.data.frame() #calculating trend and p-value of rice area tineseries for all the districts
    pos <- which(tr$p_value<0.05) #selecting districts having significant trend
    trSig <- tr[pos,] #selecting only the significant districts
    
    incTr <- trSig[which(trSig$trend>0),]
    decTr <- trSig[which(trSig$trend<0),]
    
    kh.per[[cropname]]<-c( nrow(tr),nrow(incTr),nrow(decTr))
    
    kharif[[paste0(cropname,"inc")]]<-unlist(incTr$trend)
    kharif[[paste0(cropname,"dec")]]<-(-unlist(decTr$trend))
  }
  
  boxplot(kharif,outline = F)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  
  suppressMessages( india.cropStats<- read_excel("crop_statistics_summer.xlsx") )
  
  #data preparation---------------------------------------------------------------
  
  #extracting the name of the states from the above file
  lapply(india.cropStats[,1] %>% unlist %>% na.omit(), 
         function(x) if( substr(x,1,1) %>% utf8ToInt() %in% 48:57 ) return(x)) %>% 
    unlist -> states
  
  #extracting data(APY) for each state
  state.cropStats <- list()
  for(s in 2:(length(states)+1))
  {
    if(s <= length(states))
    {
      st.1 <- states[s-1]
      st.2 <- states[s]
      
      pos.1 <- which(india.cropStats[,1]==st.1)
      pos.2 <- which(india.cropStats[,1]==st.2)
      
      state.cropStats[[st.1]]<-india.cropStats[pos.1:(pos.2-1),]
    }
    else
    {
      st.1 <- states[s-1]
      
      pos.1 <- which(india.cropStats[,1]==st.1)
      pos.2 <- nrow(india.cropStats)
      
      state.cropStats[[st.1]]<-india.cropStats[pos.1:(pos.2-1),]
    }
  }
  rm(india.cropStats,st.1,st.2,pos.1,pos.2,s)
  
  #extracting data(APY) for each district
  cropStats.dist<-list()
  for( state.name in states)
  {
    
    print(state.name)
    
    cropStats <- state.cropStats[[state.name]]
    
    row.no <- which(!is.na(cropStats[,2]))
    row.no <- c(row.no,(nrow(cropStats)+1))
    
    for(c in 1:(length(row.no)-1))
    {
      start <- row.no[c]
      end   <- row.no[c+1]-1
      dist_name <- cropStats[start,2] %>% unlist
      crop.stat <- cropStats[(start):(end),]
      colnames(crop.stat)<-colnames(cropStats)
      
      cropStats.dist[[dist_name]] <- crop.stat
    }
    
  }
  rm(cropStats, row.no, c,dist_name, start,end,crop.stat)
  
  #trend analysis-----------------------------------------------------------------
  sm.per<-list()
  summer<-list()
  for(cropname in c("Bajra","Rice","Jowar","Maize","Ragi","Groundnut","Cotton(lint)","Sugarcane","Moong(Green Gram)"))
  {
    cropArea.ts<-lapply(cropStats.dist, function(df) df[,c(3,which(colnames(df)==cropname))]) #Time series of Rice crop area for each district
    pos<-lapply(cropArea.ts,function(df) ifelse(nrow(na.omit(as.data.frame(df)))>12,1,0)) %>% unlist() %>% is_in(1) %>% which()#selecting districts with more than 12 years of data
    if(length(pos)>0)
    {
      cropArea.ts<-cropArea.ts[pos] #time series of Rice crop ares for districts having more than 12 years of data
      
      tr <- lapply(cropArea.ts,function(df) calcTrend(unlist(df[,2])))%>% do.call(rbind,.) %>% as.data.frame() #calculating trend and p-value of rice area tineseries for all the districts
      pos <- which(tr$p_value<0.05) #selecting districts having significant trend
      trSig <- tr[pos,] #selecting only the significant districts
      
      incTr <- trSig[which(trSig$trend>0),]
      decTr <- trSig[which(trSig$trend<0),]
      
      sm.per[[cropname]]<-c( nrow(tr),nrow(incTr),nrow(decTr))
      
      summer[[paste0(cropname,"inc")]]<-unlist(incTr$trend)
      summer[[paste0(cropname,"dec")]]<-(-unlist(decTr$trend))
    }
  }
  boxplot(summer)  
  
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  
  suppressMessages( india.cropStats<- read_excel("crop_statistics_rabi.xlsx") )
  
  #data preparation---------------------------------------------------------------
  
  #extracting the name of the states from the above file
  lapply(india.cropStats[,1] %>% unlist %>% na.omit(), 
         function(x) if( substr(x,1,1) %>% utf8ToInt() %in% 48:57 ) return(x)) %>% 
    unlist -> states
  
  #extracting data(APY) for each state
  state.cropStats <- list()
  for(s in 2:(length(states)+1))
  {
    if(s <= length(states))
    {
      st.1 <- states[s-1]
      st.2 <- states[s]
      
      pos.1 <- which(india.cropStats[,1]==st.1)
      pos.2 <- which(india.cropStats[,1]==st.2)
      
      state.cropStats[[st.1]]<-india.cropStats[pos.1:(pos.2-1),]
    }
    else
    {
      st.1 <- states[s-1]
      
      pos.1 <- which(india.cropStats[,1]==st.1)
      pos.2 <- nrow(india.cropStats)
      
      state.cropStats[[st.1]]<-india.cropStats[pos.1:(pos.2-1),]
    }
  }
  rm(india.cropStats,st.1,st.2,pos.1,pos.2,s)
  
  #extracting data(APY) for each district
  cropStats.dist<-list()
  for( state.name in states)
  {
    
    print(state.name)
    
    cropStats <- state.cropStats[[state.name]]
    
    row.no <- which(!is.na(cropStats[,2]))
    row.no <- c(row.no,(nrow(cropStats)+1))
    
    for(c in 1:(length(row.no)-1))
    {
      start <- row.no[c]
      end   <- row.no[c+1]-1
      dist_name <- cropStats[start,2] %>% unlist
      crop.stat <- cropStats[(start):(end),]
      colnames(crop.stat)<-colnames(cropStats)
      
      cropStats.dist[[dist_name]] <- crop.stat
    }
    
  }
  rm(cropStats, row.no, c,dist_name, start,end,crop.stat)
  
  #trend analysis-----------------------------------------------------------------
  
  rb.per<-list()
  
  rabi<-list()
  for(cropname in c("Bajra","Rice","Jowar","Maize","Ragi","Groundnut","Cotton(lint)","Sugarcane","Wheat","Barley","Moong(Green Gram)","Rapeseed &Mustard"))
  {
    cropArea.ts<-lapply(cropStats.dist, function(df) df[,c(3,which(colnames(df)==cropname))]) #Time series of Rice crop area for each district
    pos<-lapply(cropArea.ts,function(df) ifelse(nrow(na.omit(as.data.frame(df)))>12,1,0)) %>% unlist() %>% is_in(1) %>% which()#selecting districts with more than 12 years of data
    if(length(pos)>0)
    {
      cropArea.ts<-cropArea.ts[pos] #time series of Rice crop ares for districts having more than 12 years of data
      
      tr <- lapply(cropArea.ts,function(df) calcTrend(unlist(df[,2])))%>% do.call(rbind,.) %>% as.data.frame() #calculating trend and p-value of rice area tineseries for all the districts
      pos <- which(tr$p_value<0.05) #selecting districts having significant trend
      trSig <- tr[pos,] #selecting only the significant districts
      
      incTr <- trSig[which(trSig$trend>0),]
      decTr <- trSig[which(trSig$trend<0),]
      
      rb.per[[cropname]]<-c( nrow(tr),nrow(incTr),nrow(decTr))
      
      rabi[[paste0(cropname,"inc")]]<-unlist(incTr$trend)
      rabi[[paste0(cropname,"dec")]]<-(-unlist(decTr$trend))
    }
  }
  
  
boxplot(rabi)  
  
#-----------------------


crop<-kharif

for(name in names(rabi))
{
  pos<-which(names(crop) == name)
  if(length(pos)==1)
    crop[[name]]<-c(crop[[name]],rabi[[name]])
  else
    crop[[name]]<-rabi[[name]]
} 
for(name in names(summer))
{
  pos<-which(names(crop) == name)
  if(length(pos)==1)
    crop[[name]]<-c(crop[[name]],summer[[name]])
  else
    crop[[name]]<-summer[[name]]
} 

rm(states,name,pc.name,pos,state.name,tr,trSig,state.cropStats,incTr,decTr,cropStats.dist,cropArea.ts,cropname)
boxplot(crop,log="y",outline = F)
