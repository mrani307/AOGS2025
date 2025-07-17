


prepare_dist.cropInfo<-function(india.cropStats, start.year,end.year,rep.cropNames,dist.name,state.name)
{
  #extracting the name of the states from the above file
  lapply(india.cropStats[,1] %>% unlist %>% na.omit(), 
         function(x) if( substr(x,1,1) %>% utf8ToInt() %in% 48:57 ) return(x)) %>% 
    unlist -> states
  
  states %>% 
    sub("^[^. ]*\\. ", "", .) %>% 
    is_in(state.name) %>%
    which %>%
    states[[.]] -> state.name
  
  #extracting data for each state-----------------------------------------------
  
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
  
  rm(india.cropStats,st.1,st.2,pos.1,pos.2,s,states)
  
  #extracting data for a particular state-----------------------------------
  
  cropStats <- state.cropStats[[state.name]]
  
  row.no <- which(!is.na(cropStats[,2]))
  row.no <- c(row.no,(nrow(cropStats)+1))
  
  cropStats.dist<-list()
  
  for(c in 1:(length(row.no)-1))
  {
    start <- row.no[c]
    end   <- row.no[c+1]-1
    dist_name <- cropStats[start,2] %>% unlist
    crop.stat <- cropStats[(start):(end),]
    colnames(crop.stat)<-colnames(cropStats)
    
    cropStats.dist[[dist_name]] <- crop.stat
  }
  
  rm(state.cropStats,cropStats, row.no, c,dist_name, start,end,crop.stat,state.name)
  
  #extracting crop stats for the required district--------------------------
  
  cropStats.dist %>% 
    names %>% 
    sub("^[^. ]*\\. ", "", .) %>% 
    is_in(dist.name) %>%
    which %>%
    cropStats.dist[[.]] -> cropData.dist
  
  rm(cropStats.dist)
  
  #extracting required crop data--------------------------------------------
  
  lapply(rep.cropNames ,function(crop.name) colnames(cropData.dist) %>% is_in(crop.name) %>% which) %>% unlist() -> col
  
  avail.years <- cropData.dist$Year %>% substr(.,1,4) %>% as.numeric()
  if(avail.years[1]>start.year | avail.years[length(avail.years)]<end.year)
  {
    print(paste0("Crop statistics for ",dist.name," is available from ",avail.years[1]," to ",avail.years[length(avail.years)]))
    stop("Please chaange the start.year and end.year")
  }
  
  rows <- which(avail.years %in% c(start.year:end.year))
  
  lapply(col, function(c) cropData.dist[rows,(c:(c+2))] %>% sapply(., as.numeric) %>% as.data.frame() ) -> dist.cropInfo
  lapply(dist.cropInfo, function(df) {
    colnames(df) <- c("Area","Production","Yeild")
    rownames(df) <- c(start.year:end.year)
    return(df)
  }) -> dist.cropInfo
  names(dist.cropInfo) <- rep.cropNames
  
  rm(cropData.dist,col,rows,avail.years)
  
  return(dist.cropInfo)  
}

suppressMessages( india.cropStats<- read_excel("crop_statistics_kharif.xlsx") )
bajra <- prepare_dist.cropInfo(india.cropStats=india.cropStats,
                               start.year=2001,end.year=2019,
                               rep.cropNames="Bajra",dist.name="Barmer",state.name="Rajasthan")
maize <- prepare_dist.cropInfo(india.cropStats=india.cropStats,
                               start.year=2001,end.year=2019,
                               rep.cropNames="Maize",dist.name="Belgaum",state.name="Karnataka")
ragi  <- prepare_dist.cropInfo(india.cropStats=india.cropStats,
                               start.year=2001,end.year=2019,
                               rep.cropNames="Ragi",dist.name="Tumkur",state.name="Karnataka")
arhar <- prepare_dist.cropInfo(india.cropStats=india.cropStats,
                               start.year=2001,end.year=2019,
                               rep.cropNames="Arhar/Tur",dist.name="Gulbarga",state.name="Karnataka")
soyabean <- prepare_dist.cropInfo(india.cropStats=india.cropStats,
                                  start.year=2001,end.year=2019,
                                  rep.cropNames="Soyabean",dist.name="Ujjain",state.name="Madhya Pradesh")
groundnut <- prepare_dist.cropInfo(india.cropStats=india.cropStats,
                                   start.year=2001,end.year=2019,
                                   rep.cropNames="Groundnut",dist.name="Anantapur",state.name="Andhra Pradesh")
cotton <- prepare_dist.cropInfo(india.cropStats=india.cropStats,
                                start.year=2001,end.year=2019,
                                rep.cropNames="Cotton(lint)",dist.name="Jalgaon",state.name="Maharashtra")
sugarcane <- prepare_dist.cropInfo(india.cropStats=india.cropStats,
                                  start.year=2001,end.year=2019,
                                  rep.cropNames="Sugarcane",dist.name="Kheri",state.name="Uttar Pradesh")



suppressMessages( india.cropStats<- read_excel("crop_statistics_rabi.xlsx") )
jowar <- prepare_dist.cropInfo(india.cropStats= india.cropStats,
                               start.year=2001,end.year=2019,
                               rep.cropNames="Jowar",dist.name="Solapur",state.name="Maharashtra")
barley <- prepare_dist.cropInfo(india.cropStats= india.cropStats,
                                start.year=2001,end.year=2019,
                                rep.cropNames="Barley",dist.name="Jaipur",state.name="Rajasthan")
wheat <- prepare_dist.cropInfo(india.cropStats= india.cropStats,
                                start.year=2001,end.year=2019,
                                rep.cropNames="Wheat",dist.name="Sangrur",state.name="Punjab")

crop<-list(bajra,maize,ragi,arhar,soyabean,groundnut,cotton ,sugarcane,jowar,barley,wheat)
crop<-lapply(crop,function(ls) ls[[1]][,1]) %>% do.call(cbind,.)
colnames(crop)<-c("bajra (Barmer)",
                  "maize (Belagum)",
                  "ragi (Tumkur)",
                  "arhar (Gulbarga)",
                  "soyabean (Ujjain)",
                  "groundnut (Anantapur)",
                  "cotton (Jalgaon)" ,
                  "sugarcane (Kheri)",
                  "jowar (Solapur)",
                  "barley (Jaipur)",
                  "wheat (Sangrur)")

#write.csv(crop,"crop.csv",row.names = F)

crop<-crop/10^5
crop1<-crop[,c(1,5,6,9)]
crop2<-crop[,c(2,3,8,10)]
crop3<-crop[,c(4,7,11)]
color<-c(pal_npg("nrc", alpha = 0.7)(10),scales::hue_pal()(11)[2])

png("plot2.png",units = "in",res = 900,height = 7,width = 15)
par(mfrow = c(1, 3))
par(bg = "black") 
#par(mar = c(1,,1,1))
par(mar = c(5, 3, 3, 1))  # outer margin: bottom, left, top, right
par(cex.axis = 1.5)
p<-crop1
r<-range(p,na.rm=T)
plot(2001:2019,p[,1], type="b", ylab = NA,
     col.axis = "white",  col.ticks = "white",  col.lab = "white",
     ylim =c(r[1],r[2]+0.5),col=color[1], lwd=2)
box(col="white")
grid(col="grey20")
for(i in 2:4)
  lines(2001:2019,p[,i],col=color[i], type="b", lwd=2)
legend("top",legend = colnames(p),text.col = "white", col = color[1:4],bty='n',ncol = 2,lty=1, lwd=2,pch=20,cex=1.2)
p<-crop2
r<-range(p,na.rm=T)
plot(2001:2019,p[,1], type="b", ylab = NA,
     col.axis = "white",  col.ticks = "white",  col.lab = "white",
     ylim =c(r[1],r[2]),col=color[5], lwd=2)
box(col="white")
grid(col="grey20")
for(i in 2:4)
  lines(2001:2019,p[,i], type="b",col=color[4+i], lwd=2)
legend("topleft",legend = colnames(p),text.col = "white", bty='n',ncol = 2,col=color[5:8],lty=1, lwd=2,pch=20,cex=1.2)
p<-crop3
r<-range(p,na.rm=T)
plot(2001:2019,p[,1], type="b", ylab = NA,
     col.axis = "white",  col.ticks = "white",  col.lab = "white",
     ylim =c(r[1],r[2]),col=color[9], lwd=2)
box(col="white")
grid(col="grey20")
for(i in 2:3)
  lines(2001:2019,p[,i], type="b",col=color[8+i], lwd=2)
legend("top",legend = colnames(p),text.col = "white", col = color[9:11],bty='n',ncol = 2,lty=1, lwd=2, pch=20,cex=1.2)
dev.off()

