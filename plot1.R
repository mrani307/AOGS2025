  # --- Data Setup ---
  
  #crop <- kharif
  #stack_vals <- kh.per
  
  # crop <- summer[c(3,4,7:14)]
  # stack_vals<-sm.per[c(2,4:7)]

   crop <- rabi[c(3:20)]
   stack_vals<-rb.per[c(2:10)]

png("rabi.png",units = "in",res = 1200,height = 7,width = 13)  
  
  
  groups <- unique(substr(names(crop),1,nchar(names(crop))-3))
  
  # Boxplot data
  box_data<-list()
  for(i in 1:length(crop))
  {
    name <- names(crop)[i]
    cropName <- substr(name,1,nchar(name)-3)
    sub <- substr(name,nchar(name)-2,nchar(name))
    box_data[[cropName]][[sub]]<-crop[[i]]
  } 
  
  
  # 3-part stacked bars (summing to 100%)
  stack_vals<-lapply(stack_vals, function(a) c(a[2]/a[1],a[3]/a[1],(a[1]-a[2]-a[3])/a[1]))
  
  
  # Colors
  bar_colors <- c("#96640F","#DCA614","#FFE072")
  bar_colors_trans <- adjustcolor(bar_colors, alpha.f = 0.8)
  box_colors <- c("#F4784A", "#63BDBD")
  
  # --- 1. Plot the Stacked Bar Chart First (Right Axis, Linear) ---
  
  par(mar = c(5, 4, 4, 4) + 0.1)
  
  xlen <- length(box_data)
  plot(1, 1, type = "n",
       xlim = c(0.5, (xlen+0.5)), ylim = c(0, 100),
       xlab = "Crops", ylab = "", xaxt = "n", yaxt = "n", main = "")
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = "white", border = NA)
  
  grid(lwd=0.2,col = "grey",lty=3)
  
  axis(1, at = 1:xlen, labels = groups)
  axis(4, las = 1)
  mtext("Percentage of Districts showing significant trend of cropping Area", side = 4, line = 2)
  
  # Draw transparent stacked bars (3 segments per group)
  for (i in 1:xlen) {
    vals <- stack_vals[[i]] * 100
    bottoms <- c(0, cumsum(vals)[-length(vals)])
    tops <- cumsum(vals)
    
    for (j in 1:3) {
      rect(i - 0.25, bottoms[j], i + 0.25, tops[j],
           col = bar_colors_trans[j],  border = "black")
    }
  }
  
  # --- 2. Overlay the Boxplots (Left Axis, Log-Scale) ---
  
  par(new = TRUE)
  
  plot(1, 1, type = "n",
       xlim = c(0.5, (xlen+0.5)), ylim = c(0.09, max(unlist(box_data))), log = "y",
       xlab = "", ylab = "Trend (ha/year) (log scale)",
       xaxt = "n", yaxt = "n", main = "")
  
  axis(2, las = 1)
  
  # Plot 2 boxplots per group
  for (i in 1:xlen) {
    boxplot(box_data[[i]]$inc,
            at = i - 0.15, add = TRUE, boxwex = 0.5,outline = F,
            col = box_colors[1], border = "black", axes = FALSE, log = "y")
    
    boxplot(box_data[[i]]$dec,
            at = i + 0.15, add = TRUE, boxwex = 0.5,outline = F,
            col = box_colors[2], border = "black", axes = FALSE, log = "y")
  }
  
dev.off()  
  
  # --- 3. Legend ---

png("legend.png",units = "in",res = 600,height = 3,width = 12)  
plot.new()
  legend("top",
         legend = c("Districts with significantly increasing trends of crop growing area",
                    "Districts with significantly decreasing trends of crop growing area",                       "Significantly increasing cropping area",
                    "Significantly decreasing cropping area", 
                    "Trend change is insignificant"),
         fill = c(box_colors, bar_colors_trans),cex=1,ncol = 2,
         border = "black", bty = "n")
dev.off()
