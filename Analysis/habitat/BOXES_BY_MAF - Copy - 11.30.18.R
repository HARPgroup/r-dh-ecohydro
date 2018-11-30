rm(list = ls())  #clear variables
library('ggplot2')
library('plyr')
library('shape') #used to plot arrows for loss threshold on plot

basepath='C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/r-dh-ecohydro/';
source(paste(basepath,'config.local.private',sep='/'));
source(paste(basepath,"Analysis/habitat/Habitat_2019/hab_functions.R", sep = "/")) #load habitat functions


#RETRIEVE HABITAT DATA
pctchg <- "20"
tenth_percentile <- "no"
##############################################################################################################
# RETRIEVE DATA
nt_hab_chg <- read.csv(paste(basepath,"Analysis/habitat/nt_&_hab_change/nt_hab_chg_all_months.csv",sep=""))

if (tenth_percentile == "yes") {
  all_sites <- read.csv(paste(basepath,"Analysis/habitat/pctchg_datasets/tenth_percentile_flow_and_below/IFIM_SITES_",pctchg,"%Reduction.csv",sep=""))
} else {
  all_sites <- read.csv(paste(basepath,"Analysis/habitat/pctchg_datasets/all_flows/IFIM_SITES_",pctchg,"%Reduction.csv",sep=""))
  # Remove Potomac Sites when doing all_flows 
  all_sites <- all_sites[-which(all_sites$ifim_site_name=="T8&9"),] 
  all_sites <- all_sites[-which(all_sites$ifim_site_name=="T11&12"),] 
}  

#----------------------------------------------------------------------------------------------
#REMOVE NON-FISH METRICS
all_sites <- rm.benth(all_sites)

months <- c("MAF","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#r<-1
for (r in 1:length(months)) {
  month <- months[r]

  month_all_sites <- month.subset(all_sites, month)
  month_all_sites_medians <- agg.median(month_all_sites)


# MAF_all_sites <- month.subset(all_sites, "MAF")
# MAF_all_sites_medians <- agg.median(MAF_all_sites)
# 
# Jan_all_sites <- month.subset(all_sites, "Jan")
# Jan_all_sites_medians <- agg.median(Jan_all_sites)
# 
# Feb_all_sites <- month.subset(all_sites, "Feb")
# Feb_all_sites_medians <- agg.median(Feb_all_sites)
# 
# Mar_all_sites <- month.subset(all_sites, "Mar")
# Mar_all_sites_medians <- agg.median(Mar_all_sites)
# 
# Apr_all_sites <- month.subset(all_sites, "Apr")
# Apr_all_sites_medians <- agg.median(Apr_all_sites)
# 
# May_all_sites <- month.subset(all_sites, "May")
# May_all_sites_medians <- agg.median(May_all_sites)
# 
# Jun_all_sites <- month.subset(all_sites, "Jun")
# Jun_all_sites_medians <- agg.median(Jun_all_sites)
# 
# Jul_all_sites <- month.subset(all_sites, "Jul")
# Jul_all_sites_medians <- agg.median(Jul_all_sites)
# 
# Aug_all_sites <- month.subset(all_sites, "Aug")
# Aug_all_sites_medians <- agg.median(Aug_all_sites)
# 
# Sep_all_sites <- month.subset(all_sites, "Sep")
# Sep_all_sites_medians <- agg.median(Sep_all_sites)
# 
# Oct_all_sites <- month.subset(all_sites, "Oct")
# Oct_all_sites_medians <- agg.median(Oct_all_sites)
# 
# Nov_all_sites <- month.subset(all_sites, "Nov")
# Nov_all_sites_medians <- agg.median(Nov_all_sites)
# 
# Dec_all_sites <- month.subset(all_sites, "Dec")
# Dec_all_sites_medians <- agg.median(Dec_all_sites)



ascending_MAF_all_sites <- MAF_all_sites[order(MAF_all_sites$ifim_da_sqmi),] 
#ascending_MAF_all_sites <- MAF_all_sites_medians[order(MAF_all_sites_medians$ifim_da_sqmi),] 
ascending_MAF_all_sites <- ascending_MAF_all_sites[!duplicated(ascending_MAF_all_sites$ifim_site_name), ]


MAF_all_sites_medians <- data.frame(ascending_MAF_all_sites,MAF_all_sites_medians)
colnames(MAF_all_sites_medians)[1] <- "ifim_site_name"

plot_labels <- data.frame(ifim_site_name=ascending_MAF_all_sites$ifim_site_name,
                          ifim_da_sqmi=ascending_MAF_all_sites$ifim_da_sqmi,
                          ifim_maf_cfs=ascending_MAF_all_sites$ifim_maf_cfs,
                          pctchg=-40)

#INITIALIZE reg_stats DATAFRAME
reg_stats <- data.frame(month=character(),
                        pctchg=character(),
                        m=character(), 
                        b=character(), 
                        y0=character(),
                        stringsAsFactors=FALSE) 

#months <- c("MAF","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#r<-1
#for (r in 1:length(months)) {
#  month <- months[r]
  if (month == "MAF"){month_data <- MAF_all_sites}
  if (month == "Jan"){month_data <- Jan_all_sites}
  if (month == "Feb"){month_data <- Feb_all_sites}
  if (month == "Mar"){month_data <- Mar_all_sites}
  if (month == "Apr"){month_data <- Apr_all_sites}
  if (month == "May"){month_data <- May_all_sites}
  if (month == "Jun"){month_data <- Jun_all_sites}
  if (month == "Jul"){month_data <- Jul_all_sites}
  if (month == "Aug"){month_data <- Aug_all_sites}
  if (month == "Sep"){month_data <- Sep_all_sites}
  if (month == "Oct"){month_data <- Oct_all_sites}
  if (month == "Nov"){month_data <- Nov_all_sites}
  if (month == "Dec"){month_data <- Dec_all_sites}
  
  #OUTPUT MONTHLY PERCENT CHANGE TABLES AS CSV
  write.csv(month_data,paste(save_directory,"\\",month,"_site_pctchg.csv",sep=""))
  
  stat <- "median"
  month_data_medians <- aggregate(list(pctchg = month_data$pctchg),
                                  list(ifim_maf_cfs = month_data$ifim_maf_cfs),
                                  stat)

  
  #RETRIEVE NT CHANGE DATA
  Dunlap_nt_chg <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "Dunlap")),]$custom_taxachg
  PlainsMill_nt_chg  <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "Plains Mill")),]$custom_taxachg
  NorthAnnaPiedmont_nt_chg  <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "North Anna Piedmont")),]$custom_taxachg
  NorthAnnaFallZone_nt_chg  <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "North Anna Fall Zone")),]$custom_taxachg
  NorthAnnaCoastalPlain_nt_chg  <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "North Anna Coastal Plain")),]$custom_taxachg
  Craig_nt_chg  <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "Craig")),]$custom_taxachg
  
  #RETRIEVE SITE DRAINAGE AREAS
  Dunlap.da <- plot_labels[which((plot_labels$ifim_site_name == "Dunlap")),]$ifim_da_sqmi
  PlainsMill.da <- plot_labels[which((plot_labels$ifim_site_name == "Plains Mill")),]$ifim_da_sqmi
  NorthAnnaPiedmont.da <- plot_labels[which((plot_labels$ifim_site_name == "North Anna Piedmont")),]$ifim_da_sqmi
  NorthAnnaFallZone.da <- plot_labels[which((plot_labels$ifim_site_name == "North Anna Fall Zone")),]$ifim_da_sqmi
  NorthAnnaCoastalPlain.da <- plot_labels[which((plot_labels$ifim_site_name == "North Anna Coastal Plain")),]$ifim_da_sqmi
  Craig.da <- plot_labels[which((plot_labels$ifim_site_name == "Craig")),]$ifim_da_sqmi
  
  #RETRIEVE SITE MAFs
  Dunlap.maf <- plot_labels[which((plot_labels$ifim_site_name == "Dunlap")),]$ifim_maf_cfs
  PlainsMill.maf <- plot_labels[which((plot_labels$ifim_site_name == "Plains Mill")),]$ifim_maf_cfs
  NorthAnnaPiedmont.maf <- plot_labels[which((plot_labels$ifim_site_name == "North Anna Piedmont")),]$ifim_maf_cfs
  NorthAnnaFallZone.maf <- plot_labels[which((plot_labels$ifim_site_name == "North Anna Fall Zone")),]$ifim_maf_cfs
  NorthAnnaCoastalPlain.maf <- plot_labels[which((plot_labels$ifim_site_name == "North Anna Coastal Plain")),]$ifim_maf_cfs
  Craig.maf <- plot_labels[which((plot_labels$ifim_site_name == "Craig")),]$ifim_maf_cfs

  #BUILD RICHNESS DATAFRAME
  richness.df <- data.frame(
    maf = c(
      Dunlap.maf,
      PlainsMill.maf,
      NorthAnnaPiedmont.maf,
      NorthAnnaFallZone.maf,
      NorthAnnaCoastalPlain.maf,
      Craig.maf
    ),
    pctchg = c(
      Dunlap_nt_chg,
      PlainsMill_nt_chg,
      NorthAnnaPiedmont_nt_chg,
      NorthAnnaFallZone_nt_chg,
      NorthAnnaCoastalPlain_nt_chg,
      Craig_nt_chg
    )
  )
  
  
  #CALCULATE HABITAT REGRESSION & CORRESPONDING PLOTTING POINTS 
  fit <- lm(month_data_medians$pctchg~log(month_data_medians$ifim_maf_cfs))
  b <- as.numeric(fit[1]$coefficients[1])
  m <- as.numeric(fit[1]$coefficients[2])
  x2 <- 5700 
  y2 <- m*log(x2)+b
  x1 <- 180
  y1 <- m*log(x1)+b
                     
  #CALCULATE RICHNESS REGRESSION & CORRESPONDING PLOTTING POINTS                     
  rich.fit <- lm(richness.df$pctchg~log(richness.df$maf))
  rich.b <- as.numeric(rich.fit[1]$coefficients[1])
  rich.m <- as.numeric(rich.fit[1]$coefficients[2])
  rich.x2 <- 450
  rich.y2 <- rich.m * log(rich.x2) + rich.b
  rich.x1 <- 180
  rich.y1 <- rich.m * log(rich.x1) + rich.b
  
  
  
  #----------------------------------------------------------------------------------------------
  # PLOT IMAGE BLOCK
  
  #DETERMINE PLOT FILE NAME
  plot_name <- paste(month,"_boxplots_",stat,"_FISH",sep="") 
  if (month == "MAF"){plot_name  <- paste("0-",plot_name,sep="")}
  if (month == "Jan"){plot_name  <- paste("1-",plot_name,sep="")}
  if (month == "Feb"){plot_name  <- paste("2-",plot_name,sep="")}
  if (month == "Mar"){plot_name  <- paste("3-",plot_name,sep="")}
  if (month == "Apr"){plot_name  <- paste("4-",plot_name,sep="")}
  if (month == "May"){plot_name  <- paste("5-",plot_name,sep="")}
  if (month == "Jun"){plot_name  <- paste("6-",plot_name,sep="")}
  if (month == "Jul"){plot_name  <- paste("7-",plot_name,sep="")}
  if (month == "Aug"){plot_name  <- paste("8-",plot_name,sep="")}
  if (month == "Sep"){plot_name  <- paste("9-",plot_name,sep="")}
  if (month == "Oct"){plot_name  <- paste("10-",plot_name,sep="")}
  if (month == "Nov"){plot_name  <- paste("11-",plot_name,sep="")}
  if (month == "Dec"){plot_name  <- paste("12-",plot_name,sep="")}
  
  #INITIALIZE PLOT AS PNG
  png(filename=paste(save_directory,"\\",plot_name,"_",pctchg,".png",sep=""),
      width = 1500, 
      height = 750)

  #ADD BOXPLOTS
    boxplot(month_data$pctchg ~ month_data$ifim_maf_cfs, 
            at = sort(unique(month_data$ifim_maf_cfs)), 
            boxwex=0.04,
            log="x",
            ylim=c(-60,60),
            xlim=c(100,12000),
            par(mar=c(8,3,1,1),
                oma=c(3,5,0,0)),
            xaxt="n",
            yaxt="n")  
  
  #ADD RED AND GREEN BACKGROUND COLORS
      rect(80,-65, 15000, 0, col= adjustcolor('tomato',alpha=0.2))
      rect(80,65, 15000, 0, col= adjustcolor('palegreen',alpha=0.2)) 

  #FORMAT X-AXIS LABELS
  axis(1, at=c(100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,20000), 
       labels=c(100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,20000),
       cex.axis=2.5, las=2)
  
  #FORMAT Y-AXIS LABELS
  axis(2, cex.axis=2.5, las=1)
  
  #ADD AXES TITLES 
  mtext(text="              Percent Change\n",side=2,line=0,outer=TRUE,cex=2.5)
  mtext(text="Mean Annual Flow (cfs)",side=1,line=0,outer=TRUE,cex=2.5)

  #ADD HORIZONTAL LINE AT Y=0
  abline(a=0,b=0)
 
  #ADD HABITAT MEDIANS AS BLACK TRIANGLES 
  points(month_data_medians$ifim_maf_cfs, month_data_medians$pctchg, col = "black",cex=3,pch=17) #pch=19 for circle point
  
  #ADD RED RICHNESS POINTS
    points(Dunlap.maf, Dunlap_nt_chg, col = "red",cex=3,pch=18) 
    points(PlainsMill.maf, PlainsMill_nt_chg, col = "red",cex=3,pch=18) 
    points(NorthAnnaPiedmont.maf, NorthAnnaPiedmont_nt_chg, col = "red",cex=3,pch=18) 
    points(NorthAnnaFallZone.maf, NorthAnnaFallZone_nt_chg, col = "red",cex=3,pch=18) 
    points(NorthAnnaCoastalPlain.maf, NorthAnnaCoastalPlain_nt_chg, col = "red",cex=3,pch=18) 
    points(Craig.maf, Craig_nt_chg, col = "red",cex=3,pch=18) 

  #ADD LEGEND TO UPPER RIGHT CORNER
  legend(4150, 63, legend=c("Median Habitat Change", "Median Habitat Change Regression", "Habitat Change boxplots", "Species Richness Change"),
      col=c("black", "black", "black", "red"), pch=c(17,45,127,18), cex=1.5, pt.cex = 2.0)    

  #FORMAT REGRESSION EQUATION TEXT
  eq <- paste("Regression Equation:\n","y = ",round(m,2)," ln(x) ",round(b,2),
              "\ny = 0 @ ",round(exp((-b)/m),1)," (sqmi)",sep="")
  text(x = 85, y = 52, labels = eq, cex=2, pos=4)

  #ADD HABITAT BREAKPOINT LINE
  abline(v=round(exp((-b)/m),1),lty=1,lwd=2)
  
  #Add Habitat Breakpoint Label
    text(x = round(exp((-b)/m),1), y = 45, labels = "    Habitat Breakpoint", pos=4, cex=1.8, font=3) 
    Arrows(round(exp((-b)/m),1)+(0.1*round(exp((-b)/m),1)),45,round(exp((-b)/m),1),45,arr.type="simple",lwd=2)

  #ADD HABITAT REGRESSSION AS BLACK DOTTED LINE  
  segments(x1, y1, x2, y2,col='black',lwd=5,lty=3)
  
  #ADD RICHNESS REGRESSSION AS RED DOTTED LINE
  segments(rich.x1, rich.y1, rich.x2, rich.y2,col='red',lwd=5,lty=3)
  
  dev.off()
  #----------------------------------------------------------------------------------------------
  
  
  
  #output regression stats as csv
  # reg_stats_r <- data.frame(month=month,
  #                           pctchg=pctchg,
  #                           m=m, 
  #                           b=b, 
  #                           y0=as.numeric(round(exp((-b)/m),5))) 
  # reg_stats <- rbind(reg_stats,reg_stats_r)
  # 
  # if (fish.only == "yes") {
  #   write.csv(reg_stats,paste(save_directory,"\\","reg_stats_",stat,"_",pctchg,"pctchg_FISH.csv",sep=""))
  # } else {
  #   write.csv(reg_stats,paste(save_directory,"\\","reg_stats_",stat,"_",pctchg,"pctchg.csv",sep=""))
  # }
  
} #end for loop