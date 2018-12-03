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
#REMOVE NON-BENTHIC MACROINVERTEBRATE METRICS
all_sites <- macroinverts(all_sites)

months <- c("MAF","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#r<-9
for (r in 1:length(months)) {
  month <- months[r]

  month_all_sites <- month.subset(all_sites, month)
  month_all_sites_medians <- agg.median(month_all_sites)
  site_stats <- site.stats(month_all_sites)
  month_data <- month_all_sites
  
#INITIALIZE reg_stats DATAFRAME
reg_stats <- data.frame(month=character(),
                        pctchg=character(),
                        m=character(), 
                        b=character(), 
                        y0=character(),
                        stringsAsFactors=FALSE) 

  #OUTPUT MONTHLY PERCENT CHANGE TABLES AS CSV
  #write.csv(month_data,paste(save_directory,"\\",month,"_site_pctchg.csv",sep=""))
  
  #DETERMINE MEDIAN VALUE AT EACH HABITAT SITE, FOR PLOTTING POINTS AND CALCULATING REGRESSION
  stat <- "median"
  month_data_medians <- aggregate(list(pctchg = month_data$pctchg),
                                  #####list(ifim_maf_cfs = month_data$ifim_maf_cfs),
                                  list(ifim_da_sqmi = month_data$ifim_da_sqmi),
                                  stat)

  #RETRIEVE NT CHANGE DATA
  # Dunlap_nt_chg <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "Dunlap")),]$custom_taxachg
  # PlainsMill_nt_chg  <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "Plains Mill")),]$custom_taxachg
  # NorthAnnaPiedmont_nt_chg  <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "North Anna Piedmont")),]$custom_taxachg
  # NorthAnnaFallZone_nt_chg  <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "North Anna Fall Zone")),]$custom_taxachg
  # NorthAnnaCoastalPlain_nt_chg  <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "North Anna Coastal Plain")),]$custom_taxachg
  # Craig_nt_chg  <- nt_hab_chg[which((nt_hab_chg$pctchg == pctchg) & (nt_hab_chg$month == month) & (nt_hab_chg$ifim_site == "Craig")),]$custom_taxachg

  #CALCULATE HABITAT REGRESSION & CORRESPONDING PLOTTING POINTS 
  #####fit <- lm(month_data_medians$pctchg~log(month_data_medians$ifim_maf_cfs))
  fit <- lm(month_data_medians$pctchg~log(month_data_medians$ifim_da_sqmi))
  b <- as.numeric(fit[1]$coefficients[1])
  m <- as.numeric(fit[1]$coefficients[2])
  #x2 <- 4100 
  x2 <- 12000 
  y2 <- m*log(x2)+b
  #x1 <- 180
  x1 <- 100
  y1 <- m*log(x1)+b
  
  #USE TO END REGRESSION RIGHT AT HABITAT BREAKPOINT
  #x1 <- round(exp((-b)/m),1)
  #y1 <- m*log(x1)+b
  #----------------------------------------------------------------------------------------------
  # PLOT IMAGE BLOCK

  #SET PLOT FILE NAME
  plot_name <- paste((r-1),"-",month,"_boxplots_",stat,"_macroinverts",sep="") 
  
  #INITIALIZE PLOT AS PNG
  png(filename=paste(save_directory,"\\",plot_name,"_",pctchg,".png",sep=""),
      width = 1500, 
      height = 750)

  #ADD BOXPLOTS
    boxplot(#####month_data$pctchg ~ month_data$ifim_maf_cfs, 
            #####at = sort(unique(month_data$ifim_maf_cfs)), 
            month_data$pctchg ~ month_data$ifim_da_sqmi, 
            at = sort(unique(month_data$ifim_da_sqmi)), 
            boxwex=0.04,
            log="x",
            ylim=c(-250,250),
            xlim=c(100,12000),
            par(mar=c(8,3,1,1),
                oma=c(3,5,0,0)),
            xaxt="n",
            yaxt="n")  
  
  #ADD RED AND GREEN BACKGROUND COLORS
      rect(80,-275, 15000, 0, col= adjustcolor('tomato',alpha=0.2))
      rect(80,275, 15000, 0, col= adjustcolor('palegreen',alpha=0.2)) 

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
  #####points(month_data_medians$ifim_maf_cfs, month_data_medians$pctchg, col = "black",cex=3,pch=17) #pch=19 for circle point
  points(month_data_medians$ifim_da_sqmi, month_data_medians$pctchg, col = "black",cex=3,pch=17) #pch=19 for circle point
  
  
  #ADD LEGEND TO UPPER RIGHT CORNER
  legend(4150, 262, legend=c("Median Habitat Change", "Median Habitat Change Regression", "Habitat Change boxplots"),
      col=c("black", "black", "black"), pch=c(17,45,127), cex=1.5, pt.cex = 2.0)    

  #FORMAT HABITAT REGRESSION EQUATION TEXT
  eq <- paste("Regression Equation:\n","y = ",round(m,2)," ln(x) ",round(b,2),
              "\ny = 0 @ ",round(exp((-b)/m),1)," (cfs)",sep="")
  text(x = 85, y = 215, labels = eq, cex=2, pos=4)

  #ADD HABITAT BREAKPOINT LINE
  abline(v=round(exp((-b)/m),1),lty=1,lwd=2)
  
  #Add Habitat Breakpoint Label
    text(x = round(exp((-b)/m),1), y = 100, labels = "    Habitat Breakpoint", pos=4, cex=1.8, font=3) 
    Arrows(round(exp((-b)/m),1)+(0.1*round(exp((-b)/m),1)),100,round(exp((-b)/m),1),100,arr.type="simple",lwd=2)

  #ADD HABITAT REGRESSSION AS BLACK DOTTED LINE  
  segments(x1, y1, x2, y2,col='black',lwd=5,lty=3)

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