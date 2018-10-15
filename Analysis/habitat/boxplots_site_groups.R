library('ggplot2')
library('plyr')
library('shape') #used to plot arrows for loss threshold on plot

basepath='C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/r-dh-ecohydro/';
source(paste(basepath,'config.local.private',sep='/'));

#RETRIEVE HABITAT DATA
pctchg <- "10"
tenth_percentile <- "no"
fish.only <- "yes" #"yes" to plot for fish habitat metrics only 
benth.only <- "no" 
single_metric <- "smb_adult" #"no" to plot for all metrics, otherwise specify metric of interest smb_adult, nh_adult etc.

group <- "none" #"above_500", "below_500", "none"

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

#-----------------------------------------------------------------------------------------------------
# REMOVE NON-FISH METRICS
#-----------------------------------------------------------------------------------------------------

if (fish.only == "yes") {
  all_sites <- all_sites[-which(all_sites$metric=="alg_mid"),]        # Algae and Midge Guild
  all_sites <- all_sites[-which(all_sites$metric=="bd_high_grad"),]   # Benthos Diversity, high gradient
  all_sites <- all_sites[-which(all_sites$metric=="bd_low_grad"),]    # Benthos Diversity, low gradient
  all_sites <- all_sites[-which(all_sites$metric=="benth_mac"),]      # Benthic Macroinvertebrates
  all_sites <- all_sites[-which(all_sites$metric=="cf"),]             # Crayfish
  all_sites <- all_sites[-which(all_sites$metric=="e_comp"),]         # Eastern Elliptio mussel, Elliptio complanata
  all_sites <- all_sites[-which(all_sites$metric=="eph_mac"),]        # Mayfly
  all_sites <- all_sites[-which(all_sites$metric=="l_rad"),]          # Eastern lampmussel
  all_sites <- all_sites[-which(all_sites$metric=="plec_mac"),]       # Stonefly
  all_sites <- all_sites[-which(all_sites$metric=="pwb"),]            # Purple Wartyback mussel
  all_sites <- all_sites[-which(all_sites$metric=="sm_shal_slow"),]   # Spike Mussel, Shallow
  all_sites <- all_sites[-which(all_sites$metric=="sm_int"),]         # Spike Mussel, Intermediate
  all_sites <- all_sites[-which(all_sites$metric=="tric_mac"),]       # Caddisfly
}

if (benth.only == "yes") {
  all_sites1 <- all_sites[which(all_sites$metric=="alg_mid"),]        # Algae and Midge Guild
  all_sites2 <- all_sites[which(all_sites$metric=="bd_high_grad"),]   # Benthos Diversity, high gradient
  all_sites3 <- all_sites[which(all_sites$metric=="bd_low_grad"),]    # Benthos Diversity, low gradient
  all_sites4 <- all_sites[which(all_sites$metric=="benth_mac"),]      # Benthic Macroinvertebrates
  all_sites5 <- all_sites[which(all_sites$metric=="cf"),]             # Crayfish
  all_sites6 <- all_sites[which(all_sites$metric=="e_comp"),]         # Eastern Elliptio mussel, Elliptio complanata
  all_sites7 <- all_sites[which(all_sites$metric=="eph_mac"),]        # Mayfly
  all_sites8 <- all_sites[which(all_sites$metric=="l_rad"),]          # Eastern lampmussel
  all_sites9 <- all_sites[which(all_sites$metric=="plec_mac"),]       # Stonefly
  all_sites10 <- all_sites[which(all_sites$metric=="pwb"),]            # Purple Wartyback mussel
  all_sites11 <- all_sites[which(all_sites$metric=="sm_shal_slow"),]   # Spike Mussel, Shallow
  all_sites12 <- all_sites[which(all_sites$metric=="sm_int"),]         # Spike Mussel, Intermediate
  all_sites13 <- all_sites[which(all_sites$metric=="tric_mac"),]       # Caddisfly
  
  
  mussel_sites <- rbind(
    all_sites6,
    all_sites8,
    all_sites10,
    all_sites11,
    all_sites12)
  
  inverts_sites <- rbind(
    all_sites2,
    all_sites3,
    all_sites4,
    all_sites5,
    all_sites7,
    all_sites9,
    all_sites13) 
  
  algae_sites <- rbind(all_sites1)
  
  all_sites <-  algae_sites         
}

#-----------------------------------------------------------------------------------------------------

if (group != "none") {
    if (group == "above_500") {
      all_sites <- all_sites[-which(all_sites$ifim_da_sqmi<500),] 
    } else if (group == "below_500") {
      all_sites <- all_sites[-which(all_sites$ifim_da_sqmi>500),] 
    }
}
  
  
##############################################################################################################
##############################################################################################################

# box_table_i <- data.frame(metric = targets,
#                         flow = names(dfList[L]),
#                         pctchg = as.numeric(col_all[1,]),
#                         stringsAsFactors=FALSE)
# 
# box_table <- rbind(box_table, box_table_i)
# 
# 
# 
# box_table <- box_table [-1,] #remove empty first row
# 
# #convert table values to numeric before plotting 
#   box_table[,3] <- as.numeric(as.character(box_table[,3]))

ggplot(all_sites, aes(flow,pctchg))+

  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf, fill = "palegreen", alpha = 0.2) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, fill = "tomato", alpha = 0.2) +  
  
  geom_boxplot(fill='#A4A4A4', color="darkred")+
  #geom_text(aes(label=metric),hjust=0, vjust=0)+
  geom_hline(yintercept=0,col='#A4A4A4')+
#  labs(title = paste("Percent Habitat Change with ",pctchg,"% Flow Reduction (",stat_method," monthly)",sep=""),
#       subtitle = paste(ifim_site_name,":\nDrainage Area: ",ifim_da_sqmi," sqmi\nUSGS: ",gage," (",start_date," to ",end_date,")",sep=""))+
  

  xlab("Month")+ 
  ylab("Percent Change")+
  scale_x_discrete(limit = c("MAF",month.abb))+
  ylim(-40,40)+
  theme(text = element_text(size=20))
  #scale_y_continuous(limits = c(-40, 40))


filename <- paste(group,pctchg,"pct","boxplot.png", sep="_")
ggsave(file=filename, path = save_directory, width=14, height=8)


#-------------------------------------------------------------------------------------------------
