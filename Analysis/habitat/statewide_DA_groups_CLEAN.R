rm(list = ls())  #clear variables
options(timeout=240); # set timeout to twice default level to avoid abort due to high traffic

#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
 
#----FOR RUNNING LOCALLY:
#basepath='/var/www/R';
basepath='C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/r-dh-ecohydro/';
source(paste(basepath,'config.local.private',sep='/'));

#Load Functions               
source(paste(fxn_locations,"elf_retrieve_data.R", sep = ""));  #loads function used to retrieve F:E data from VAHydro
source(paste(habitat_files,'hab_ts_functions.R',sep='/')) #loads habtat timeseries functions
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/"));       #loads file containing function that retrieves REST token
rest_uname = FALSE;
rest_pw = FALSE;
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token(site, token, rest_uname, rest_pw);


#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
# READ IN DATA
stat_method <- "median"
#pctchg <- 20
#statewide <- read.csv(paste(save_directory,paste("\\wua-groupings\\","statewide","\\",pctchg,"%\\statewide_",stat_method,"_",pctchg,"pct.csv",sep=""),sep=""))

#statewide <- statewide[,-1]
#statewide <- statewide[,-1]

pctchg <- "10"
statewide <- read.csv(paste(basepath,"Analysis/habitat/pctchg_datasets/all_flows/IFIM_SITES_",pctchg,"%Reduction.csv",sep=""))
#statewide <- statewide[,-1]
#statewide <- statewide[,-1]


#groupings <- read.table(paste("C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\r-dh-ecohydro\\Analysis\\habitat\\kh_groupings.txt",sep=""), sep = '\t', header = TRUE)
#join <- merge(statewide, groupings)
#join <- left_join(statewide, groupings, by = c("ifim_site_name" = "site", "metric" = "metric"))

#group <- "constant_high"
#group <- "constant_low"
#group <- "seasonal_spring"
group <- "DA_BELOW_486"
#group <- "DA_ABOVE_486"

#box_table <- join[which(join$group == group),]

box_table <- statewide[which(statewide$ifim_da_sqmi < 500),]
#box_table <- statewide[which(statewide$ifim_da_sqmi > 500),]


fish.only <- "yes" #"yes" to plot for fish habitat metrics only 
#-----------------------------------------------------------------------------------------------------
# REMOVE NON-FISH METRICS
#-----------------------------------------------------------------------------------------------------
if (fish.only == "yes") {
  if (length(which(box_table$metric=="alg_mid"))>0){box_table <- box_table[-which(box_table$metric=="alg_mid"),]}        # Algae and Midge Guild
  if (length(which(box_table$metric=="bd_high_grad"))>0){box_table <- box_table[-which(box_table$metric=="bd_high_grad"),]}   # Benthos Diversity, high gradient
  if (length(which(box_table$metric=="bd_low_grad"))>0){box_table <- box_table[-which(box_table$metric=="bd_low_grad"),]}    # Benthos Diversity, low gradient
  if (length(which(box_table$metric=="benth_mac"))>0){ box_table <- box_table[-which(box_table$metric=="benth_mac"),]}      # Benthic Macroinvertebrates
  if (length(which(box_table$metric=="cf"))>0){box_table <- box_table[-which(box_table$metric=="cf"),]}             # Crayfish
  if (length(which(box_table$metric=="e_comp"))>0){box_table <- box_table[-which(box_table$metric=="e_comp"),]}         # Eastern Elliptio mussel, Elliptio complanata
  if (length(which(box_table$metric=="eph_mac"))>0){box_table <- box_table[-which(box_table$metric=="eph_mac"),]}        # Mayfly
  if (length(which(box_table$metric=="l_rad"))>0){box_table <- box_table[-which(box_table$metric=="l_rad"),]}          # Eastern lampmussel
  if (length(which(box_table$metric=="plec_mac"))>0){box_table <- box_table[-which(box_table$metric=="plec_mac"),]}       # Stonefly
  if (length(which(box_table$metric=="pwb"))>0){box_table <- box_table[-which(box_table$metric=="pwb"),]}            # Purple Wartyback mussel
  if (length(which(box_table$metric=="sm_shal_slow"))>0){box_table <- box_table[-which(box_table$metric=="sm_shal_slow"),]}   # Spike Mussel, Shallow
  if (length(which(box_table$metric=="sm_int"))>0){box_table <- box_table[-which(box_table$metric=="sm_int"),]}         # Spike Mussel, Intermediate
  if (length(which(box_table$metric=="tric_mac"))>0){box_table <- box_table[-which(box_table$metric=="tric_mac"),]}       # Caddisfly
}
#-------------------------------------------------------------------------------------------------

which_sites <- group
  
ggplot(box_table, aes(flow,pctchg))+
  geom_boxplot(fill='#A4A4A4', color="darkred")+
  #geom_text(aes(label=metric),hjust=0, vjust=0)+
  geom_hline(yintercept=0,col='#A4A4A4')+
#  labs(title = paste("Percent Habitat Change with 10% Flow Reduction (",stat_method," monthly)",sep=""),
#       subtitle = paste(ifim_site_name,":\nDrainage Area: ",ifim_da_sqmi," sqmi\nUSGS: ",gage," (",start_date," to ",end_date,")",sep=""))+
  #labs(title = paste("Percent Habitat Change with ",pctchg,"% Flow Reduction (",stat_method," monthly)",sep=""),
  #     subtitle = paste(which_sites,"\n",sep=""))+
  scale_y_continuous(limits = c(-38, 38))+
  xlab("\nMonth")+ 
  ylab("Percent Habitat Change\n")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))+
  scale_x_discrete(limit = c("MAF",month.abb))
#scale_y_continuous(limits = c(-10, 100))

#output_dir <- save_directory
#filename <- paste("statewide",pctchg,"pct",stat_method,"boxplot.png", sep="_")
filename <- paste(which_sites,pctchg,"pct",stat_method,"boxplot.png", sep="_")
ggsave(file=filename, path = save_directory, width=14, height=8)

#-------------------------------------------------------------------------------------------------
