library('ggplot2')
library('plyr')

basepath='C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/r-dh-ecohydro/';
source(paste(basepath,'config.local.private',sep='/'));

pctchg <- "50"

stat_method <- "median\\median_50%"
stat_method2 <- "median_tenth_percentile_flow_boxplot"
#stat_method2 <- "median_tenth_percentile_flow_boxplot"


Above_Harvell_Dam <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Above Harvell Dam_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Below_Harvell_Dam <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Below Harvell Dam_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Craig <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Craig_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Dunlap <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Dunlap_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Front_Royal <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Front Royal_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Head_Kerr_Lake <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Head Kerr Lake_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
James <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\James_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Laurel_Hill <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Laurel Hill_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Luray <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Luray_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Lynwood <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Lynwood_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Maury <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Maury_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
New_River_Claytor_to_Pembroke <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\New River- Claytor to Pembroke_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
New_River_Pembroke_to_Glen_Lyn <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\New River- Pembroke to Glen Lyn_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
North_Anna_Coastal_Plain <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\North Anna Coastal Plain_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
North_Anna_Fall_Zone <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\North Anna Fall Zone_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
North_Anna_Piedmont <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\North Anna Piedmont_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Pamunkey_Coastal_Plain <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Pamunkey Coastal Plain_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Plains_Mill <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Plains Mill_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Posey_Hollow <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Posey Hollow_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Rt_648 <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Rt 648_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Spring_Hollow <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Spring Hollow_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
Staunton <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Staunton_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
T8_9 <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\T8&9_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))
T11_12 <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\T11&12_",pctchg,"pct_",stat_method2,".csv",sep=""),sep=""))


all_sites <- rbind(Above_Harvell_Dam,
               Below_Harvell_Dam,
               Craig,
               Dunlap,
               Front_Royal,
               Head_Kerr_Lake,
               James,
               Laurel_Hill,
               Luray,
               Lynwood,
               Maury,
               New_River_Claytor_to_Pembroke,
               New_River_Pembroke_to_Glen_Lyn,
               North_Anna_Coastal_Plain,
               North_Anna_Fall_Zone,
               North_Anna_Piedmont,
               Pamunkey_Coastal_Plain,
               Plains_Mill,
               Posey_Hollow,
               Rt_648,
               Spring_Hollow,
               Staunton,
               T8_9,
               T11_12
               )
			   
#-----------------------------------------------------------------------------
# REMOVE BOAT FROM ALL FLOWS CSVS
#pctchg <- "50"
#all_sites <- read.csv(paste(basepath,"Analysis/Habitat/IFIM_SITES_",pctchg,"%Reduction.csv",sep=""))
#
#  if (length(which(all_sites$metric=="boat")) != 0 ){
#    print("Removing boat")
#    all_sites <- all_sites[-which(all_sites$metric=="boat"),]
#  } 
#-----------------------------------------------------------------------------

write.csv(all_sites, file = paste(save_directory,"\\IFIM_SITES_",pctchg,"%Reduction.csv",sep=""))
#-----------------------------------------------------------------------------