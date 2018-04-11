
basepath='C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\r-dh-ecohydro\\ELFGEN\\internal\\';
source(paste(basepath,'config.local.private',sep='/'));

stat_method <- "mean"
Above_Harvell_Dam <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Above Harvell Dam_10pct_",stat_method,".csv",sep=""),sep=""))
Below_Harvell_Dam <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Below Harvell Dam_10pct_",stat_method,".csv",sep=""),sep=""))
Craig <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Craig_10pct_",stat_method,".csv",sep=""),sep=""))
Dunlap <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Dunlap_10pct_",stat_method,".csv",sep=""),sep=""))
Front_Royal <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Front Royal_10pct_",stat_method,".csv",sep=""),sep=""))
Head_Kerr_Lake <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Head Kerr Lake_10pct_",stat_method,".csv",sep=""),sep=""))
James <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\James_10pct_",stat_method,".csv",sep=""),sep=""))
Laurel_Hill <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Laurel Hill_10pct_",stat_method,".csv",sep=""),sep=""))
Luray <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Luray_10pct_",stat_method,".csv",sep=""),sep=""))
Lynwood <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Lynwood_10pct_",stat_method,".csv",sep=""),sep=""))
Maury <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Maury_10pct_",stat_method,".csv",sep=""),sep=""))
New_River_Claytor_to_Pembroke <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\New River- Claytor to Pembroke_10pct_",stat_method,".csv",sep=""),sep=""))
New_River_Pembroke_to_Glen_Lyn <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\New River- Pembroke to Glen Lyn_10pct_",stat_method,".csv",sep=""),sep=""))
North_Anna_Coastal_Plain <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\North Anna Coastal Plain_10pct_",stat_method,".csv",sep=""),sep=""))
North_Anna_Fall_Zone <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\North Anna Fall Zone_10pct_",stat_method,".csv",sep=""),sep=""))
North_Anna_Piedmont <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\North Anna Piedmont_10pct_",stat_method,".csv",sep=""),sep=""))
Pamunkey_Coastal_Plain <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Pamunkey Coastal Plain_10pct_",stat_method,".csv",sep=""),sep=""))
Plains_Mill <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Plains Mill_10pct_",stat_method,".csv",sep=""),sep=""))
Posey_Hollow <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Posey Hollow_10pct_",stat_method,".csv",sep=""),sep=""))
Rt_648 <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Rt 648_10pct_",stat_method,".csv",sep=""),sep=""))
Spring_Hollow <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Spring Hollow_10pct_",stat_method,".csv",sep=""),sep=""))
Staunton <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\Staunton_10pct_",stat_method,".csv",sep=""),sep=""))
T8_9 <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\T8&9_10pct_",stat_method,".csv",sep=""),sep=""))
T11_12 <- read.csv(paste(save_directory,paste("\\WUA-CSV\\",stat_method,"\\T11&12_10pct_",stat_method,".csv",sep=""),sep=""))

Above_Harvell_Dam_Aug <- median((Above_Harvell_Dam[Above_Harvell_Dam$flow %in% "Aug",])$pctchg)
Below_Harvell_Dam_Aug <- median((Below_Harvell_Dam[Below_Harvell_Dam$flow %in% "Aug",])$pctchg)
Craig_Aug <- median((Craig[Craig$flow %in% "Aug",])$pctchg)
Dunlap_Aug <- median((Dunlap[Dunlap$flow %in% "Aug",])$pctchg)
Front_Royal_Aug <- median((Front_Royal[Front_Royal$flow %in% "Aug",])$pctchg)
Head_Kerr_Lake_Aug <- median((Head_Kerr_Lake[Head_Kerr_Lake$flow %in% "Aug",])$pctchg)
James_Aug <- median((James[James$flow %in% "Aug",])$pctchg)
Laurel_Hill_Aug <- median((Laurel_Hill[Laurel_Hill$flow %in% "Aug",])$pctchg)
Luray_Aug <- median((Luray[Luray$flow %in% "Aug",])$pctchg)
Lynwood_Aug <- median((Lynwood[Lynwood$flow %in% "Aug",])$pctchg)
Maury_Aug <- median((Maury[Maury$flow %in% "Aug",])$pctchg)
New_River_Claytor_to_Pembroke_Aug <- median((New_River_Claytor_to_Pembroke[New_River_Claytor_to_Pembroke$flow %in% "Aug",])$pctchg)
New_River_Pembroke_to_Glen_Lyn_Aug <- median((New_River_Pembroke_to_Glen_Lyn[New_River_Pembroke_to_Glen_Lyn$flow %in% "Aug",])$pctchg)
North_Anna_Coastal_Plain_Aug <- median((North_Anna_Coastal_Plain[North_Anna_Coastal_Plain$flow %in% "Aug",])$pctchg)
North_Anna_Fall_Zone_Aug <- median((North_Anna_Fall_Zone[North_Anna_Fall_Zone$flow %in% "Aug",])$pctchg)
North_Anna_Piedmont_Aug <- median((North_Anna_Piedmont[North_Anna_Piedmont$flow %in% "Aug",])$pctchg)
Pamunkey_Coastal_Plain_Aug <- median((Pamunkey_Coastal_Plain[Pamunkey_Coastal_Plain$flow %in% "Aug",])$pctchg)
Plains_Mill_Aug <- median((Plains_Mill[Plains_Mill$flow %in% "Aug",])$pctchg)
Posey_Hollow_Aug <- median((Posey_Hollow[Posey_Hollow$flow %in% "Aug",])$pctchg)
Rt_648_Aug <- median((Rt_648[Rt_648$flow %in% "Aug",])$pctchg)
Spring_Hollow_Aug <- median((Spring_Hollow[Spring_Hollow$flow %in% "Aug",])$pctchg)
Staunton_Aug <- median((Staunton[Staunton$flow %in% "Aug",])$pctchg)
T8_9_Aug <- median((T8_9[T8_9$flow %in% "Aug",])$pctchg)
T11_12_Aug <- median((T11_12[T11_12$flow %in% "Aug",])$pctchg)


all_sites <- rbind(Above_Harvell_Dam_Aug,
               Below_Harvell_Dam_Aug,
               Craig_Aug,
               Dunlap_Aug,
               Front_Royal_Aug,
               Head_Kerr_Lake_Aug,
               James_Aug,
               Laurel_Hill_Aug,
               Luray_Aug,
               Lynwood_Aug,
               Maury_Aug,
               New_River_Claytor_to_Pembroke_Aug,
               New_River_Pembroke_to_Glen_Lyn_Aug,
               North_Anna_Coastal_Plain_Aug,
               North_Anna_Fall_Zone_Aug,
               North_Anna_Piedmont_Aug,
               Pamunkey_Coastal_Plain_Aug,
               Plains_Mill_Aug,
               Posey_Hollow_Aug,
               Rt_648_Aug,
               Spring_Hollow_Aug,
               Staunton_Aug,
               T8_9_Aug,
               T11_12_Aug
               )


aug_all_sites <- all_sites[all_sites$flow %in% "Aug",]


ggplot(aug_all_sites, aes(ifim_da_sqmi,pctchg))+
  geom_boxplot(fill='#A4A4A4', color="darkred")+
  #geom_text(aes(label=metric),hjust=0, vjust=0)+
  geom_hline(yintercept=0,col='#A4A4A4')+
  #labs(title = paste("Percent Habitat Change with 10% Flow Reduction (",stat_method," monthly)",sep=""),
  #     subtitle = paste(ifim_site_name,":\nDrainage Area: ",ifim_da_sqmi," sqmi\nUSGS: ",gage," (",start_date," to ",end_date,")",sep=""))+
  
  xlab("Flow (cfs)")+ 
  ylab("Percent Habitat Change")#+
  #scale_x_discrete(limit = c("MAF",month.abb))
#scale_y_continuous(limits = c(-10, 100))

filename <- paste(ifim_site_name,"10pct",stat_method,"boxplot.png", sep="_")
ggsave(file=filename, path = save_directory, width=14, height=8)

#--------------------------------------------------------------------------------------------------

