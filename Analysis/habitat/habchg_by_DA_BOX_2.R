library('ggplot2')
library('plyr')

basepath='C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\r-dh-ecohydro\\ELFGEN\\internal\\';
source(paste(basepath,'config.local.private',sep='/'));

#specify either "median" or "mean" of august daily percent habitat change
stat_method <- "median"


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

aug_all_sites <- all_sites[all_sites$flow %in% "Aug",]
aug_all_sites <- na.omit(aug_all_sites) #remove NA rows prior to calculating medians


#----------------------------------------------------------------------------------------------
aug_all_sites_medians <- aggregate(list(pctchg = aug_all_sites[,6]), list(ifim_da_sqmi = aug_all_sites$ifim_da_sqmi), median)
aug_all_sites_min<- aggregate(list(pctchg = aug_all_sites[,6]), list(ifim_da_sqmi = aug_all_sites$ifim_da_sqmi), min)
aug_all_sites_25 <- ddply(aug_all_sites, "ifim_da_sqmi", summarise, pctchg = quantile(pctchg, .25))
aug_all_sites_75 <- ddply(aug_all_sites, "ifim_da_sqmi", summarise, pctchg = quantile(pctchg, .75))
aug_all_sites_max <- aggregate(list(pctchg = aug_all_sites[,6]), list(ifim_da_sqmi = aug_all_sites$ifim_da_sqmi), max)


ascending_aug_all_sites <- aug_all_sites[order(aug_all_sites$ifim_da_sqmi),] 
ascending_aug_all_sites <- ascending_aug_all_sites[!duplicated(ascending_aug_all_sites$ifim_site_name), ]
ascending_aug_all_sites <- ascending_aug_all_sites [,-6]
ascending_aug_all_sites <- ascending_aug_all_sites [,-5]
ascending_aug_all_sites <- ascending_aug_all_sites [,-4]
ascending_aug_all_sites <- ascending_aug_all_sites [,-3]
ascending_aug_all_sites <- ascending_aug_all_sites [,-1]

aug_all_sites_medians <- data.frame(ascending_aug_all_sites,aug_all_sites_medians)
colnames(aug_all_sites_medians)[1] <- "ifim_site_name"

ggplot(aug_all_sites_medians, aes(ifim_da_sqmi,pctchg))+
  geom_point(size = 2,color = "blue4")+
  geom_point(data = aug_all_sites_min,aes(ifim_da_sqmi,pctchg),size = 2,color = "chartreuse4")+
  geom_point(data = aug_all_sites_25,aes(ifim_da_sqmi,pctchg),size = 2,color = "chocolate3")+
  geom_point(data = aug_all_sites_75,aes(ifim_da_sqmi,pctchg),size = 2,color = "chocolate3")+
  geom_point(data = aug_all_sites_max,aes(ifim_da_sqmi,pctchg),size = 2,color = "chartreuse4")+
  #geom_smooth()+
  #geom_line(method='lm',formula=y~log(x))

  geom_smooth(data=aug_all_sites_medians, method=lm,se=FALSE,color = "blue4")+
  geom_smooth(data=aug_all_sites_min, method=lm,se=FALSE,color = "chartreuse4")+
  geom_smooth(data=aug_all_sites_25, method=lm,se=FALSE,color = "chocolate3")+
  geom_smooth(data=aug_all_sites_75, method=lm,se=FALSE,color = "chocolate3")+
  geom_smooth(data=aug_all_sites_max, method=lm,se=FALSE,color = "chartreuse4")+
  
  xlab("Drainage Area (mi^2)")+ 
  ylab("Percent Habitat Change (August)")+
  geom_hline(yintercept=0,col='#A4A4A4')+
  labs(title = paste("Percent Habitat Change with 10% Flow Reduction (August ",stat_method,")",sep=""),
       subtitle = paste("",sep=""))+
  scale_y_continuous(limits = c(-40, 40))+
  #geom_text(aes(label=aug_all_sites_medians$ifim_site_name),hjust=0, vjust=0,size = 3)+#,angle = 15)+
  scale_x_continuous(trans = "log",breaks = c(0,10,100,1000,10000), labels =c(0,10,100,1000,10000))


  
filename <- paste("aug_median_10pct_mo-",stat_method,".png", sep="_")
ggsave(file=filename, path = save_directory, width=10, height=8)


