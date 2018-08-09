library('ggplot2')
library('plyr')

basepath='C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/r-dh-ecohydro/';
source(paste(basepath,'config.local.private',sep='/'));

#RETRIEVE HABITAT DATA
pctchg <- "50"

all_sites <- read.csv(paste(basepath,"Analysis/habitat/pctchg_datasets/all_flows/IFIM_SITES_",pctchg,"%Reduction.csv",sep=""))
#all_sites <- read.csv(paste(basepath,"Analysis/habitat/pctchg_datasets/tenth_percentile_flow_and_below/IFIM_SITES_",pctchg,"%Reduction.csv",sep=""))



# Remove Potomac Sites when doing all_flows 
all_sites <- all_sites[-which(all_sites$ifim_site_name=="T8&9"),] 
all_sites <- all_sites[-which(all_sites$ifim_site_name=="T11&12"),] 

fish.only <- "yes" #"yes" to plot for fish habitat metrics only 


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
#-----------------------------------------------------------------------------------------------------
#
#-----------------------------------------------------------------------------------------------------
# Single metric tests
#all_sites <- all_sites[which(all_sites$metric=="smb_adult"),]  
#all_sites <- all_sites[which(all_sites$metric=="nh_adult"),]    
#all_sites <- all_sites[which(all_sites$metric=="alg_mid"),]   


site_subset <- data.frame(ifim_site_name = character(),
                          ifim_da_sqmi = character(), 
                          metric = character(), 
                          flow = character(), 
                          pctchg = character(), 
                          stringsAsFactors=FALSE) 


 site_subset <- rbind(site_subset, all_sites[which(all_sites$ifim_site_name=="Dunlap"),])       
 site_subset <- rbind(site_subset, all_sites[which(all_sites$ifim_site_name=="Craig"),])  
 site_subset <- rbind(site_subset, all_sites[which(all_sites$ifim_site_name=="Plains Mill"),])    
 site_subset <- rbind(site_subset, all_sites[which(all_sites$ifim_site_name=="North Anna Piedmont"),])      
 site_subset <- rbind(site_subset, all_sites[which(all_sites$ifim_site_name=="North Anna Fall Zone"),])      


 all_sites <- site_subset
#-----------------------------------------------------------------------------------------------------

MAF_all_sites <- all_sites[all_sites$flow %in% "MAF",]
MAF_all_sites <- na.omit(MAF_all_sites) #remove NA rows prior to calculating medians
MAF_all_sites_medians <- aggregate(list(pctchg = MAF_all_sites$pctchg), list(ifim_da_sqmi = MAF_all_sites$ifim_da_sqmi), median)

Jan_all_sites <- all_sites[all_sites$flow %in% "Jan",]
Jan_all_sites <- na.omit(Jan_all_sites) #remove NA rows prior to calculating medians
Jan_all_sites_medians <- aggregate(list(pctchg = Jan_all_sites$pctchg), list(ifim_da_sqmi = Jan_all_sites$ifim_da_sqmi), median)

Feb_all_sites <- all_sites[all_sites$flow %in% "Feb",]
Feb_all_sites <- na.omit(Feb_all_sites) #remove NA rows prior to calculating medians
Feb_all_sites_medians <- aggregate(list(pctchg = Feb_all_sites$pctchg), list(ifim_da_sqmi = Feb_all_sites$ifim_da_sqmi), median)

Mar_all_sites <- all_sites[all_sites$flow %in% "Mar",]
Mar_all_sites <- na.omit(Mar_all_sites) #remove NA rows prior to calculating medians
Mar_all_sites_medians <- aggregate(list(pctchg = Mar_all_sites$pctchg), list(ifim_da_sqmi = Mar_all_sites$ifim_da_sqmi), median)

Apr_all_sites <- all_sites[all_sites$flow %in% "Apr",]
Apr_all_sites <- na.omit(Apr_all_sites) #remove NA rows prior to calculating medians
Apr_all_sites_medians <- aggregate(list(pctchg = Apr_all_sites$pctchg), list(ifim_da_sqmi = Apr_all_sites$ifim_da_sqmi), median)

May_all_sites <- all_sites[all_sites$flow %in% "May",]
May_all_sites <- na.omit(May_all_sites) #remove NA rows prior to calculating medians
May_all_sites_medians <- aggregate(list(pctchg = May_all_sites$pctchg), list(ifim_da_sqmi = May_all_sites$ifim_da_sqmi), median)

Jun_all_sites <- all_sites[all_sites$flow %in% "Jun",]
Jun_all_sites <- na.omit(Jun_all_sites) #remove NA rows prior to calculating medians
Jun_all_sites_medians <- aggregate(list(pctchg = Jun_all_sites$pctchg), list(ifim_da_sqmi = Jun_all_sites$ifim_da_sqmi), median)

Jul_all_sites <- all_sites[all_sites$flow %in% "Jul",]
Jul_all_sites <- na.omit(Jul_all_sites) #remove NA rows prior to calculating medians
Jul_all_sites_medians <- aggregate(list(pctchg = Jul_all_sites$pctchg), list(ifim_da_sqmi = Jul_all_sites$ifim_da_sqmi), median)

Aug_all_sites <- all_sites[all_sites$flow %in% "Aug",]
Aug_all_sites <- na.omit(Aug_all_sites) #remove NA rows prior to calculating medians
Aug_all_sites_medians <- aggregate(list(pctchg = Aug_all_sites$pctchg), list(ifim_da_sqmi = Aug_all_sites$ifim_da_sqmi), median)

Sep_all_sites <- all_sites[all_sites$flow %in% "Sep",]
Sep_all_sites <- na.omit(Sep_all_sites) #remove NA rows prior to calculating medians
Sep_all_sites_medians <- aggregate(list(pctchg = Sep_all_sites$pctchg), list(ifim_da_sqmi = Sep_all_sites$ifim_da_sqmi), median)

Oct_all_sites <- all_sites[all_sites$flow %in% "Oct",]
Oct_all_sites <- na.omit(Oct_all_sites) #remove NA rows prior to calculating medians
Oct_all_sites_medians <- aggregate(list(pctchg = Oct_all_sites$pctchg), list(ifim_da_sqmi = Oct_all_sites$ifim_da_sqmi), median)

Nov_all_sites <- all_sites[all_sites$flow %in% "Nov",]
Nov_all_sites <- na.omit(Nov_all_sites) #remove NA rows prior to calculating medians
Nov_all_sites_medians <- aggregate(list(pctchg = Nov_all_sites$pctchg), list(ifim_da_sqmi = Nov_all_sites$ifim_da_sqmi), median)

Dec_all_sites <- all_sites[all_sites$flow %in% "Dec",]
Dec_all_sites <- na.omit(Dec_all_sites) #remove NA rows prior to calculating medians
Dec_all_sites_medians <- aggregate(list(pctchg = Dec_all_sites$pctchg), list(ifim_da_sqmi = Dec_all_sites$ifim_da_sqmi), median)


ascending_MAF_all_sites <- MAF_all_sites[order(MAF_all_sites$ifim_da_sqmi),] 
ascending_MAF_all_sites <- ascending_MAF_all_sites[!duplicated(ascending_MAF_all_sites$ifim_site_name), ]
#ascending_MAF_all_sites <- ascending_MAF_all_sites [,-6]
#ascending_MAF_all_sites <- ascending_MAF_all_sites [,-5]
#ascending_MAF_all_sites <- ascending_MAF_all_sites [,-4]
#ascending_MAF_all_sites <- ascending_MAF_all_sites [,-3]
#ascending_MAF_all_sites <- ascending_MAF_all_sites [,-2]

MAF_all_sites_medians <- data.frame(ascending_MAF_all_sites,MAF_all_sites_medians)
colnames(MAF_all_sites_medians)[1] <- "ifim_site_name"

#REMOVE PLAINS MILL FROM LABELS BECAUSE THERES NO DATA AT 10% AND BELOW FOR PLAINS MILL IN AUGUST
#MAF_all_sites_medians <- MAF_all_sites_medians[-which(MAF_all_sites_medians$ifim_site_name == "Plains Mill"),]

#plot_labels <- data.frame(ifim_site_name=MAF_all_sites_medians$ifim_site_name,
#                          ifim_da_sqmi=MAF_all_sites_medians$ifim_da_sqmi,
#                          #pctchg=-33)
#                          pctchg=-63)

plot_labels <- data.frame(ifim_site_name=ascending_MAF_all_sites$ifim_site_name,
                          ifim_da_sqmi=ascending_MAF_all_sites$ifim_da_sqmi,
                          #pctchg=-33)
                          pctchg=-63)

#Aug_all_sites <- Aug_all_sites[order(Aug_all_sites$ifim_da_sqmi),] 

#Aug_all_sites$ifim_da_sqmi <- as.integer(Aug_all_sites$ifim_da_sqmi)

#ggplot(Aug_all_sites, aes(x = reorder(ifim_da_sqmi, ifim_da_sqmi), y = pctchg))+
  
  #ggplot(Aug_all_sites, aes(x = reorder(ifim_da_sqmi, ifim_da_sqmi), y = pctchg))+

  
 # Aug_all_sites$ifim_da_sqmi <- as.character(Aug_all_sites$ifim_da_sqmi)
#  Aug_all_sites$count <- as.numeric(ave(Aug_all_sites$ifim_da_sqmi, Aug_all_sites$ifim_da_sqmi, FUN = length))

#  length(Aug_all_sites$ifim_da_sqmi)
#  length(Aug_all_sites$count)
#     
#   ggplot(Aug_all_sites, aes(x = reorder(ifim_da_sqmi, ifim_da_sqmi), y = pctchg))+  
#   geom_boxplot(fill='#A4A4A4', color="darkred")+
#   geom_text(data=plot_labels, aes(label=ifim_site_name),hjust=0, vjust=0,size = 3,angle = 90)+
#   geom_hline(yintercept=0,col='#A4A4A4')+
#   labs(title = paste("Percent Habitat Change with 10% Flow Reduction (August - 10th Percentile and Below)",sep=""))+
#        ##subtitle = paste("ifim_site_name",":\nDrainage Area: ","ifim_da_sqmi"," sqmi\nUSGS: ","gage"," (","start_date"," to ","end_date",")",sep=""))+
#   xlab("Drainage Area (sqmi)")+ 
#   ylab("Percent Habitat Change")+
# 
#   #scale_x_continuous(trans = "log",breaks = c(0,10,100,1000,10000), labels =c(0,10,100,1000,10000))+
#   scale_y_continuous(limits = c(-35, 40))
# 
# 
# 
# filename <- paste("median_10pct_mo_August_TENTH.png", sep="")
# ggsave(file=filename, path = save_directory, width=15, height=8)

#----------------------------------------------------------------

#reg <- lm(log(Aug_all_sites_medians$ifim_da_sqmi)~Aug_all_sites_medians$pctchg)
#b <- as.numeric(reg[1]$coefficients[1])
#a <- as.numeric(reg[1]$coefficients[2])

#fit <- lm(Aug_all_sites_medians$pctchg~log(Aug_all_sites_medians$ifim_da_sqmi))



#month <- "MAF"

months <- c("MAF","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#r<-1
for (r in 1:length(months)) {
  month <- months[r]
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

stat <- "median"


month_data_medians <- aggregate(list(pctchg = month_data$pctchg), 
                                   list(ifim_da_sqmi = month_data$ifim_da_sqmi), 
                                   stat)

fit <- lm(month_data_medians$pctchg~log(month_data_medians$ifim_da_sqmi))
b <- as.numeric(fit[1]$coefficients[1])
m <- as.numeric(fit[1]$coefficients[2])


if (length(which(all_sites$ifim_site_name=="T11&12")) > 1){
  x2 <- 12500  
} else {
  x2 <- 4000   
}

y2 <- m*log(x2)+b

x1 <- 150
y1 <- m*log(x1)+b


#reg <- data.frame(x=c(0,4000),y=c(b,y2))

#(0-b)/m
#if (fish.only == "yes") {
#  plot_name <- paste(month,"_boxplots_",stat,"_FISH",sep="") 
#} else {
#  plot_name <- paste(month,"_boxplots_",stat,sep="") 
#}




if (fish.only == "yes") {
  plot_name <- paste(month,"_boxplots_",stat,"_FISH",sep="") 
} else {
  plot_name <- paste(month,"_boxplots_",stat,sep="") 
}

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

 
tiff(filename=paste(save_directory,"\\",plot_name,"_",pctchg,".tiff",sep=""),
    width = 1500, 
    height = 750)

# boxplot(month_data$pctchg ~ month_data$ifim_da_sqmi, 
#         at = sort(unique(month_data$ifim_da_sqmi)), 
#         boxwex=0.04, #box width (was 0.04 when doing log, 25 for linear)
#         log="x",
#         #ylim=c(-30,60),
#         ylim=c(-60,60),
#         main=paste("Percent Habitat Change with ",pctchg,"% Flow Reduction (",month,")",sep=""),
#         xlab="Drainage Area (sqmi)",
#         ylab="Percent Habitat Change",cex.lab=1.25)#,
#         #xaxt="n")
# 
# text(plot_labels$ifim_da_sqmi, 
#      plot_labels$pctchg, 
#      plot_labels$ifim_site_name, 
#      cex=1.0, #site label size (was 1.2 when doing log, 1.0 for linear)
#      pos=4, 
#      col="black",
#      srt=90)

#plot(month_data_medians)
#abline(a=0,b=0)
#points(month_data_medians$ifim_da_sqmi, month_data_medians$pctchg, col = "red",cex=5,pch="-") #pch=19 for circle point
#abline(lm(Aug_all_sites_medians$pctchg~log(Aug_all_sites_medians$ifim_da_sqmi)), 
#       col = "blue",
#       lwd=2)

## sign check to avoid having plus followed by minus for negative coefficients
#eq <- paste("Regression Equation:\n","y = ",round(m,2)," ln(x) ",round(b,2),
#            "\ny = 0 @ ",round(exp((-b)/m),1)," (sqmi)",sep="")

## printing of the equation
#text(x = 210, y = 45, labels = eq, cex=2)

#abline(fit)
#lines(reg,type="o", lty=2, col="blue")
#segments(166, 0, 3000, 8.788643)
#segments(x1, y1, x2, y2,col='red',lwd=1)
#----------------------------------------------------- 
#----------------------------------------------------- 
# site_subset <- rbind(site_subset, all_sites[which(all_sites$ifim_site_name=="Dunlap"),])       
# site_subset <- rbind(site_subset, all_sites[which(all_sites$ifim_site_name=="Craig"),])  
# site_subset <- rbind(site_subset, all_sites[which(all_sites$ifim_site_name=="Plains Mill"),])    
# site_subset <- rbind(site_subset, all_sites[which(all_sites$ifim_site_name=="North Anna Piedmont"),])      
# site_subset <- rbind(site_subset, all_sites[which(all_sites$ifim_site_name=="North Anna Fall Zone"),])  
#(m(log((1)/(1-z))))/(m(log(x))+b)



################################################################################################################
# RETRIEVE ELF SUBMITTALS WITH REST
################################################################################################################
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
hydro_tools <- 'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools\\' #location of hydro-tools repo
#----------------------------------------------

#Generate REST token for authentication              
rest_uname = FALSE
rest_pw = FALSE
source(paste(hydro_tools,"auth.private", sep = "\\")); #load rest username and password, contained in auth.private file
source(paste(hydro_tools,"VAHydro-2.0","rest_functions.R", sep = "\\")) #load REST functions
token <- rest_token(site, token, rest_uname, rest_pw)


submittal_inputs <- list(
  bundle = 'authority',
  ftype = 'federal_enviro_agency',
  admincode = 'epa',
  stringsAsFactors = FALSE
) 
submittal.dataframe <- getAdminregFeature(submittal_inputs, site, adminreg_feature)
submittal.adminid <- as.character(submittal.dataframe$adminid)


Dunlap.huc10.inputs <- list(bundle = 'watershed',ftype = 'nhd_huc10',hydrocode = '0208020103',stringsAsFactors=FALSE) 
Dunlap.huc10.dataframe <- getFeature(Dunlap.huc10.inputs, site, feature)
Dunlap.huc10.hydroid <- as.character(Dunlap.huc10.dataframe$hydroid)
Dunlap.huc10.stats <- fn_dh_elfstats(feature_ftype = 'nhd_huc10',yvar = 'aqbio_nt_total',xvar = 'erom_q0001e_mean',sampres = 'species',dataset_tag = 'bpj-530',featureid = Dunlap.huc10.hydroid)





base_url <- "http://deq2.bse.vt.edu/d.dh/"
site <- base_url
#Set location of "fn_dh_elfstats" function. Choose one below
path <- "/Users/nrf46657/Desktop/VAHydro Development/GitHub/r-dh-ecohydro/Analysis/"
#Set location of save path Choose one below
save_directory <- "C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/plots/"                    
source(paste(path,"query_elf_statistics.R", sep = ""))

Dunlap.huc10.stats <- fn_dh_elfstats(feature_ftype = 'nhd_huc10', 
                            yvar = 'aqbio_nt_total', 
                            xvar = 'erom_q0001e_mean',
                            sampres = 'species',
                            dataset_tag = 'bpj-530',
                            featureid = Dunlap.huc10.hydroid)


Dunlap.huc10.stats <- fn_dh_elfstats(feature_ftype = 'nhd_huc10',yvar = 'aqbio_nt_total',xvar = 'erom_q0001e_mean',sampres = 'species',dataset_tag = 'bpj-530',featureid = Dunlap.huc10.hydroid)




################################################################################################################
# RETREIEVE IFIM SITE DH FEATURE AND ITS MAF AND DA PROPERTIES
################################################################################################################
Dunlap.site.inputs <- list(bundle = 'monitoringpoint',ftype = 'ifim_transect',hydrocode = 'ifim_uja_00',stringsAsFactors=FALSE) 
Dunlap.site.dataframe <- getFeature(Dunlap.site.inputs, site, feature)
Dunlap.site.hydroid <- as.character(Dunlap.site.dataframe$hydroid)
#
Dunlap.maf.inputs <-list(featureid = Dunlap.site.hydroid,varkey = 'erom_q0001e_mean',entity_type = 'dh_feature')
Dunlap.maf.dataframe <- getProperty(Dunlap.maf.inputs, site, prop)
Dunlap.erom_q0001e_mean <- as.numeric(as.character(Dunlap.maf.dataframe$propvalue))
#
Dunlap.da.inputs <-list(featureid = Dunlap.site.hydroid,varkey = 'nhdp_drainage_sqmi',entity_type = 'dh_feature')
Dunlap.da.dataframe <- getProperty(Dunlap.da.inputs, site, prop)
Dunlap.nhdp_drainage_sqmi <- as.numeric(as.character(Dunlap.da.dataframe$propvalue))
#---------------------------------------------------------------------------------------------------------------
PlainsMill.site.inputs <- list(bundle = 'monitoringpoint',ftype = 'ifim_transect',hydrocode = 'ifim_nfs_00',stringsAsFactors=FALSE) 
PlainsMill.site.dataframe <- getFeature(PlainsMill.site.inputs, site, feature)
PlainsMill.site.hydroid <- as.character(PlainsMill.site.dataframe$hydroid)
#
PlainsMill.maf.inputs <-list(featureid = PlainsMill.site.hydroid,varkey = 'erom_q0001e_mean',entity_type = 'dh_feature')
PlainsMill.maf.dataframe <- getProperty(PlainsMill.maf.inputs, site, prop)
PlainsMill.erom_q0001e_mean <- as.numeric(as.character(PlainsMill.maf.dataframe$propvalue))
#
PlainsMill.da.inputs <-list(featureid = PlainsMill.site.hydroid,varkey = 'nhdp_drainage_sqmi',entity_type = 'dh_feature')
PlainsMill.da.dataframe <- getProperty(PlainsMill.da.inputs, site, prop)
PlainsMill.nhdp_drainage_sqmi <- as.numeric(as.character(PlainsMill.da.dataframe$propvalue))
#---------------------------------------------------------------------------------------------------------------
NorthAnnaPiedmont.site.inputs <- list(bundle = 'monitoringpoint',ftype = 'ifim_transect',hydrocode = 'ifim_yor_00',stringsAsFactors=FALSE) 
NorthAnnaPiedmont.site.dataframe <- getFeature(NorthAnnaPiedmont.site.inputs, site, feature)
NorthAnnaPiedmont.site.hydroid <- as.character(NorthAnnaPiedmont.site.dataframe$hydroid)
#
NorthAnnaPiedmont.maf.inputs <-list(featureid = NorthAnnaPiedmont.site.hydroid,varkey = 'erom_q0001e_mean',entity_type = 'dh_feature')
NorthAnnaPiedmont.maf.dataframe <- getProperty(NorthAnnaPiedmont.maf.inputs, site, prop)
NorthAnnaPiedmont.erom_q0001e_mean <- as.numeric(as.character(NorthAnnaPiedmont.maf.dataframe$propvalue))
#
NorthAnnaPiedmont.da.inputs <-list(featureid = NorthAnnaPiedmont.site.hydroid,varkey = 'nhdp_drainage_sqmi',entity_type = 'dh_feature')
NorthAnnaPiedmont.da.dataframe <- getProperty(NorthAnnaPiedmont.da.inputs, site, prop)
NorthAnnaPiedmont.nhdp_drainage_sqmi <- as.numeric(as.character(NorthAnnaPiedmont.da.dataframe$propvalue))
#---------------------------------------------------------------------------------------------------------------
NorthAnnaFallZone.site.inputs <- list(bundle = 'monitoringpoint',ftype = 'ifim_transect',hydrocode = 'ifim_yor_01',stringsAsFactors=FALSE) 
NorthAnnaFallZone.site.dataframe <- getFeature(NorthAnnaFallZone.site.inputs, site, feature)
NorthAnnaFallZone.site.hydroid <- as.character(NorthAnnaFallZone.site.dataframe$hydroid)
#
NorthAnnaFallZone.maf.inputs <-list(featureid = NorthAnnaFallZone.site.hydroid,varkey = 'erom_q0001e_mean',entity_type = 'dh_feature')
NorthAnnaFallZone.maf.dataframe <- getProperty(NorthAnnaFallZone.maf.inputs, site, prop)
NorthAnnaFallZone.erom_q0001e_mean <- as.numeric(as.character(NorthAnnaFallZone.maf.dataframe$propvalue))
#
NorthAnnaFallZone.da.inputs <-list(featureid = NorthAnnaFallZone.site.hydroid,varkey = 'nhdp_drainage_sqmi',entity_type = 'dh_feature')
NorthAnnaFallZone.da.dataframe <- getProperty(NorthAnnaFallZone.da.inputs, site, prop)
NorthAnnaFallZone.nhdp_drainage_sqmi <- as.numeric(as.character(NorthAnnaFallZone.da.dataframe$propvalue))
#---------------------------------------------------------------------------------------------------------------
Craig.site.inputs <- list(bundle = 'monitoringpoint',ftype = 'ifim_transect',hydrocode = 'ifim_uja_01',stringsAsFactors=FALSE) 
Craig.site.dataframe <- getFeature(Craig.site.inputs, site, feature)
Craig.site.hydroid <- as.character(Craig.site.dataframe$hydroid)
#
Craig.maf.inputs <-list(featureid = Craig.site.hydroid,varkey = 'erom_q0001e_mean',entity_type = 'dh_feature')
Craig.maf.dataframe <- getProperty(Craig.maf.inputs, site, prop)
Craig.erom_q0001e_mean <- as.numeric(as.character(Craig.maf.dataframe$propvalue))
#
Craig.da.inputs <-list(featureid = Craig.site.hydroid,varkey = 'nhdp_drainage_sqmi',entity_type = 'dh_feature')
Craig.da.dataframe <- getProperty(Craig.da.inputs, site, prop)
Craig.nhdp_drainage_sqmi <- as.numeric(as.character(Craig.da.dataframe$propvalue))
#---------------------------------------------------------------------------------------------------------------


################################################################################################################
# RETREIEVE ELF STAT SUBMITTALS
################################################################################################################
#xvar <- 'erom_q0001e_mean'
xvar <- 'erom_q0001e_july'

Dunlap.huc10.inputs <- list(bundle = 'watershed',ftype = 'nhd_huc10',hydrocode = '0208020103',stringsAsFactors=FALSE) 
Dunlap.huc10.dataframe <- getFeature(Dunlap.huc10.inputs, site, feature)
Dunlap.huc10.hydroid <- as.character(Dunlap.huc10.dataframe$hydroid)
Dunlap.huc10.stats <- fn_dh_elfstats(feature_ftype = 'nhd_huc10',yvar = 'aqbio_nt_total',xvar = xvar,sampres = 'species',stat_quantreg_qu = "0.50",dataset_tag = 'bpj-q50',featureid = Dunlap.huc10.hydroid)

PlainsMill.huc10.inputs <- list(bundle = 'watershed',ftype = 'nhd_huc10',hydrocode = '0207000603',stringsAsFactors=FALSE) 
PlainsMill.huc10.dataframe <- getFeature(PlainsMill.huc10.inputs, site, feature)
PlainsMill.huc10.hydroid <- as.character(PlainsMill.huc10.dataframe$hydroid)
PlainsMill.huc10.stats <- fn_dh_elfstats(feature_ftype = 'nhd_huc10',yvar = 'aqbio_nt_total',xvar = xvar,sampres = 'species',stat_quantreg_qu = "0.50",dataset_tag = 'bpj-q50',featureid = PlainsMill.huc10.hydroid)

NorthAnna.huc10.inputs <- list(bundle = 'watershed',ftype = 'nhd_huc10',hydrocode = '0208010608',stringsAsFactors=FALSE) 
NorthAnna.huc10.dataframe <- getFeature(NorthAnna.huc10.inputs, site, feature)
NorthAnna.huc10.hydroid <- as.character(NorthAnna.huc10.dataframe$hydroid)
NorthAnna.huc10.stats <- fn_dh_elfstats(feature_ftype = 'nhd_huc10',yvar = 'aqbio_nt_total',xvar = xvar,sampres = 'species',stat_quantreg_qu = "0.50",dataset_tag = 'bpj-q50',featureid = NorthAnna.huc10.hydroid)

Craig.huc10.inputs <- list(bundle = 'watershed',ftype = 'nhd_huc10',hydrocode = '0208020112',stringsAsFactors=FALSE) 
Craig.huc10.dataframe <- getFeature(Craig.huc10.inputs, site, feature)
Craig.huc10.hydroid <- as.character(Craig.huc10.dataframe$hydroid)
Craig.huc10.stats <- fn_dh_elfstats(feature_ftype = 'nhd_huc10',yvar = 'aqbio_nt_total',xvar = xvar,sampres = 'species',stat_quantreg_qu = "0.50",dataset_tag = 'bpj-q50',featureid = Craig.huc10.hydroid)

################################################################################################################
# CALCULATE PERCENT CHANGE IN RICHNESS USING m, b, AND SITE MAF 
################################################################################################################
pctchg <-10


Dunlap.huc10.m <- Dunlap.huc10.stats$out_m
Dunlap.huc10.b <- Dunlap.huc10.stats$out_b
Dunlap <- ((Dunlap.huc10.m*(log((1)/(1-(as.numeric(pctchg)/100)))))/(Dunlap.huc10.m*(log(Dunlap.erom_q0001e_mean))+Dunlap.huc10.b))*100

PlainsMill.huc10.m <- PlainsMill.huc10.stats$out_m
PlainsMill.huc10.b <- PlainsMill.huc10.stats$out_b
PlainsMill <- ((PlainsMill.huc10.m*(log((1)/(1-(as.numeric(pctchg)/100)))))/(PlainsMill.huc10.m*(log(PlainsMill.erom_q0001e_mean))+PlainsMill.huc10.b))*100

NorthAnnaPiedmont.huc10.m <- NorthAnnaPiedmont.huc10.stats$out_m
NorthAnnaPiedmont.huc10.b <- NorthAnnaPiedmont.huc10.stats$out_b
NorthAnnaPiedmont <- ((NorthAnnaPiedmont.huc10.m*(log((1)/(1-(as.numeric(pctchg)/100)))))/(NorthAnnaPiedmont.huc10.m*(log(NorthAnnaPiedmont.erom_q0001e_mean))+NorthAnnaPiedmont.huc10.b))*100

NorthAnnaFallZone.huc10.m <- NorthAnnaFallZone.huc10.stats$out_m
NorthAnnaFallZone.huc10.b <- NorthAnnaFallZone.huc10.stats$out_b
NorthAnnaFallZone <- ((NorthAnnaFallZone.huc10.m*(log((1)/(1-(as.numeric(pctchg)/100)))))/(NorthAnnaFallZone.huc10.m*(log(NorthAnnaFallZone.erom_q0001e_mean))+NorthAnnaFallZone.huc10.b))*100

Craig.huc10.m <- Craig.huc10.stats$out_m
Craig.huc10.b <- Craig.huc10.stats$out_b
Craig <- ((Craig.huc10.m*(log((1)/(1-(as.numeric(pctchg)/100)))))/(Craig.huc10.m*(log(Craig.erom_q0001e_mean))+Craig.huc10.b))*100
################################################################################################################
################################################################################################################
# pctchg <-10
# 
# Dunlap                <- ((1.22*(log((1)/(1-(as.numeric(pctchg)/100)))))/(1.22*(log(193.252))+16.2))*100
# Craig                 <- ((1.22*(log((1)/(1-(as.numeric(pctchg)/100)))))/(1.22*(log(410.75))+16.2))*100
# 
# Plains_Mill           <- ((1.22*(log((1)/(1-(as.numeric(pctchg)/100)))))/(1.22*(log(314.966))+16.2))*100
# North_Anna_Piedmont   <- ((1.22*(log((1)/(1-(as.numeric(pctchg)/100)))))/(1.22*(log(374.153))+16.2))*100
# North_Anna_Fall_Zone  <- ((1.22*(log((1)/(1-(as.numeric(pctchg)/100)))))/(1.22*(log(400.778))+16.2))*100


#----------------------------------------------------- 
#----------------------------------------------------- 
plot(month_data_medians,
     ylim=c(-80,80))
     #ylim=c(-5,5))
abline(a=0,b=0)

points(month_data_medians$ifim_da_sqmi, month_data_medians$pctchg, col = "red",cex=5,pch="-") #pch=19 for circle point




        
dev.off()


} #end for loop

