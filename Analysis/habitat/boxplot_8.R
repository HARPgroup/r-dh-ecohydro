rm(list = ls())  #clear variables
options(timeout=240); # set timeout to twice default level to avoid abort due to high traffic

#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------

#----FOR RUNNING LOCALLY:
basepath='C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\r-dh-ecohydro\\ELFGEN\\internal\\';
source(paste(basepath,'config.local.private',sep='/'));

#Load Functions               
source(paste(fxn_locations,"elf_retrieve_data.R", sep = ""));  #loads function used to retrieve F:E data from VAHydro
source(paste(habitat_files,'hab_ts_functions.R',sep='/')) #loads habtat timeseries functions
source(paste(fxn_vahydro,"rest_functions.R", sep = ""));       #loads file containing function that retrieves REST token
rest_uname = FALSE;
rest_pw = FALSE;
source(paste(fxn_locations,"rest.private", sep = ""));         #load rest username and password, contained in rest.private file
token <- rest_token(site, token, rest_uname, rest_pw);

#_________________________________________________________
# PROJECT               SITE HYDROIDS
#
# NF_Shenandoah         397290,397291,397292,397293,397294
# SF_Shenandoah         397299,397300,397301
# Upper_James           397302,397303,397304,397305
# New_River_Claytor     397284,397285
# Appomattox            397282,397283
# Potomac               397295,397296
# North_Anna            397286,397287,397288,397289
# Roanoke               397297,397298
#_________________________________________________________

#===============================================================
# BEGIN RETRIEVE IFIM DATA
#===============================================================
ifim_sites <- c(397290,397291,397292,397293,397294,397299,397300,397301,397302,397303,397304,397305,397282,397283,397295,397296,397286,397287,397288,397289,397297,397298)


for (x in 1:length(ifim_sites)){
  ifim_featureid <- ifim_sites[x]
  
#ifim_featureid <- 397301



ifim_dataframe <- vahydro_prop_matrix(ifim_featureid,'ifim_habitat_table')
WUA.df <- ifim_dataframe
targets <- colnames(WUA.df[-1])

#ifim_metric <- 'riffle'
#ifim_metric <- 'smb_adult'
#ifim_metric <- 'smb_sub_adult'
#ifim_metric <- 'smb_juv'
#ifim_metric <- 'smb_yoy'
#ifim_metric <- 'smb_spawn'

ifim_site <- getFeature(list(hydroid = ifim_featureid), token, site, feature)
ifim_site_name <- as.character(ifim_site$name)

inputs = list(varkey = 'usgs_siteid',featureid = ifim_featureid,entity_type = 'dh_feature')
gage <- getProperty(inputs,site,prop)
gage <- as.character(gage$propcode)

ifim_da_sqmi <- getProperty(list(featureid = ifim_featureid,
                                 varkey = 'nhdp_drainage_sqmi',
                                 entity_type = 'dh_feature'), 
                            site, prop)

ifim_da_sqmi <- round(as.numeric(as.character(ifim_da_sqmi$propvalue)),1)

ifim_maf <- getProperty(list(featureid = ifim_featureid,
                             varkey = 'erom_q0001e_mean',
                             entity_type = 'dh_feature'), 
                        site, prop)

ifim_maf <- round(as.numeric(as.character(ifim_maf$propvalue)),1)
#===============================================================
# END RETRIEVE IFIM DATA
#===============================================================

#===============================================================
#===============================================================

f_0 <- f_fxn(gage,0.0)
flow.ts.range_0 <- flow.ts.range_fxn(f_0,"all")
wua.at.q_0 <- wua.at.q_fxn(flow.ts.range_0)
wua.at.q_0 <- data.frame(flow.ts.range_0,wua.at.q_0)

f_10 <- f_fxn(gage,0.10)
flow.ts.range_10 <- flow.ts.range_fxn(f_10,"all")
wua.at.q_10 <- wua.at.q_fxn(flow.ts.range_10)
wua.at.q_10 <- data.frame(flow.ts.range_10,wua.at.q_10)

#-------------------------------------------------------------------------------------------------

wua_10pct_chg <- ((wua.at.q_0-wua.at.q_10)/wua.at.q_0)*100
wua_10pct_chg<-wua_10pct_chg[,-1] #remove empty date col
wua_10pct_chg<-wua_10pct_chg[,-1] #remove empty flow col
wua_10pct_chg <- (wua_10pct_chg)*-1
Date <- wua.at.q_0$Date
wua_10pct_chg <- data.frame(Date,wua_10pct_chg)
#negative means habitat loss

#-------------------------------------------------------------------------------------------------
#Add column of month names
wua_10pct_chg [grep('-01-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "Jan"
wua_10pct_chg [grep('-02-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "Feb"
wua_10pct_chg [grep('-03-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "Mar"
wua_10pct_chg [grep('-04-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "Apr"
wua_10pct_chg [grep('-05-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "May"
wua_10pct_chg [grep('-06-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "Jun"
wua_10pct_chg [grep('-07-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "Jul"
wua_10pct_chg [grep('-08-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "Aug"
wua_10pct_chg [grep('-09-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "Sep"
wua_10pct_chg [grep('-10-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "Oct"
wua_10pct_chg [grep('-11-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "Nov"
wua_10pct_chg [grep('-12-', wua_10pct_chg$Date, perl= TRUE), "month" ] <- "Dec"
#-------------------------------------------------------------------------------------------------
start_date <- min(wua_10pct_chg$Date)
end_date <- max(wua_10pct_chg$Date)

#Jan_rows <- which(wua_10pct_chg$month == "Jan")
Jan_data <- wua_10pct_chg[which(wua_10pct_chg$month == "Jan"),]
Feb_data <- wua_10pct_chg[which(wua_10pct_chg$month == "Feb"),]
Mar_data <- wua_10pct_chg[which(wua_10pct_chg$month == "Mar"),]
Apr_data <- wua_10pct_chg[which(wua_10pct_chg$month == "Apr"),]
May_data <- wua_10pct_chg[which(wua_10pct_chg$month == "May"),]
Jun_data <- wua_10pct_chg[which(wua_10pct_chg$month == "Jun"),]
Jul_data <- wua_10pct_chg[which(wua_10pct_chg$month == "Jul"),]
Aug_data <- wua_10pct_chg[which(wua_10pct_chg$month == "Aug"),]
Sep_data <- wua_10pct_chg[which(wua_10pct_chg$month == "Sep"),]
Oct_data <- wua_10pct_chg[which(wua_10pct_chg$month == "Oct"),]
Nov_data <- wua_10pct_chg[which(wua_10pct_chg$month == "Nov"),]
Dec_data <- wua_10pct_chg[which(wua_10pct_chg$month == "Dec"),]

dfList <- list(MAF = wua_10pct_chg, 
               Jan = Jan_data,
               Feb = Feb_data,
               Mar = Mar_data,
               Apr = Apr_data,
               May = May_data,
               Jun = Jun_data,
               Jul = Jul_data,
               Aug = Aug_data,
               Sep = Sep_data,
               Oct = Oct_data,
               Nov = Nov_data,
               Dec = Dec_data)

colnames(wua_10pct_chg)


box_table <- data.frame(metric = c(''),
                        flow = c(''),
                        pctchg = c(''),
                        stringsAsFactors=FALSE)

#L <- 1
for (L in 1:length(dfList)) {
  hab.df <- dfList[L]
  hab.df <- data.frame(hab.df)
  names(hab.df) <- colnames(wua_10pct_chg)

#-------------------------------------------------------------------------------------------------
#Build dataframe of means of each metric percent change

#hab.df <- wua_10pct_chg 

col_all <- data.frame(col_all = c(''),
                      stringsAsFactors=FALSE)

#col_num <- 6
for (col_num  in 1:length(colnames(hab.df))){
  col_data <- hab.df[,col_num]
  col_data <- col_data[!is.na(col_data)] #remove NA values prior to calculating mean
  col_data <- col_data[!is.infinite(col_data)]   #remove Inf values prior to calculating mean
  
  col_mean <- mean(col_data)
  col_name <- colnames(hab.df[col_num])
  
  col_i <- data.frame(col_mean)
  names(col_i)[1]<-paste(col_name)
  
  col_all <-data.frame(col_all,col_i)
}
col_all <-col_all[,-1] #remove empty col
col_all <-col_all[,-1] #remove date col
col_all = col_all[,!(names(col_all) %in% "month")] #remove month col

#} #close df loop

#-------------------------------------------------------------------------------------------------
#box_table_i <- data.frame(newx,newy,pctchg)
box_table_i <- data.frame(metric = targets,
                        flow = names(dfList[L]),
                        pctchg = as.numeric(col_all[1,]),
                        stringsAsFactors=FALSE)

box_table <- rbind(box_table, box_table_i)
} #close df loop
# box_table <- data.frame(metric = targets,
#                         flow = "Mean Annual Flow",
#                         pctchg = as.numeric(col_all[1,]),
#                         stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------------------------
#convert table values to numeric before plotting 
#box_table <- wua_10pct_chg
#for (z in 2:length(table)) {
#  box_table[,z] <-as.numeric(as.character(  table[,z]))
#}
#box_table[,2] <- round(box_table[,2], digits = 0)

#remove canoe data before generating boxplot 
canoe_row <- which(box_table$metric=="canoe")
if((length(canoe_row)>0) == TRUE){
  box_table <- box_table[-canoe_row,]
}

box_table <- box_table [-1,] #remove empty first row
#box_table$pct_chg_10 <- (box_table$pct_chg_10)*-1 #invert sign for yaxis % chg

#convert table values to numeric before plotting 
  box_table[,3] <- as.numeric(as.character(box_table[,3]))
#box_table[,3] <- round(box_table[,2], digits = 0)

#ggplot(box_table, aes(as.factor(maf),pct_chg_10))+
#ggplot(box_table, aes(as.factor(flow),pctchg))+
ggplot(box_table, aes(flow,pctchg))+
  geom_boxplot(fill='#A4A4A4', color="darkred")+
  geom_text(aes(label=metric),hjust=0, vjust=0)+
  geom_hline(yintercept=0,col='#A4A4A4')+
  labs(title = "Percent Habitat Change with 10% Flow Reduction",
       subtitle = paste(ifim_site_name,":\nDrainage Area: ",ifim_da_sqmi," sqmi\nUSGS: ",gage," (",start_date," to ",end_date,")",sep=""))+

  xlab("Flow (cfs)")+ 
  ylab("Percent Habitat Change")+
  scale_x_discrete(limit = c("MAF",month.abb))
#scale_y_continuous(limits = c(-10, 100))

filename <- paste(ifim_site_name,"boxplot.png", sep="_")
ggsave(file=filename, path = save_directory, width=14, height=8)



}

#-------------------------------------------------------------------------------------------------
