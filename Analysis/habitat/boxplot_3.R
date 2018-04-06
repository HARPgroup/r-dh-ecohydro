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
#ifim_sites <- c(397290,397291,397292,397293,397294,397299,397300,397301,397302,397303,397304,397305,397282,397283,397295,397296,397286,397287,397288,397289,397297,397298)


#for (x in 1:length(ifim_sites)){
#  ifim_featureid <- ifim_sites[x]
  
ifim_featureid <- 397301
#ifim_featureid <-397295


ifim_dataframe <- vahydro_prop_matrix(ifim_featureid,'ifim_habitat_table')
WUA.df <- ifim_dataframe
targets <- colnames(WUA.df[-1])

#ifim_metric <- 'riffle'
ifim_metric <- 'smb_adult'
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

#head(wua.at.q_0)
#head(data.frame(wua.at.q_0))

#row_number(wua.at.q_0)
#row(wua.at.q_0)
#Flow <- row.names(wua.at.q_0)
#wua.at.q_0 <- data.frame(Flow,wua.at.q_0)

wua.at.q_0 <- data.frame(flow.ts.range_0,wua.at.q_0)


#f_10 <- f_fxn(gage,0.10)
#flow.ts.range_10 <- flow.ts.range_fxn(f_10,"ten_year")
#wua.at.q_10 <- wua.at.q_fxn(flow.ts.range_10)
#===============================================================
#===============================================================
col_all <- data.frame(col_all = c(''),
                      stringsAsFactors=FALSE)

#col_num <- 2
for (col_num  in 1:length(colnames(wua.at.q_0))){
col_data <- wua.at.q_0[,col_num]
col_data <- col_data[!is.na(col_data)]
  
col_mean <- mean(col_data)
col_name <- colnames(wua.at.q_0[col_num])

col_i <- data.frame(col_mean)
names(col_i)[1]<-paste(col_name)

col_all <-data.frame(col_all,col_i)
#pctchg_all <- rbind(pctchg_all, pctchg_i)
}
col_all <-col_all[,-1] #remove empty col
col_all <-col_all[,-1] #remove date col


#===============================================================
##flow.ts.range <- data.frame("MAF",ifim_maf)
##wua.at.maf <- wua.at.q_fxn(flow.ts.range)
#===============================================================
wua.at.maf <- col_all



# BEGIN CALCULATING PERCENT HABITAT LOSS
shen <- c(397290,397291,397292,397293,397294,397299,397300,397301) #need to check why shenandoah sites have "0" row
if (length(which(grepl(ifim_featureid, shen))) > 0){
  dataframe <- ifim_dataframe[-1,]
} else {
  dataframe <- ifim_dataframe
}


#Get column for metric of interest
#colnum <- grep(ifim_metric,colnames(ifim_dataframe))
#dataframe[,colnum]

#pct_list <- c(5,50,20,30,40,10)
pct_list <- c(10)

#table <- data.frame(x = dataframe$discharge,
#                    y = dataframe[,colnum],
#                    stringsAsFactors=FALSE)
#targets <- targets


table <- data.frame(metric = targets,
                    maf = ifim_maf,
                    maf_wua = as.numeric(wua.at.maf[1,]),
                    stringsAsFactors=FALSE)



#j <- 1
for (j in 1:length(pct_list)) {
#for (j in 1:length(table$metric)) {
    
  
  pctchg_all <- data.frame(newx = c(''),
                           newy= c(''),
                           pctchg = c(''),
                           stringsAsFactors=FALSE)


  #i <- 1
  #for (i in 1:length(dataframe$discharge)) { 
  #loop through metrics 
  for (i in 1:length(table$metric)) {  
    
    ifim_metric <- table$metric[i]
    colnum <- grep(ifim_metric,colnames(ifim_dataframe))
    colnum <- i+1
    
 #   x2 <- dataframe$discharge[i]
 #   y_base <- dataframe[,colnum][i]

    #x2 <- 400.8
    
    x2 <- table$maf[i]
    y_base <- table$maf_wua[i]
    
    newx <- x2 - (x2*(pct_list[j])/100)

    #if point minus x% exists as is next down (no interpolation needed)
    newy <- (dataframe[which(dataframe$discharge==newx), ])[,colnum]
    if(length(newy) == TRUE){
      pctchg <- ((y_base-newy)/y_base)*100
    } else {
      
      x1 <- max((subset(dataframe, discharge<newx))$discharge)
      y1 <- (dataframe[which(dataframe$discharg==x1), ])[,colnum]
      
      x2 <- min((subset(dataframe, discharge>newx))$discharge)
      y2 <- (dataframe[which(dataframe$discharg==x2), ])[,colnum]
      
      newy <- y1+((newx-x1)*((y2-y1)/(x2-x1)))
      
      pctchg <- ((y_base-newy)/y_base)*100
      
    }
    
    if (length(pctchg)==0){pctchg <- NA}
    if (length(newx)==0){newx <- NA}
    if (length(newy)==0){newy<- NA}
    pctchg_i <- data.frame(newx,newy,pctchg)
    pctchg_all <- rbind(pctchg_all, pctchg_i)
  }
  pctchg_all <- data.frame(pctchg_all[-1,])
  
  #detail table for individual % changes 
#  df_pctchg <- data.frame(x = dataframe$discharge,
#                          y = dataframe[,colnum],
#                          newx = pctchg_all$newx,
#                          newy = pctchg_all$newy,
#                          pctchg = pctchg_all$pctchg,
#                          stringsAsFactors=FALSE)
  
    df_pctchg <- data.frame(metric = table$metric,
                            maf = table$maf,
                            maf_wua = table$maf_wua,
                            newx = pctchg_all$newx,
                            newy = pctchg_all$newy,
                            pctchg = pctchg_all$pctchg,
                            stringsAsFactors=FALSE)
  
  
  table_i = data.frame(pctchg_all$pctchg)
  names(table_i) <- c(paste("pct_chg_",pct_list[j],sep=""))
  table <- cbind(table, table_i)
  
}

#convert table values to numeric before plotting 
box_table <- table
for (z in 2:length(table)) {
  box_table[,z] <-as.numeric(as.character(  table[,z]))
}

#remove canoe data before generating boxplot 
canoe_row <- which(box_table$metric=="canoe")
if(length(canoe_row) == TRUE){
  box_table <- box_table[-8,]
}
box_table$pct_chg_10 <- (box_table$pct_chg_10)*-1 #invert sign for yaxis % chg

ggplot(box_table, aes(as.factor(maf),pct_chg_10))+
  geom_boxplot()+
  geom_text(aes(label=metric),hjust=0, vjust=0)+
  ggtitle(paste(ifim_site_name,":\n\nHabitat Change with 10% Reduction in Mean Annual Flow",sep=""))+
  xlab("Mean Annual Flow (cfs)")+ 
  ylab("Percent Habitat Change")#+
  #scale_y_continuous(limits = c(-10, 100))
  
filename <- paste(ifim_site_name,"boxplot.png", sep="_")
ggsave(file=filename, path = save_directory, width=6, height=10)

#}
#---------------------------------------------------