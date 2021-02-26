#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
library(hydrotools)
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
source(paste(github_location,'r-dh-ecohydro/Analysis/habitat','ifim_wua_change_plot.R',sep='/'))
source(paste(github_location,'r-dh-ecohydro/Analysis/habitat','hab_ts_functions.R',sep='/'))


# INPUTS #######################################################################################
ifim_featureid <- 397295 #Potomac 8&9
wshed_featureid <- 68346 #Potomac River Great Falls

################################################################################################
# RETRIEVE RSEG MODEL
wshed_model <- om_get_model(base_url, wshed_featureid, 'dh_feature', 'vahydro-1.0', 'any')
elid <- om_get_model_elementid(base_url, wshed_model$pid) 

################################################################################################
# RETRIEVE IFIM SITE FEATURE 
ifim_site <- getFeature(list(hydroid = ifim_featureid), token, site, feature)
ifim_site_name <- as.character(ifim_site$name)

################################################################################################
# RETRIEVE WUA TABLE 
ifim_dataframe <- vahydro_prop_matrix(ifim_featureid, 'dh_feature','ifim_habitat_table')
WUA.df <- t(ifim_dataframe)
targets <- colnames(WUA.df)[-1]

################################################################################################
# DERIVE MULTIPLYING FACTOR FOR AREA-WEIGHTING FLOWS AT MODEL OUTLET TO IFIM SITE LOCATION

# RETRIEVE IFIM SITE DA SQMI
ifim_da_sqmi <- getProperty(list(varkey = 'nhdp_drainage_sqmi',featureid = ifim_featureid,entity_type = 'dh_feature'),site,prop)
ifim_da_sqmi <- as.numeric(ifim_da_sqmi$propvalue)
# RETRIEVE RSEG DA SQMI
rseg_da_sqmi <- getProperty(list(varkey = 'wshed_drainage_area_sqmi',featureid = wshed_featureid,entity_type = 'dh_feature'),site,prop)
rseg_da_sqmi <- as.numeric(rseg_da_sqmi$propvalue)

weighting_factor <- ifim_da_sqmi/rseg_da_sqmi

# RETRIEVE USGS GAGE CODE
inputs = list(varkey = 'usgs_siteid',featureid = ifim_featureid,entity_type = 'dh_feature')
gageprop <- getProperty(inputs,site,prop)
gage <- as.character(gageprop$propcode)
# THIS FACTOR CAN BE USED IF USING GAGE FLOWS INSTEAD OF MODEL FLOWS
# gage_factor <- as.numeric(as.character(gageprop$propvalue))
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
# RETRIEVE RUN 11 MODEL FLOW TIMESERIES (using elid aka om_element_connection)
model_flows_11 <- om_get_rundata(elid, 11)
model_flows_11$Qbaseline <- model_flows_11$Qout + (model_flows_11$wd_cumulative_mgd - model_flows_11$ps_cumulative_mgd ) * 1.547
ts1 <- as.data.frame(model_flows_11[,c('thisdate', 'Qout')])
ts1$thisdate <- as.character(as.Date(index(model_flows_11))) 
names(ts1) <- c('Date', 'Flow')
ts1$Flow <- (as.numeric(ts1$Flow)*weighting_factor) #ADJUST MODEL FLOW USING WEIGHTING FACTOR

################################################################################################
# RETRIEVE RUN 13 MODEL FLOW TIMESERIES
model_flows_13 <- om_get_rundata(elid, 13)
model_flows_13$Qbaseline <- model_flows_13$Qout + (model_flows_13$wd_cumulative_mgd - model_flows_13$ps_cumulative_mgd ) * 1.547
ts2 <- as.data.frame(model_flows_13[,c('thisdate', 'Qout')])
ts2$thisdate <- as.character(as.Date(index(model_flows_13))) 
names(ts2) <- c('Date', 'Flow')
ts2 <- ts2
ts2$Flow <- (as.numeric(ts2$Flow)*weighting_factor) #ADJUST MODEL FLOW USING WEIGHTING FACTOR

################################################################################################
# PLOT THE HABITAT CHANGE BETWEEN THE 2 MODEL RUNS USING Qout

# ALL FLOWS
# ifim_plot <- ifim_wua_change_plot(ts1, ts2, WUA.df, 1.0,"ifim_da_sqmi" = ifim_da_sqmi,
#                                     runid_a = "11",
#                                     metric_a = "Qout",
#                                     runid_b = "13",
#                                     metric_b = "Qout")
# ifim_plot + ylim(c(-50,50))
# ggsave(paste(export_path,'ifim_boxplot_11Qout_13Qout_ALL_',elid,'.png',sep=""), width = 7, height = 4)


# FLOWS BELOW THE 0.05 PERCENTILE
ifim_plot05 <- ifim_wua_change_plot(ts1, ts2, WUA.df, 0.05,"ifim_da_sqmi" = ifim_da_sqmi,
                                    runid_a = "11",
                                    metric_a = "Qout",
                                    runid_b = "13",
                                    metric_b = "Qout")
ifim_plot05 + ylim(c(-50,50))
ggsave(paste(export_path,'ifim_boxplot_11Qout_13Qout_05_',elid,'.png',sep=""), width = 7, height = 4)

################################################################################################
# PLOT THE HABITAT CHANGE BETWEEN Qbaseline AND Qout FOR THE SECOND MODEL RUN

ts2base <- as.data.frame(model_flows_13[,c('thisdate', 'Qbaseline')])
ts2base$thisdate <- as.character(as.Date(index(model_flows_13))) 
names(ts2base) <- c('Date', 'Flow')

ts2base$Flow <- (as.numeric(ts2base$Flow)*weighting_factor) 
ifim_plot05 <- ifim_wua_change_plot(ts2base, ts2, WUA.df, 0.05,"ifim_da_sqmi" = ifim_da_sqmi,runid_a = "13",metric_a = "Qbaseline",runid_b = "13",metric_b = "Qout")
ifim_plot05 + ylim(c(-50,50))
ggsave(paste(export_path,'ifim_boxplot_13QQbaseline_13Qout_05_',elid,'.png',sep=""), width = 7, height = 4)

ifim_plot10 <- ifim_wua_change_plot(ts2base, ts2, WUA.df, 0.1,"ifim_da_sqmi" = ifim_da_sqmi,runid_a = "13",metric_a = "Qbaseline",runid_b = "13",metric_b = "Qout")
ifim_plot10 + ylim(c(-50,50))
ggsave(paste(export_path,'ifim_boxplot_13QQbaseline_13Qout_10_',elid,'.png',sep=""), width = 7, height = 4)

ifim_plot20 <- ifim_wua_change_plot(ts2base, ts2, WUA.df, 0.2,"ifim_da_sqmi" = ifim_da_sqmi,runid_a = "13",metric_a = "Qbaseline",runid_b = "13",metric_b = "Qout")
ifim_plot20 + ylim(c(-50,50))
ggsave(paste(export_path,'ifim_boxplot_13QQbaseline_13Qout_20_',elid,'.png',sep=""), width = 7, height = 4)

################################################################################################



model_flows_17 <- om_get_rundata(elid, 17) 
ts3 <- as.data.frame(model_flows_17[,c('thisdate', 'Qout')])
ts3$thisdate <- as.character(as.Date(index(model_flows_17))) 
names(ts3) <- c('Date', 'Flow')
ts1cc_daterange <- sqldf("select * from ts1 where Date <= '2000-09-30' ")
ifim_plot10cc <- ifim_wua_change_plot(ts1cc_daterange, ts3, WUA.df, 0.1,"ifim_da_sqmi" = ifim_da_sqmi,runid_a = "11",metric_a = "Qout",runid_b = "17",metric_b = "Qout")
ifim_plot10cc + ylim(c(-50,50))
ggsave(paste(export_path,'ifim_boxplot_11Qout_17Qout_10_',elid,'.png',sep=""), width = 7, height = 4)

