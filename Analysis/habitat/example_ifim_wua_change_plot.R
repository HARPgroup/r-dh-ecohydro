#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
source(paste(github_location,'r-dh-ecohydro/Analysis/habitat','ifim_wua_change_plot.R',sep='/'))
source(paste(github_location,'r-dh-ecohydro/Analysis/habitat','hab_ts_functions.R',sep='/'))

ifim_featureid <- 397286 # NA Piedmont


# Now this doesn't work since updating password?
ifim_dataframe <- vahydro_prop_matrix(ifim_featureid, 'dh_feature','ifim_habitat_table')
WUA.df <- ifim_dataframe

  
targets <- colnames(WUA.df[-1])
#targets <- colnames(WUA.df)

ifim_site <- getFeature(list(hydroid = ifim_featureid), token, site, feature)
ifim_site_name <- as.character(ifim_site$name)

inputs = list(varkey = 'usgs_siteid',featureid = ifim_featureid,entity_type = 'dh_feature')
gageprop <- getProperty(inputs,site,prop)
gage <- as.character(gageprop$propcode)
gage_factor <- as.numeric(as.character(gageprop$propvalue))

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


# f_0 is the baseline flow timeseries could be Qbaseline 
model_flows <- om_get_rundata(207923, 11) # tbd this should come from IFIM model
model_flows$Qbaseline <- model_flows$Qout + (model_flows$wd_cumulative_mgd - model_flows$ps_cumulative_mgd ) * 1.547
ts1 <- as.data.frame(model_flows[,c('thisdate', 'Qout')])
ts1$thisdate <- as.character(as.Date(index(model_flows))) 
names(ts1) <- c('Date', 'Flow')
#mode(ts1) <- 'numeric'
ts1$Flow <- (as.numeric(ts1$Flow)*gage_factor) 
# f_10 is the post timeseries Qout
# or as a function of Q baseline
# f_10 <- model_flows[,c('thisdate', 'Qbaseline')]           
# f_10$thisdate <- as.character(as.Date(index(model_flows)))

model_flows <- om_get_rundata(207923, 13) # tbd this should come from IFIM model
ts2 <- as.data.frame(model_flows[,c('thisdate', 'Qout')])
ts2$thisdate <- as.character(as.Date(index(model_flows))) 
names(ts2) <- c('Date', 'Flow')
ts2 <- ts2
#mode(ts2) <- 'numeric'
ts2$Flow <- (as.numeric(ts2$Flow)*gage_factor)

ifim_wua_change_plot(ts1, ts2, WUA.df, 0.1)

