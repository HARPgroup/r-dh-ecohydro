#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
source(paste(github_location,'r-dh-ecohydro/Analysis/habitat','ifim_wua_change_plot.R',sep='/'))
source(paste(github_location,'r-dh-ecohydro/Analysis/habitat','hab_ts_functions.R',sep='/'))

ifim_featureid <- 397295 # Potomac 8&9
wshed_featureid <- 68346
wshed_model <- om_get_model(base_url, wshed_featureid, 'dh_feature', 'vahydro-1.0', 'any')
elid <- om_get_model_elementid(base_url, wshed_model$pid) 

# This is goofed up.  The jsonlite returns a transpoed matrix
# the rjson returns the right matrix orientation but the first row 
# is turned into column names instead of a row.  How to sort this out?
#ifim_dataframe <- vahydro_prop_matrix(ifim_featureid, 'dh_feature','ifim_habitat_table')
#ifim_model <- om_get_model(base_url, ifim_featureid, 'dh_feature', 'vahydro-1.0', 'any')
#ifim_model_wua <- om_get_prop(base_url, ifim_model$pid, 'dh_properties', 'wua')
# Alt - get from property, this *should* work, but does not...
#iWUA.df <-jsonlite::unserializeJSON(ifim_model_wua$field_dh_matrix)
#colnames(iWUA.df[-1])
#WUA.df <- t(jsonlite::unserializeJSON(ifim_dataframe$field_dh_matrix))
# get wua from feature
ifim_dataframe <- vahydro_prop_matrix(ifim_featureid, 'dh_feature','ifim_habitat_table')
#WUA.df <- ifim_dataframe
WUA.df <- t(ifim_dataframe)

targets <- colnames(WUA.df)[-1]

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
model_flows <- om_get_rundata(elid, 11) # tbd this should come from IFIM model
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

model_flows <- om_get_rundata(elid, 13) # tbd this should come from IFIM model
model_flows$Qbaseline <- model_flows$Qout + (model_flows$wd_cumulative_mgd - model_flows$ps_cumulative_mgd ) * 1.547
ts2 <- as.data.frame(model_flows[,c('thisdate', 'Qout')])
ts2$thisdate <- as.character(as.Date(index(model_flows))) 
names(ts2) <- c('Date', 'Flow')
ts2 <- ts2
#mode(ts2) <- 'numeric'
ts2$Flow <- (as.numeric(ts2$Flow)*gage_factor)

# Plot difference between runid 1 and runid 2
ifim_wua_change_plot(ts1, ts2, WUA.df, 0.1)
# Plot difference between runid 1 and runid 2
ifim_plot05 <- ifim_wua_change_plot(ts1, ts2, WUA.df, 0.05)
ifim_plot05 + ylim(c(-50,50))

# now take ts2 Qbaseline compared to ts2 Qout
ts1 <- as.data.frame(model_flows[,c('thisdate', 'Qbaseline')])
ts1$thisdate <- as.character(as.Date(index(model_flows))) 
names(ts1) <- c('Date', 'Flow')

ts1$Flow <- (as.numeric(ts1$Flow)*gage_factor) 
ifim_plot05 <- ifim_wua_change_plot(ts1, ts2, WUA.df, 0.05)
ifim_plot05 + ylim(c(-50,50))
ifim_plot10 <- ifim_wua_change_plot(ts1, ts2, WUA.df, 0.1)
ifim_plot10 + ylim(c(-50,50))
ifim_plot10 <- ifim_wua_change_plot(ts1, ts2, WUA.df, 0.2)
ifim_plot10 + ylim(c(-50,50))



model_flows <- om_get_rundata(elid, 17) # tbd this should come from IFIM model
ts3 <- as.data.frame(model_flows[,c('thisdate', 'Qout')])
ts3$thisdate <- as.character(as.Date(index(model_flows))) 
ts1cc <- sqldf("select * from ts1 where Date <= '2000-09-30' ")
names(ts3) <- c('Date', 'Flow')
ifim_plot10cc <- ifim_wua_change_plot(ts1cc, ts3, WUA.df, 0.1)
ifim_plot10cc + ylim(c(-50,50))


