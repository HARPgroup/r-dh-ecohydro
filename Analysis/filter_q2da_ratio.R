rm(list = ls())  #clear variables
options(timeout=2400); # set timeout to twice default level to avoid abort due to high traffic

library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);
#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
#----FOR RUNNING LOCALLY:
# Only need to change 1 path here - set to wherever this code is located
basepath='C:\\usr\\local\\home\\git\\r-dh-ecohydro';

# set your local directory paths in config.local.private located in filepath above
# this file will NOT be sent to git, so it should persist
# so, edit config.local.private once and you should be good to go
source(paste(basepath,'config.local.private',sep='/'));
# all these rely on variables set in config.local.private
source(paste(fxn_vahydro,"rest_functions.R", sep = "/"));
source(paste(auth_rest,"rest.private", sep="/"));
source(paste(fxn_locations,"ELFGEN/internal/elf_quantreg.R", sep = "/"));
source(paste(fxn_locations,"ELFGEN/internal/elf_pct_chg.R", sep = "/"));
source(paste(fxn_locations,"ELFGEN/internal/elf_pw_it.R", sep = "/"));
source(paste(fxn_locations,"ELFGEN/internal/elf_assemble_batch.R", sep = "/"));

token <- rest_token(site, token, rest_uname, rest_pw);
#####
inputs <- list(
  site = site,
  pct_chg = 10,                             #Percent decrease in flow for barplots (keep at 10 for now)
  save_directory = save_directory, 
  x_metric = 'erom_q0001e_mean', #Flow metric to be plotted on the x-axis
  y_metric = 'aqbio_nt_total',	   #Biometric to be plotted on the y-axis, see "dh variable key" column for options: https://docs.google.com/spreadsheets/d/1PnxY4Rxfk9hkuaXy7tv8yl-mPyJuHdQhrOUWx_E1Y_w/edit#gid=0
  ws_ftype = c('nhd_huc10'),		     #Options: state, hwi_region, nhd_huc8, nhd_huc6, ecoregion_iii, ecoregion_iv, ecoiii_huc6
  target_hydrocode = 'usa_state_virginia',           #Leave blank to process all, individual examples: usa_state_virginia for all of VA, atl_non_coastal_plain_usgs,ohio_river_basin_nhdplus,nhd_huc8_05050001...
  quantile = .80,                  #Specify the quantile to use for quantile regresion plots 
  xaxis_thresh = 15000,            #Leave at 15000 so all plots have idential axis limits 
  analysis_timespan = 'full',      #used to plot for entire timespan 
  send_to_rest = "NO",            #"YES" to push ELF statistic outputs to VAHydro
  station_agg = "max",             #Specify aggregation to only use the "max" NT value for each station or "all" NT values
  sampres = 'species',                  
  glo = 1,  
  ghi = 530,
  dataset_tag = 'ymax75',
  token = token,
  startdate = '1900-01-01',
  enddate = '2100-01-01'
);

##### Data Acquisition #####
#retrieve raw data
mydata <- vahydro_fe_data(
  'usa_state_virginia', "erom_q0001e_mean", "aqbio_nt_total", 
  'landunit',  "state", "species"
);
data <- elf_cleandata(mydata, inputs);

#for setting ghi = max x_value
inputs$ghi <- max(mydata$x_value);


##### Plot PWIT ####
# modify elf_pwit to do analysis and return results
elf_pw_it (
  inputs, data, inputs$x_metric, 
  inputs$y_metric, ws_ftype_code = NULL, 
  'usa_state_virginia_qda_lt1000', 'usa_state_virginia_qda_lt1000', 
  'usa_state_virginia_qda_lt1000', token, 
  min(data$tstime), max(data$tstime)
)

# Now, filter out those that have da : q ratio > 10

data <- subset(data, ratio <= 10);
elf_pw_it (
  inputs, data, inputs$x_metric, 
  inputs$y_metric, ws_ftype_code = NULL, 
  'usa_state_virginia_qda_lt10', 'usa_state_virginia_qda_lt10', 
  'usa_state_virginia_qda_lt10', token, 
  min(data$tstime), max(data$tstime)
)
