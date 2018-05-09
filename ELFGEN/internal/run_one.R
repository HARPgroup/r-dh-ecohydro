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
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
#----FOR RUNNING LOCALLY:
# Only need to change 1 path here - set to wherever this code is located
basepath='/var/www/R';

# set your local directory paths in config.local.private located in filepath above
# this file will NOT be sent to git, so it should persist
# so, edit config.local.private once and you should be good to go
source(paste(basepath,'config.local.private',sep='/'));
# all these rely on variables set in config.local.private
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
source(paste(fxn_locations,"elf_quantreg.R", sep = ""));
source(paste(fxn_locations,"elf_pct_chg.R", sep = ""));
source(paste(fxn_locations,"elf_pw_it.R", sep = ""));
source(paste(fxn_locations,"elf_ymax.R", sep = ""));
source(paste(fxn_locations,"elf_assemble_batch.R", sep = ""));

token <- rest_token(site, token, rest_uname, rest_pw);

# Load Default inputs
source(paste(fxn_locations,"elf_default_inputs.R", sep = ""));
#####
inputs$x_metric = 'erom_q0001e_mean'; #Flow metric to be plotted on the x-axis
inputs$y_metric = 'aqbio_nt_total';
inputs$ws_ftype = c('nhd_huc10');
inputs$target_hydrocode = 'usa_state_virginia';
inputs$quantile = .80;
inputs$send_to_rest = "NO";
inputs$glo = 1;
inputs$ghi = 530;
inputs$dataset_tag = 'ymax75';
inputs$token = token;


##### Data Acquisition #####
#retrieve raw data
mydata <- vahydro_fe_data(
  'usa_state_virginia', "erom_q0001e_mean", "aqbio_nt_total", 
  'landunit',  "state", "species"
);
data <- elf_cleandata(mydata, inputs);
# do ymax calcs and plot
elf_ymax(
  inputs, data, x_metric_code = "erom_q0001e_mean", 
  y_metric_code = "aqbio_nt_total", ws_ftype_code = 'landunit', 
  Feature.Name_code = 'Virginia', Hydroid_code = 'usa_state_virginia', 
  search_code = 'usa_state_virginia', token, 
  startdate = min(data$tstime), enddate = max(data$tstime))

#perform quantile regression calculation and plot 
elf_quantreg(
 inputs, data, x_metric_code = inputs$x_metric, 
 y_metric_code = inputs$y_metric, 
 ws_ftype_code = NULL, 
 Feature.Name_code = 'Roanoke ELF', 
 Hydroid_code = NULL, 
 search_code = NULL, token, 
 min(data$tstime), max(data$tstime)
)
plot(log(data$x_value), data$y_value)
reg <- lm(y_value~log(x_value),data)
abline(lm(y_value~log(x_value),data))
lines(log(data$x_value[order(data$x_value)]), loess(y_value~log(x_value),data)$fitted[order(data$x_value)],col="blue",lwd=3)
lines(table$x[order(table$x)],(loess(as.character(pct_chg_10) ~ x,data=table))$fitted[order(table$x)],col="blue",lwd=3)

summary(reg)

#for setting ghi = max x_value
inputs$ghi <- max(mydata$x_value);


##### Plot PWIT ####
# modify elf_pwit to do analysis and return results
elf_pw_it (
  inputs, data, inputs$x_metric, 
  inputs$y_metric, ws_ftype_code = NULL, 
  'usa_state_virginia', 'usa_state_virginia', 
  'usa_state_virginia', token, 
  min(data$tstime), max(data$tstime)
)
# add new function
# plot_elf_pwit()
# add new function store_elf_pwit() (if SEND_TO_REST = TRUE)
