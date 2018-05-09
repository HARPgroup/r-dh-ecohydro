rm(list = ls())  #clear variables
options(timeout=240); # set timeout to twice default level to avoid abort due to high traffic
 
#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
datasite <- "http://deq2.bse.vt.edu/d.dh" # where to get the raw data to analyze
#----------------------------------------------

#----Change Basepath here to point to your global config file:
basepath='/var/www/R';
#basepath='C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\r-dh-ecohydro\\';
# set your local directory paths in config.local.private located in filepath above
# this file will NOT be sent to git, so it should persist
# so, edit config.local.private once and you should be good to go
source('/var/www/R/config.local.private');

#Load Functions               
source(paste(fxn_locations,"elf_retrieve_data.R", sep = ""));  #loads function used to retrieve F:E data from VAHydro
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
rest_uname = FALSE;
rest_pw = FALSE;
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token(site, token, rest_uname, rest_pw);

source(paste(fxn_locations,"huc8_groupings.txt", sep = "")); 

# Load Default inputs
source(paste(fxn_locations,"elf_default_inputs.R", sep = ""));
#####
# Now add custom local settings here
inputs$x_metric = 'erom_q0001e_mean'; #Flow metric to be plotted on the x-axis
inputs$y_metric = 'aqbio_nt_total';
inputs$ws_ftype = c('nhd_huc10');
inputs$target_hydrocode = '';
inputs$quantile = .80;
inputs$send_to_rest = "NO";
inputs$glo = 1;
inputs$method = "ymax"; #quantreg, pwit, ymax, twopoint, pwit_RS
inputs$ghi = 530;
inputs$offset_hydrocode = 481
inputs$dataset_tag = 'ymax75';
inputs$token = token;

#------------------------------------------------------------------------------------------------
elf_retrieve_data (inputs) 

##############################################################################
