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
datasite <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
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

hucs = c(
  '0208020604', '0207001004' ,'0601010203' ,'0208020602' ,'0301010103', 
  '0208020305' ,'0208010301' ,'0208010201' ,'0207000409'
);

for (i in 1:length(hucs) ) {
  mydata <- vahydro_fe_data(
    hucs[i], 'erom_q0001e_mean', 'aqbio_nt_total', 
    'watershed', 'nhd_huc10', 'species'
  );
  data <- elf_cleandata(mydata, inputs);
  write.csv(data,paste(hucs[i],".csv", sep=''))
}
