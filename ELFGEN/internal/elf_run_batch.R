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
source(paste(fxn_locations,"elf_assemble_batch.R", sep = ""));
source(paste(fxn_locations,"elf_quantreg.R", sep = ""));
source(paste(fxn_locations,"elf_ymax.R", sep = ""));
source(paste(fxn_locations,"elf_pw_it.R", sep = ""));
source(paste(fxn_locations,"elf_pct_chg.R", sep = ""));
source(paste(fxn_locations,"elf_store_data.R", sep = ""));
source(paste(base_directory,"Analysis/query_elf_statistics.R", sep = "/")); 
#####
# Now add custom local settings here
inputs$x_metric = 'erom_q0001e_mean'; #Flow metric to be plotted on the x-axis
inputs$y_metric = 'aqbio_nt_total';
inputs$ws_ftype = c('nhd_huc8');
inputs$target_hydrocode = '';
inputs$quantile = .80;
inputs$send_to_rest = "YES";
inputs$glo = 1;
inputs$ghi = 530;
inputs$method = "ymax"; #quantreg, pwit, ymax, twopoint, pwit_RS
inputs$dataset_tag = 'ymax75';
inputs$token = token;

#------------------------------------------------------------------------------------------------
# 1. Get data list - expects header line format with at least target_hydrocode
#    and optional any of the following
# target_hydrocode,name,ghi,glo,
batchlist = elf_assemble_batch(inputs) 
# or
batchlist = read.csv(file=paste(fxn_locations,"Huc8_MAFw_Huc6BP_ForQuantreg.csv",sep="/"),header=TRUE)
# 2. Iterate through each item in the list
for (row in 1:nrow(batchlist)) {
  tin = inputs
  target <- batchlist[row,];
  # 2.1 Check for custom inputs in list
  eligible <- c(
    'glo', 'ghi', 'target_hydrocode', 'x_metric_code', 'y_metric_code', 'startdate', 'enddate',
    'bundle', 'ws_ftype', 'name', 'method', 'sampres', 'dataset_tag'
    )
  for (col in eligible) {
    if (col %in% colnames(target)) {
      tin[col] <- target[col]
    }
  }
  print(tin$target_hydrocode)
  # get the raw data
  mydata <- vahydro_fe_data(
    Watershed_Hydrocode = tin$target_hydrocode, x_metric_code = tin$x_metric, 
    y_metric_code = tin$y_metric, bundle = tin$bundle,  
    ws_ftype_code = tin$ws_ftype, sampres = tin$sampres
  );
  # filter out stuff we don't want (can be controlled via tin)
  data <- elf_cleandata(mydata, inputs = tin);
  # 2.2 Run selected routine 
  if (!(is.data.frame(data))) {
    print("No Data Found") 
  } else {
    if (is.null(tin$hydroid)) {
      feature <- getFeature(
        list(
          ftype = tin$ws_ftype,
          bundle = tin$bundle,
          hydrocode = tin$target_hydrocode
        )
        , token, base_url);
      tin$name = feature$name
      tin$hydroid = feature$hydroid
    }
    if (is.null(tin$name)) {
      tin$name = tin$target_hydrocode
    }
    startdate = min(data$tstime)
    enddate = max(data$tstime)
    elf_run_method(
      method = tin$method, inputs = tin, data = data, 
      x_metric_code = as.character(tin$x_metric), y_metric_code = as.character(tin$y_metric), 
      ws_ftype_code = tin$ws_ftype, Feature.Name_code = tin$name, 
      Hydroid_code = tin$hydroid, search_code = tin$target_hydrocode, 
      token = token, startdate = startdate, enddate = enddate
    )
  }
}
##############################################################################
