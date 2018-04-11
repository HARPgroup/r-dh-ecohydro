rm(list = ls())  #clear variables
options(timeout=240); # set timeout to twice default level to avoid abort due to high traffic

#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------

#----FOR RUNNING LOCALLY:
basepath='C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\r-dh-ecohydro\\ELFGEN\\internal\\';
source(paste(basepath,'config.local.private',sep='/'));

source(paste(fxn_vahydro,"rest_functions.R", sep = "")); 
rest_uname = FALSE;
rest_pw = FALSE;
source(paste(fxn_locations,"rest.private", sep = ""));         #load rest username and password, contained in rest.private file
token <- rest_token(site, token, rest_uname, rest_pw);

#-------------------------------------------------------------------------------
#'NF_Shenandoah'      transects = 397290,397291,397292,397293,397294
#'SF_Shenandoah'      transects = 397299,397300,397301
#'Upper_James'        transects = 397302,397303,397304,397305
#'New_River_Claytor'  transects = 397284,397285
#'Appomattox'         transects = 397282,397283
#'Potomac'            transects = 397295,397296
#'North_Anna'         transects = 397286,397287,397288,397289
#'Roanoke'            transects = 397297,397298

ifim_featureid <- 397290

#-------------------------------------------------------------------------------
# RETRIEVE SITE DATA
ifim_dataframe <- vahydro_prop_matrix(ifim_featureid,'ifim_habitat_table')
ifim_site_name <- as.character((getFeature(list(hydroid = ifim_featureid), token, site, feature))$name)

#Properties from NHDPlusV2
ifim_da_sqmi <- round(as.numeric(as.character((getProperty(list(featureid = ifim_featureid,varkey = 'nhdp_drainage_sqmi',entity_type = 'dh_feature'),site, prop))$propvalue)),1)
ifim_da_maf <- round(as.numeric(as.character((getProperty(list(featureid = ifim_featureid,varkey = 'erom_q0001e_mean',entity_type = 'dh_feature'),site, prop))$propvalue)),1)

#-------------------------------------------------------------------------------
