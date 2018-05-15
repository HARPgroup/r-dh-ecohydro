#User inputs 
inputs <- list(
  site = site,
  datasite = datasite,
  offset_x_metric = 1,                      #Leave at 1 to start from begining of x_metric for-loop
  offset_y_metric = 1,                      #Leave at 1 to start from begining of y_metric for-loop
  offset_ws_ftype = 1,                      #Leave at 1 to start from begining of ws_ftype for-loop
  offset_hydrocode = 1,                     #Leave at 1 to start from begining of Watershed_Hydrocode for-loop
  pct_chg = 10,                             #Percent decrease in flow for barplots (keep at 10 for now)
  save_directory = save_directory, 
  x_metric = c(
    'nhdp_drainage_sqmi',
    'erom_q0001e_mean',
    'erom_q0001e_jan',
    'erom_q0001e_feb',
    'erom_q0001e_mar', 
    'erom_q0001e_apr', 
    'erom_q0001e_may',
    'erom_q0001e_june',
    'erom_q0001e_july',
    'erom_q0001e_aug',
    'erom_q0001e_sept',
    'erom_q0001e_oct',
    'erom_q0001e_nov',
    'erom_q0001e_dec'
  ),		
  not_x_metric = 'erom_q0001e_mean', #Flow metric to be plotted on the x-axis
  not_y_metric = c(
               'nhdp_drainage_sqmi',
               'aqbio_nt_bival',
               'aqbio_nt_cypr_native'
              ), #this can be used to process by multiple biometrics at once 
  y_metric = 'aqbio_nt_total',	   #Biometric to be plotted on the y-axis, see "dh variable key" column for options: https://docs.google.com/spreadsheets/d/1PnxY4Rxfk9hkuaXy7tv8yl-mPyJuHdQhrOUWx_E1Y_w/edit#gid=0
  not_ws_ftype = c(
    'state',
    'hwi_region',
    'nhd_huc6',
    'nhd_huc8',
    'nhd_huc10',
    'nhd_huc12',
    'ecoregion_iii',
    'ecoregion_iv',
    'ecoiii_huc6'
  ),#this can be used to process by multiple region types at once 
  ws_ftype = c('nhd_huc8'),		     #Options: state, hwi_region, nhd_huc8, nhd_huc6, ecoregion_iii, ecoregion_iv, ecoiii_huc6
  bundle = 'watershed',
  target_hydrocode = '',
  #target_hydrocode = atl_new, 
  quantile = .80,                  #Specify the quantile to use for quantile regresion plots 
  xaxis_thresh = 15000,            #Leave at 15000 so all plots have idential axis limits 
  #analysis_timespan = '1990-2000',#used to subset data on date range 
  analysis_timespan = 'full',      #used to plot for entire timespan 

  send_to_rest = "YES",            #"YES" to push ELF statistic outputs to VAHydro
  station_agg = "max",             #Specify aggregation to only use the "max" NT value for each station or "all" NT values
  sampres = 'species',                  
  #sampres = 'maj_fam_gen_spec',                  
                                  #--Sample Resolution Grouping Options 
                                   #   species...............Species taxanomic level (Fish metrics only)
                                   #   maj_fam_gen_spec......majority a mix of family/genus/species (Benthics only)
                                   #   maj_fam_gen...........majority a mix of family/genus (Benthics only)
                                   #   maj_fam...............majority family (Benthics only)
                                   #   maj_species...........majority species (Benthics only)
  
  method = "quantreg", #quantreg, pwit, ymax, twopoint, pwit_RS
  glo = 1,   # PWIT Breakpoint lower guess (sqmi/cfs)
  ghi = 408, # PWIT Breakpoint upper guess (sqmi/cfs) - also used as DA or MAF breakpoint for elf_quantreg method 
  ghi_var = '',  #qmean_annual or drainage_area_sqmi; use '' for default to have fn choose DA or Qmean
  token = '',
  dataset_tag = "none"
) 
