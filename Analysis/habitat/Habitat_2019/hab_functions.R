# VARIOUS HABITAT FUNCTIONS


#' Remove non fish metrics from dataframe 
#' @param all_sites dataframe of IFIM Sites and habitat pctchg
#' @return datframe 
rm.benth <- function(all_sites) {
  all_sites <- all_sites[-which(all_sites$metric=="alg_mid"),]        # Algae and Midge Guild
  all_sites <- all_sites[-which(all_sites$metric=="bd_high_grad"),]   # Benthos Diversity, high gradient
  all_sites <- all_sites[-which(all_sites$metric=="bd_low_grad"),]    # Benthos Diversity, low gradient
  all_sites <- all_sites[-which(all_sites$metric=="benth_mac"),]      # Benthic Macroinvertebrates
  all_sites <- all_sites[-which(all_sites$metric=="cf"),]             # Crayfish
  all_sites <- all_sites[-which(all_sites$metric=="e_comp"),]         # Eastern Elliptio mussel, Elliptio complanata
  all_sites <- all_sites[-which(all_sites$metric=="eph_mac"),]        # Mayfly
  all_sites <- all_sites[-which(all_sites$metric=="l_rad"),]          # Eastern lampmussel
  all_sites <- all_sites[-which(all_sites$metric=="plec_mac"),]       # Stonefly
  all_sites <- all_sites[-which(all_sites$metric=="pwb"),]            # Purple Wartyback mussel
  all_sites <- all_sites[-which(all_sites$metric=="sm_shal_slow"),]   # Spike Mussel, Shallow
  all_sites <- all_sites[-which(all_sites$metric=="sm_int"),]         # Spike Mussel, Intermediate
  all_sites <- all_sites[-which(all_sites$metric=="tric_mac"),]       # Caddisfly
  return(all_sites)
}


#' Subset dataframe for a single month of interest
#' @param all_sites dataframe of IFIM Sites and habitat pctchg
#' @param month character for month of interest
#' @return datframe 
month.subset <- function(all_sites,month) {
  month_all_sites <- all_sites[all_sites$flow %in% month,]
  month_all_sites <- na.omit(month_all_sites) #remove NA rows prior to calculating medians
  return(month_all_sites)
}


#' Aggregate dataframe percent changes by monthly median at each site
#' @param month_all_sites dataframe for single month of interest
#' @return datframe 
agg.median <- function(month_all_sites) {
  month_all_sites <- aggregate(list(pctchg = month_all_sites$pctchg), list(ifim_da_sqmi = month_all_sites$ifim_da_sqmi), median)
  return(month_all_sites)
}


#' Generate dataframe with IFIM site names, site DA and site MAF
#' @param month_all_sites dataframe for single month of interest
#' @return datframe 
site.stats <- function(month_all_sites) {
  month_all_sites <- month_all_sites[order(month_all_sites$ifim_da_sqmi),] 
  month_all_sites <- month_all_sites[!duplicated(month_all_sites$ifim_site_name), ]
  month_all_sites <- month_all_sites[,-6]  #remove last column
  month_all_sites <- month_all_sites[,-5]  #remove last column
  month_all_sites <- month_all_sites[,-4]  #remove last column 
  return(month_all_sites)
}


 
