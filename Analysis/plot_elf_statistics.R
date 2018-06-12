erom_vars = c(
  'nhdp_drainage_sqmi', 'erom_q0001e_mean', 'erom_q0001e_jan', 
  'erom_q0001e_feb', 'erom_q0001e_mar', 'erom_q0001e_apr', 
  'erom_q0001e_may', 'erom_q0001e_june', 'erom_q0001e_july', 
  'erom_q0001e_aug', 'erom_q0001e_sept', 'erom_q0001e_oct', 
  'erom_q0001e_nov', 'erom_q0001e_dec'
);

plot_monthly <- function (data, subname = 'all', xmetric = "out_rsq_adj", ymetric = 'nt_total', ymin = 0.5, ymax = 1.0) {
  # dataset_tag: bpj-530, bpj-rcc
  data.da <- subset(data, in_xvar == 'nhdp_drainage_sqmi');
  data.mean <- subset(data, in_xvar == 'erom_q0001e_mean');
  data.jan <- subset(data, in_xvar == 'erom_q0001e_jan');
  data.feb <- subset(data, in_xvar == 'erom_q0001e_feb');
  data.mar <- subset(data, in_xvar == 'erom_q0001e_mar');
  data.apr <- subset(data, in_xvar == 'erom_q0001e_apr');
  data.may <- subset(data, in_xvar == 'erom_q0001e_may');
  data.jun <- subset(data, in_xvar == 'erom_q0001e_june');
  data.jul <- subset(data, in_xvar == 'erom_q0001e_july');
  data.aug <- subset(data, in_xvar == 'erom_q0001e_aug');
  data.sep <- subset(data, in_xvar == 'erom_q0001e_sept');
  data.oct <- subset(data, in_xvar == 'erom_q0001e_oct');
  data.nov <- subset(data, in_xvar == 'erom_q0001e_nov');
  data.dec <- subset(data, in_xvar == 'erom_q0001e_dec');
  n = c('DA', 'Mean', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  title <- paste(ftype, " Q=", 100.*(1.0 - quantile), "%", subname);
  if (length(data.da$s_adminid) > 1) {
    boxplot(
      data.da[,xmetric], data.mean[,xmetric], data.jan[,xmetric], data.feb[,xmetric], data.mar[,xmetric], 
      data.apr[,xmetric], data.may[,xmetric], data.jun[,xmetric], data.jul[,xmetric], data.aug[,xmetric], 
      data.sep[,xmetric], data.oct[,xmetric], data.nov[,xmetric], data.dec[,xmetric],
      names = n, 
      main = title,
      ylab = paste(ymetric, " = f(DA), Qmean,.., Qdec"),
      ylim = c(ymin, ymax)
    );
    
  } else {
    # we are running a single watershed, so use a bar chart
    nms = strsplit(as.vector(data$in_xvar), '_', fixed = TRUE);
    xs = matrix(unlist(nms),ncol=3,byrow=TRUE)
    labs = xs[,3]
    par(las=1) # make label text perpendicular to axis
    barplot(data[,xmetric], names.arg=labs, main=paste("R# for region", selected), 
            xlab="DA/Flow Variable",)
    
  }
}

plot_monthly_count <- function (
  data, subname = 'all', xmetric = "out_rsq_adj", 
  ymin = 0.5, ymax = 1.0) {
  # dataset_tag: bpj-530, bpj-rcc
  data.da <- subset(data, in_xvar == 'nhdp_drainage_sqmi');
  data.mean <- subset(data, in_xvar == 'erom_q0001e_mean');
  data.jan <- subset(data, in_xvar == 'erom_q0001e_jan');
  data.feb <- subset(data, in_xvar == 'erom_q0001e_feb');
  data.mar <- subset(data, in_xvar == 'erom_q0001e_mar');
  data.apr <- subset(data, in_xvar == 'erom_q0001e_apr');
  data.may <- subset(data, in_xvar == 'erom_q0001e_may');
  data.jun <- subset(data, in_xvar == 'erom_q0001e_june');
  data.jul <- subset(data, in_xvar == 'erom_q0001e_july');
  data.aug <- subset(data, in_xvar == 'erom_q0001e_aug');
  data.sep <- subset(data, in_xvar == 'erom_q0001e_sept');
  data.oct <- subset(data, in_xvar == 'erom_q0001e_oct');
  data.nov <- subset(data, in_xvar == 'erom_q0001e_nov');
  data.dec <- subset(data, in_xvar == 'erom_q0001e_dec');
  n = c('DA', 'Mean', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  if (length(data.da$s_adminid) > 1) {
    ns <- cbind(
      'DA' = length(data.da[,xmetric]), 
      'Qmean' = length(data.mean[,xmetric]), 
      'Qjan' = length(data.jan[,xmetric]), 
      'Qfeb' = length(data.feb[,xmetric]),  
      'Qmar' = length(data.mar[,xmetric]),  
      'Qapr' = length(data.apr[,xmetric]),  
      'Qmay' = length(data.may[,xmetric]),  
      'Qjun' = length(data.jun[,xmetric]),  
      'Qjul' = length(data.jul[,xmetric]),  
      'Qaug' = length(data.aug[,xmetric]), 
      'Qsep' = length(data.sep[,xmetric]),  
      'Qoct' = length(data.oct[,xmetric]),  
      'Qnov' = length(data.nov[,xmetric]),  
      'Qdec' = length(data.dec[,xmetric])
    )
    barplot(ns, names.arg=n, main=paste("# of Sig. Relationships", subname), 
            xlab="DA/Flow Variable")
  } 
}

erom_monthly_keyed <- function(data) {
  # returns list of intersecting locations
  data.keyed <- list(
    'da' = subset(data, in_xvar == 'nhdp_drainage_sqmi'),
    'mean' = subset(data, in_xvar == 'erom_q0001e_mean'),
    'jan' = subset(data, in_xvar == 'erom_q0001e_jan'),
    'feb' = subset(data, in_xvar == 'erom_q0001e_feb'),
    'mar' = subset(data, in_xvar == 'erom_q0001e_mar'),
    'apr' = subset(data, in_xvar == 'erom_q0001e_apr'),
    'may' = subset(data, in_xvar == 'erom_q0001e_may'),
    'jun' = subset(data, in_xvar == 'erom_q0001e_june'),
    'jul' = subset(data, in_xvar == 'erom_q0001e_july'),
    'aug' = subset(data, in_xvar == 'erom_q0001e_aug'),
    'sep' = subset(data, in_xvar == 'erom_q0001e_sept'),
    'oct' = subset(data, in_xvar == 'erom_q0001e_oct'),
    'nov' = subset(data, in_xvar == 'erom_q0001e_nov'),
    'dec' = subset(data, in_xvar == 'erom_q0001e_dec')
  );
  return(data.keyed);
}

erom_monthly_frame <- function(data) {
  # returns list of intersecting locations
  data.df <- list(
    'da' = subset(data, in_xvar == 'nhdp_drainage_sqmi'),
    'mean' = subset(data, in_xvar == 'erom_q0001e_mean'),
    'jan' = subset(data, in_xvar == 'erom_q0001e_jan'),
    'feb' = subset(data, in_xvar == 'erom_q0001e_feb'),
    'mar' = subset(data, in_xvar == 'erom_q0001e_mar'),
    'apr' = subset(data, in_xvar == 'erom_q0001e_apr'),
    'may' = subset(data, in_xvar == 'erom_q0001e_may'),
    'jun' = subset(data, in_xvar == 'erom_q0001e_june'),
    'jul' = subset(data, in_xvar == 'erom_q0001e_july'),
    'aug' = subset(data, in_xvar == 'erom_q0001e_aug'),
    'sep' = subset(data, in_xvar == 'erom_q0001e_sept'),
    'oct' = subset(data, in_xvar == 'erom_q0001e_oct'),
    'nov' = subset(data, in_xvar == 'erom_q0001e_nov'),
    'dec' = subset(data, in_xvar == 'erom_q0001e_dec')
  );
  row.names(data.df) <- c(
    'da', 'mean', 'jan', 'feb', 'mar', 'apr', 'may', 
    'june', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'
  );
  return(data.df);
}

# Obtain list of all data with common hydrocodes present
erom_monthly_intersect <- function(data) {
  data.keyed <- erom_monthly_keyed(data);
  all_codes <- list(
    as.list(data.keyed['da']$da['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['mean']$mean['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['jan']$jan['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['feb']$feb['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['mar']$mar['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['apr']$apr['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['may']$may['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['jun']$jun['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['jul']$jul['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['aug']$aug['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['sep']$sep['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['oct']$oct['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['nov']$nov['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['dec']$dec['containing_hydrocode'])$containing_hydrocode
  )
  comlocs <- Reduce(
    intersect, all_codes
  )
  return(comlocs)
}


# Obtain list of  data where da/mean have common hydrocodes present
erom_monthly_intersect <- function(data) {
  data.keyed <- erom_monthly_keyed(data);
  all_codes <- list(
    as.list(data.keyed['da']$da['containing_hydrocode'])$containing_hydrocode,
    as.list(data.keyed['mean']$mean['containing_hydrocode'])$containing_hydrocode
 )
  comlocs <- Reduce(
    intersect, all_codes
  )
  return(comlocs)
}