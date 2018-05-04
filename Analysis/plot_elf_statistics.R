
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

plot_monthly_count <- function (data, subname = 'all', xmetric = "out_rsq_adj", ymin = 0.5, ymax = 1.0) {
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