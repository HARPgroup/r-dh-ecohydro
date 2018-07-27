erom_vars = c(
  'nhdp_drainage_sqmi', 'erom_q0001e_mean', 'erom_q0001e_jan', 
  'erom_q0001e_feb', 'erom_q0001e_mar', 'erom_q0001e_apr', 
  'erom_q0001e_may', 'erom_q0001e_june', 'erom_q0001e_july', 
  'erom_q0001e_aug', 'erom_q0001e_sept', 'erom_q0001e_oct', 
  'erom_q0001e_nov', 'erom_q0001e_dec'
);

elf_quantile_table <- function (data) {
  ss = data.frame(
    "x-Variable" = character(), 
    "0%" =  numeric(), 
    "25%" =  numeric(), 
    "50%" =  numeric(), 
    "75%" =  numeric(), 
    "100%" =  numeric(), 
    "n" = integer(),
    stringsAsFactors = FALSE
  ) ;
  for(i in erom_vars) {
    dset = subset(data, in_xvar == i)
    qq = quantile(dset$out_rsq_adj)
    ss <- rbind(
      ss,
      data.frame(
        "x-Variable" = i,  
        "0%" = as.numeric(qq["0%"]), 
        "25%" = as.numeric(qq["25%"]), 
        "50%" = as.numeric(qq["50%"]), 
        "75%" = as.numeric(qq["75%"]), 
        "100%" = as.numeric(qq["100%"]), 
        "n" = as.integer(length(dset$out_rsq_adj))
      )
    )
  }
  names(ss) <- c("x-Variable", "0%", "25%", "50%", "75%", "100%", "n")
  return(ss)
}

erom_var_readable <- function(varname) {
  lookers = list(
    'nhdp_drainage_sqmi' = 'da', 
    'erom_q0001e_mean' = 'maf', 
    'erom_q0001e_jan' = 'jan', 
    'erom_q0001e_feb' = 'feb', 
    'erom_q0001e_mar' = 'mar', 
    'erom_q0001e_apr' = 'apr', 
    'erom_q0001e_may' = 'may', 
    'erom_q0001e_june' = 'jun', 
    'erom_q0001e_july' = 'jul', 
    'erom_q0001e_aug' = 'aug', 
    'erom_q0001e_sept' = 'sep', 
    'erom_q0001e_oct' = 'oct', 
    'erom_q0001e_nov' = 'nov', 
    'erom_q0001e_dec' = 'dec'
  )
  return(lookers[varname])
}

elf_monthly_summary <- function (
  data, mmin = 0.0, pmin = 0.01, Rmin = 0.0, pmethod = 'text') {
  
  # filter our data?
  data = rawdata; # make a copy to start so we can compare raw to filtered if desired
  tot <- nrow(subset(data, in_xvar == 'nhdp_drainage_sqmi'))
  #data = subset(data, out_n >= 20)
  data = subset(data, out_m > mmin)
  data = subset(data, out_p < pmax); # 
  data = subset(data, out_rsq_adj > Rmin)
  #***********************
  # BEGIN - Intersections
  #***********************
  # Choose One or NONE of the Two Intersections
  # only show watersheds in all sets
  #data = subset(data,containing_hydrocode %in% erom_monthly_intersect(data))
  # OR only show watersheds in all sets
  #data = subset(data,containing_hydrocode %in% erom_daq_intersect(data))
  #***********************
  # END - Intersections
  #***********************
  mtch <- nrow(subset(data, in_xvar == 'nhdp_drainage_sqmi'))
  if (mtch > 0) {
    #data = subset(data, isheadwater > 0)
    # render it
    ymin = 0.0; # restrain plot
    crit_label =  paste('p<', pmax, ',R>', Rmin,sep='')
    labeltext = paste(q_ftype, ":", dataset_tag, '\n', ' (', crit_label, mtch, ' of ', tot,' )',sep='')
    plot_monthly(data, labeltext, xmetric = 'out_rsq_adj', ymetric = metric, ymin=ymin, title = title);
    mmax = ceiling(max(data$out_m))
    #plot_monthly(data, labeltext, xmetric = 'out_m', ymetric = metric, ymin=0, ymax=mmax);
    plot_monthly_count (data, subname = dataset_tag, xmetric = "out_rsq_adj", 
                        ymin = 0.0, ymax = 1.0)
    
    # Show variation in slope
    ymin = 0.0; # restrain plot
    labeltext = paste(q_ftype, ":", dataset_tag, '\n', ' ( p<', pmax, 'R>', Rmin, ' ', mtch, ' of ', tot,' )',sep='')
    mmax = ceiling(max(data$out_m))
    #  plot_monthly(data, labeltext, xmetric = 'out_m', ymetric = metric, ymin=ymin, ymax=mmax);
    
    #plot_monthly(data, labeltext, xmetric = 'out_m', ymetric = metric, ymin=0, ymax=mmax);
    
    # add to summary table
    meds <- aggregate(out_rsq_adj ~ in_xvar, data = data, median)
    mmx = meds[which.max(meds$out_rsq_adj),];
    mmxvar = mmx$in_xvar;
    ps = 'n/a';
    ps25 = 'n/a';
    if (mmxvar != 'nhdp_drainage_sqmi') {
      daq = subset(
        data, 
        in_xvar == 'nhdp_drainage_sqmi' | in_xvar == mmxvar
      )
      fit <- aov(out_rsq_adj ~ in_xvar, data=daq)
      sfit = summary(fit) 
      usfit = unlist(sfit)
      p = as.numeric(usfit["Pr(>F)1"])
      ps = elf_format_p(p, meth = pmethod)
    }
    # ****************************************
    # ** Analyze x with best 25th percentile
    # ** disabled since the aggregate call fails now?
    # ****************************************
    # Error is:
    # Error in get(as.character(FUN), mode = "function", envir = envir) : 
    # object 'FUN' of mode 'function' was not found
    #quants <- aggregate(out_rsq_adj ~ in_xvar, data = data, quantile)
    #q25mx = quants[which.max(quants$out_rsq_adj[,"25%"]),]
    #q25mxvar = q25mx$in_xvar;
    #if (q25mxvar != 'nhdp_drainage_sqmi' & FALSE) {
    #  daq = subset(
    #    data, 
    #    in_xvar == 'nhdp_drainage_sqmi' | in_xvar == q25mxvar
    #  )
    #  fit <- aov(out_rsq_adj ~ in_xvar, data=daq)
    #  sfit = summary(fit) 
    #  usfit = unlist(sfit)
    #  p = as.numeric(usfit["Pr(>F)1"])
    #  ps25 = elf_format_p(p, meth = pmethod)
    #}
    # ****************************************
    # ** END -- Analyze x with best 25th percentile
    # ****************************************
    sline <- data.frame(
      "Biometric" = metric, 
      'Med R(all/DA/Qmean)' = 
        paste(
          round(median(data$out_rsq_adj),2), 
          round(median(subset(data, in_xvar == 'nhdp_drainage_sqmi')$out_rsq_adj),2),
          round(median(subset(data, in_xvar == 'erom_q0001e_mean')$out_rsq_adj),2),
          sep="/"
        ),
      "Best" = paste(erom_var_readable(as.character(mmxvar)), round(mmx$out_rsq_adj,2),ps,sep=':'),
      #   "L25" = paste(erom_var_readable(as.character(q25mxvar)), round(q25mx$out_rsq_adj[,"25%"],2),ps25,sep=':'),
      "Count (DA)" = mtch, 
      stringsAsFactors = FALSE
    );
  }
  return (sline)
}
  
  
elf_monthly_batch_summary <- function (
  batch, mmin = 0.0, pmin = 0.01, Rmin = 0.0, pmethod = 'text') {
  # Summary Stats
  ss = data.frame(
    "Biometric" = character(), 
    'Med R(all/DA/Qmean)' =  numeric(),  
    "Best" = character(), 
   # "L25" = character(), 
    "Count" = character(), 
    stringsAsFactors = FALSE) ;
  
  for (row in 1:nrow(batch)) {
    bundle <- batch[row,]$bundle;
    ftype <- batch[row,]$ftype;
    title <- batch[row,]$title;
    dataset_tag <- batch[row,]$dataset_tag;
    q_ftype <- batch[row,]$q_ftype;
    metric <- batch[row,]$metric;
    rawdata = fn_dh_elfstats (
      ftype = q_ftype,
      feature_ftype = ftype,
      yvar = metric,
      dataset_tag = dataset_tag
    );
    
    
    # filter our data?
    data = rawdata; # make a copy to start so we can compare raw to filtered if desired
    tot <- nrow(subset(data, in_xvar == 'nhdp_drainage_sqmi'))
    #data = subset(data, out_n >= 20)
    data = subset(data, out_m > mmin)
    data = subset(data, out_p < pmax); # 
    data = subset(data, out_rsq_adj > Rmin)
    #***********************
    # BEGIN - Intersections
    #***********************
    # Choose One or NONE of the Two Intersections
    # only show watersheds in all sets
    #data = subset(data,containing_hydrocode %in% erom_monthly_intersect(data))
    # OR only show watersheds in all sets
    #data = subset(data,containing_hydrocode %in% erom_daq_intersect(data))
    #***********************
    # END - Intersections
    #***********************
    mtch <- nrow(subset(data, in_xvar == 'nhdp_drainage_sqmi'))
    if (mtch > 0) {
      #data = subset(data, isheadwater > 0)
      # render it
      ymin = 0.0; # restrain plot
      crit_label =  paste('p<', pmax, ',R>', Rmin,sep='')
      labeltext = paste(q_ftype, ":", dataset_tag, '\n', ' (', crit_label, mtch, ' of ', tot,' )',sep='')
      plot_monthly(data, labeltext, xmetric = 'out_rsq_adj', ymetric = metric, ymin=ymin, title = title);
      mmax = ceiling(max(data$out_m))
      #plot_monthly(data, labeltext, xmetric = 'out_m', ymetric = metric, ymin=0, ymax=mmax);
      plot_monthly_count (data, subname = dataset_tag, xmetric = "out_rsq_adj", 
                          ymin = 0.0, ymax = 1.0)
      
      # Show variation in slope
      ymin = 0.0; # restrain plot
      labeltext = paste(q_ftype, ":", dataset_tag, '\n', ' ( p<', pmax, 'R>', Rmin, ' ', mtch, ' of ', tot,' )',sep='')
      mmax = ceiling(max(data$out_m))
      #  plot_monthly(data, labeltext, xmetric = 'out_m', ymetric = metric, ymin=ymin, ymax=mmax);
      
      #plot_monthly(data, labeltext, xmetric = 'out_m', ymetric = metric, ymin=0, ymax=mmax);
      
      # add to summary table
      meds <- aggregate(out_rsq_adj ~ in_xvar, data = data, median)
      mmx = meds[which.max(meds$out_rsq_adj),];
      mmxvar = mmx$in_xvar;
      ps = 'n/a';
      ps25 = 'n/a';
      if (mmxvar != 'nhdp_drainage_sqmi') {
        daq = subset(
          data, 
          in_xvar == 'nhdp_drainage_sqmi' | in_xvar == mmxvar
        )
        fit <- aov(out_rsq_adj ~ in_xvar, data=daq)
        sfit = summary(fit) 
        usfit = unlist(sfit)
        p = as.numeric(usfit["Pr(>F)1"])
        ps = elf_format_p(p, meth = pmethod)
      }
      # ****************************************
      # ** Analyze x with best 25th percentile
      # ** disabled since the aggregate call fails now?
      # ****************************************
      # Error is:
      # Error in get(as.character(FUN), mode = "function", envir = envir) : 
      # object 'FUN' of mode 'function' was not found
      #quants <- aggregate(out_rsq_adj ~ in_xvar, data = data, quantile)
      #q25mx = quants[which.max(quants$out_rsq_adj[,"25%"]),]
      #q25mxvar = q25mx$in_xvar;
      #if (q25mxvar != 'nhdp_drainage_sqmi' & FALSE) {
      #  daq = subset(
      #    data, 
      #    in_xvar == 'nhdp_drainage_sqmi' | in_xvar == q25mxvar
      #  )
      #  fit <- aov(out_rsq_adj ~ in_xvar, data=daq)
      #  sfit = summary(fit) 
      #  usfit = unlist(sfit)
      #  p = as.numeric(usfit["Pr(>F)1"])
      #  ps25 = elf_format_p(p, meth = pmethod)
      #}
      # ****************************************
      # ** END -- Analyze x with best 25th percentile
      # ****************************************
      ss <- rbind(
        ss, 
        data.frame(
          "Biometric" = metric, 
          'Med R(all/DA/Qmean)' = 
            paste(
              round(median(data$out_rsq_adj),2), 
              round(median(subset(data, in_xvar == 'nhdp_drainage_sqmi')$out_rsq_adj),2),
              round(median(subset(data, in_xvar == 'erom_q0001e_mean')$out_rsq_adj),2),
              sep="/"
            ),
          "Best" = paste(erom_var_readable(as.character(mmxvar)), round(mmx$out_rsq_adj,2),ps,sep=':'),
       #   "L25" = paste(erom_var_readable(as.character(q25mxvar)), round(q25mx$out_rsq_adj[,"25%"],2),ps25,sep=':'),
          "Count (DA)" = mtch, 
          stringsAsFactors = FALSE)
      );
    }
    
  }
  #names(ss) <- c("Biometric",'Med R(all/DA/Qmean)','Best', ">L25%","Count");
  names(ss) <- c("Biometric",'Med R(all/DA/Qmean)','Best', "Count");
  return (ss)
}

elf_format_p <- function(p, meth = 'text', digits = 3) {
  ps = 'ns'
  if (p <= 0.15) {
    ps = '.'
  }
  if (p <= 0.1) {
    ps = '..'
  }
  if (p <= 0.05) {
    ps = '*'
  }
  if (p <= 0.01) {
    ps = '**'
  }
  if (p <= 0.001) {
    ps = '***'
  }
  if (meth == 'numeric') {
    ps = round(p, digits)
    if ((ps == 0) & (p > ps)) {
      # give a < 0.00 to digits result
      ps = paste(str_pad("<0.", 3 + (digits - 1), side = "right", pad = "0"), "1", sep='')
    }
  }
  return(ps)
}

plot_monthly <- function (
    data, subname = 'all', xmetric = "out_rsq_adj", 
    ymetric = 'nt_total', ymin = 0.5, ymax = 1.0, title = "NT"
  ) {
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
  yvar = as.character(data[1,]$in_yvar)
  quantile = as.numeric(data[1,]$in_quantile)
  n = c('DA', 'Mean', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  #title <- paste(yvar, " Q=", 100.*(1.0 - quantile), "%", subname);
  if (length(data.da$s_adminid) > 1) {
    boxplot(
      data.da[,xmetric], data.mean[,xmetric], data.jan[,xmetric], data.feb[,xmetric], data.mar[,xmetric], 
      data.apr[,xmetric], data.may[,xmetric], data.jun[,xmetric], data.jul[,xmetric], data.aug[,xmetric], 
      data.sep[,xmetric], data.oct[,xmetric], data.nov[,xmetric], data.dec[,xmetric],
      names = n, 
      main = title,
      ylab = expression( R^2 ~ of ~ NT ~ as ~ f(DA,Qmean,Qjan...)),
      xlab = "DA, Qmean and Monthly EROM Flow",
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