library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);

elf_pw_it_RS <- function(inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate, geom){
  
  a = FALSE
  
  #Load inputs
  x_metric <- x_metric_code
  y_metric <- y_metric_code
  Feature.Name <- Feature.Name_code
  Hydroid <- Hydroid_code
  ws_ftype <- ws_ftype_code
  pct_chg <- inputs$pct_chg 
  save_directory <- inputs$save_directory 
  target_hydrocode <- inputs$target_hydrocode
  quantile <- inputs$quantile  
  xaxis_thresh <- inputs$xaxis_thresh
  send_to_rest <- inputs$send_to_rest
  offset <- inputs$offset
  analysis_timespan <- inputs$analysis_timespan
  #startdate <- inputs$startdate
  #enddate <- inputs$enddate
  station_agg <- inputs$station_agg
  site <- inputs$site
  sampres <- inputs$sampres
  glo <- inputs$glo
  ghi <- inputs$ghi
  dataset_tag <- inputs$dataset_tag
  
  full_dataset <- data

  #must round these boundary values so they fit in admincode 
  glo <- round(glo,digits=0)
  ghi <- round(ghi,digits=0)
  
  #Creates subset of data consisting of only the upper x% of the datapoints
  ##The piecewise function then looks for a breakpoint between the user specified bounding values using only these upper points 
  upper_points <- rq(y_value ~ log(x_value),data = full_dataset, tau = quantile)
  upper_points_newy <- c(log(full_dataset$x_value)*coef(upper_points)[2]+coef(upper_points)[1])
  upper_points <- subset(full_dataset, full_dataset$y_value > upper_points_newy)
 
  x <- upper_points$x_value
  y <- upper_points$y_value
  
  #set initial guess range
  breaks <- x[which(x >= glo & x <= ghi)]
  as.numeric(breaks)
  #print(breaks)
  
  #This is necessary in case no breaks are found
  if(length(breaks) != 0) {
  #Needed in case pwit function only locates a single break in the data    
    if(length(breaks) == 1) {
      breakpt <- breaks
    }else{
      
    #mse <- numeric(length(breaks))
    mse <- as.numeric(length(breaks))

    for(n in 1:length(breaks)){
      piecewise1 <- lm(y ~ log(x)*(x < breaks[n]) + log(x)*(x >= breaks[n]))
      mse[n] <- summary(piecewise1)[6]
    }
    mse <- as.numeric(mse)
    #remove any breaks that are NaN
    mse[is.na(mse)] <- 100000
    breakpt <- breaks[which(mse==min(mse))]
    breakpt <- breakpt[1]
    } #end of breaks == 1 loop 
  
  #Right Side of Breakpoint  
  #---------------------------------------------------------------------------------
    
  data_RS <-data[(data$x_value >  breakpt),]
  #print(head(data_RS))
  
  #If statement needed in case there are fewer than 4 datapoints to the right of x-axis inflection point, or if there are more than 3 points but all have the same x_value
  duplicates_RS <- unique(data_RS$x_value, incomparables = FALSE)
  if(nrow(data_RS) && length(duplicates_RS) > 3) {   
  
  upquant_RS <- rq(y_value ~log(x_value),data = data_RS, tau = quantile)
  newy_RS <- c(log(data_RS$x_value)*coef(upquant_RS)[2]+coef(upquant_RS)[1])
  upper.quant_RS <- subset(data_RS, data_RS$y_value > newy_RS)
  print(paste("Upper quantile to the right of breakpoint has ", nrow(upper.quant_RS), "values"));
 
  #If statement needed in case there are fewer than 4 datapoints in upper quantile on the right side of the breakpoint
  if (nrow(upper.quant_RS) > 3) {
    
  regupper_RS <- lm(y_value ~ log(x_value),data = upper.quant_RS)  
  ru_RS <- summary(regupper_RS) #regression for upper quantile
  
  #If statement needed in case slope to right of breakpoint is "NA"
  if (nrow(ru_RS$coefficients) > 1) {
  
  regfull_RS <- lm(y_value ~ log(x_value),data = data_RS)            
  rf_RS <- summary(regfull_RS)                                                   
  rfint_RS <- round(rf_RS$coefficients[1,2], digits = 6)        #intercept 
  rfslope_RS <- round(rf_RS$coefficients[2,1], digits = 6)         #slope of regression
  rfrs_RS <- round(rf_RS$r.squared, digits = 6)                    #r squared of full dataset linear regression
  rfp_RS <- round(rf_RS$coefficients[2,4], digits = 6)             #p-value of full dataset
  rfcor_RS <- round(cor.test(log(data_RS$x_value),data_RS$y_value)$estimate, digits = 6) #correlation coefficient of full dataset
  rfcount_RS <- length(data_RS$y_value) 
  #---------------------------------------------------------------------------------
  
  data<-data[!(data$x_value > breakpt),]
  subset_n <- length(data$y_value)
  
  stat_quantreg_bkpt <-  breakpt
  
  #If statement needed in case there are fewer than 4 datapoints to the left of x-axis inflection point, or if there are more than 3 points but all have the same x_value
  duplicates <- unique(data$x_value, incomparables = FALSE)
  if(nrow(data) && length(duplicates) > 3) {   

    up90 <- rq(y_value ~ log(x_value),data = data, tau = quantile) #calculate the quantile regression
    newy <- c(log(data$x_value)*coef(up90)[2]+coef(up90)[1])            #find the upper quantile values of y for each value of DA based on the quantile regression
    upper.quant <- subset(data, data$y_value > newy)                        #create a subset of the data that only includes the stations with NT values higher than the y values just calculated
    
    print(paste("Upper quantile has ", nrow(upper.quant), "values"));
    #If statement needed in case there ae fewer than 4 datapoints in upper quantile of data set
    if (nrow(upper.quant) > 3) {
      
      regupper <- lm(y_value ~ log(x_value),data = upper.quant)  
      ru <- summary(regupper) #regression for upper quantile

      #If statement needed in case slope is "NA"
      if (nrow(ru$coefficients) > 1) {
        
      ruint <- round(ru$coefficients[1,1], digits = 6)                         #intercept 
      ruslope <- round(ru$coefficients[2,1], digits = 6)                       #slope of regression
      rurs <- round(ru$r.squared, digits = 6)                                  #r squared of upper quantile
      rursadj <- round(ru$adj.r.squared, digits = 6)                           #adjusted r squared of upper quantile
      rup <- round(ru$coefficients[2,4], digits = 6)                           #p-value of upper quantile
      rucor <-round(cor.test(log(upper.quant$x_value),upper.quant$y_value)$estimate, digits = 6) #correlation coefficient of upper quantile
      rucount <- length(upper.quant$y_value)
      regfull <- lm(y_value ~ log(x_value),data = data)            
      rf <- summary(regfull)                                                   #regression for full dataset
      rfint <- round(rf$coefficients[1,2], digits = 6)                         #intercept 
      rfslope <- round(rf$coefficients[2,1], digits = 6)                       #slope of regression
      rfrs <- round(rf$r.squared, digits = 6)                                  #r squared of full dataset linear regression
      rfp <- round(rf$coefficients[2,4], digits = 6)                           #p-value of full dataset
      rfcor <- round(cor.test(log(data$x_value),data$y_value)$estimate, digits = 6) #correlation coefficient of full dataset
      rfcount <- length(data$y_value) 
      
      #Set yaxis threshold = to maximum biometric value in database 
      yaxis_thresh <- paste(site,"/elfgen_maxy_export/",y_metric, sep="")
      yaxis_thresh <- read.csv(yaxis_thresh , header = TRUE, sep = ",")
      yaxis_thresh <- yaxis_thresh$y_value
      print (paste("Setting ymax = ", yaxis_thresh));
      
      #retreive metric varids and varnames
      metric_definitions <- paste(site,"/?q=/fe_metric_export",sep="");
      metric_table <- read.table(metric_definitions,header = TRUE, sep = ",")
      
      biometric_row <- which(metric_table$varkey == y_metric)
      biomeric_name <- metric_table[biometric_row,]
      biometric_title <- biomeric_name$varname                #needed for human-readable plot titles

      flow_row <- which(metric_table$varkey == x_metric)
      flow_name <- metric_table[flow_row,]
      flow_title <- flow_name$varname                         #needed for human-readable plot titles

      admincode <-paste(Hydroid,"_fe_quantreg_pwit",sep="");
      
      # stash the regression statistics using REST  
      if (send_to_rest == 'YES') {
        
        qd <- list(
          featureid = Hydroid,
          admincode = admincode,
          name = paste( "Quantile Regression (Piecewise IT), ", y_metric, ' = f( ', x_metric, ' ), upper ',quantile * 100, '%', sep=''),
          ftype = 'fe_quantreg_pwit',
          site = site,
          x = x_metric,
          y = y_metric,
          stats = list(
            stat_quantreg_m = ruslope,
            stat_quantreg_b = ruint,
            stat_quantreg_n = rucount,
            stat_quantreg_p = rup,
            stat_quantreg_rsq = rurs,
            stat_quantreg_adj_rsq = rursadj,
            stat_quantreg_qu = quantile,
            stat_quantreg_x = x_metric,
            stat_quantreg_y = y_metric,
            station_agg =station_agg,
            sampres = sampres,
            stat_quantreg_bkpt = stat_quantreg_bkpt,
            stat_quantreg_glo= glo,
            stat_quantreg_ghi = ghi,
            analysis_timespan = analysis_timespan,
            dataset_tag = dataset_tag
          )
        );
        print("Storing quantile regression.");
        adminid <- elf_store_data(qd, token, inputs, adminid)
      } else {
        #Plot images are stored using watershed hydrocode when NOT performing REST 
        adminid <- paste(search_code,"fe_quantreg_pwit_RS",x_metric,y_metric,quantile,station_agg,sampres,analysis_timespan,glo,ghi, sep='_');
      }
      
      #Display only 3 significant digits on plots
      plot_ruslope <- signif(ruslope, digits = 3)
      plot_ruint <- signif(ruint, digits = 3)
      plot_rurs <- signif(rurs, digits = 3)
      plot_rursadj <- signif(rursadj, digits = 3)
      plot_rup <- signif(rup, digits = 3)
      
      #Plot titles
      #plot_title <- paste(Feature.Name," (",sampres," grouping)\n",startdate," to ",enddate,"\n\nQuantile Regression: (breakpoint = ",round(breakpt, digits = 1),") - PWIT (",glo," < breakpoint < ",ghi,")",sep=""); #,"\n","\n",search_code,"  (",y_metric,")  vs  (",x_metric,")","\n",sep="");
      #plot_title <- paste(Feature.Name," (",sampres," grouping)\n",startdate," to ",enddate,"\n\nQuantile Regression: (",glo," < ",round(breakpt, digits = 1)," < ",ghi,") - PWIT ",sep=""); #,"\n","\n",search_code,"  (",y_metric,")  vs  (",x_metric,")","\n",sep="");
      plot_title <- paste(Feature.Name," (",sampres," grouping)\n",startdate," to ",enddate,"\n\nQuantile Regression: (",round(glo,1)," < breakpoint < ",round(ghi,1)," = ",round(breakpt, digits = 1),") - PWIT ",sep=""); #,"\n","\n",search_code,"  (",y_metric,")  vs  (",x_metric,")","\n",sep="");
      
      xaxis_title <- paste(flow_title,"\n","\n","m: ",plot_ruslope,"    b: ",plot_ruint,"    r^2: ",plot_rurs,"    adj r^2: ",plot_rursadj,"    p: ",plot_rup,"\n","    Upper ",((1 - quantile)*100),"% n: ",rucount,"    Data Subset n: ",subset_n,"    Full Dataset n: ",length(full_dataset$y_value),sep="");
      yaxis_title <- paste(biometric_title);
      EDAS_upper_legend <- paste("Data Subset (Upper ",((1 - quantile)*100),"%)",sep="");
      Reg_upper_legend <- paste("Regression (Upper ",((1 - quantile)*100),"%)",sep="");       
      Quantile_Legend <- paste(quantile," Quantile (Data Subset)",sep=""); 
      EDAS_lower_legend <- paste("Data Subset (Lower ",(100-((1 - quantile)*100)),"%)",sep="");
      
      print (paste("Plotting ELF"));
      
      my.plot <- function() {
      # START - plotting function
        
        result <- base.plot(geom, data, full_dataset, upper.quant,
                            yaxis_thresh, quantile,
                            plot_title, xaxis_title, yaxis_title,
                            EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend)
        
        #RIGHT SIDE OF PIECEWISE
        result <- result + geom_vline(xintercept = breakpt[1],linetype = "longdash",colour = "black")+
        geom_point(data = data_RS, show.legend = FALSE, colour = "blue")+
        geom_point(data = upper.quant_RS, show.legend = FALSE, colour = "forestgreen")+
        geom_quantile(data = data_RS, show.legend = FALSE,formula = y ~ x, quantiles = quantile, colour = "black")+
        geom_smooth(data = data_RS, show.legend = FALSE,method = "lm", colour = "red", se = FALSE)+
        geom_smooth(data = upper.quant_RS, show.legend = FALSE,method = "lm", formula = y ~ x, colour = "orange", se = FALSE)

      }
      
      a <- my.plot()
      print(class(a)) 
      
      # END plotting function
      filename <- paste(adminid,"elf.png", sep="_")
      ggsave(file=filename, path = save_directory, width=8, height=6)
      
      
      print (paste("Plotting Barplot"));
      print (paste("ELF Slope: ",ruslope,sep="")); 
      print (paste("ELF Y-Intercept: ",ruint,sep="")); 
      if (ruslope >= 0){
        if (ruint >= 0){
          
          #slope barplot  
          pct_inputs<- list(ruslope = ruslope, 
                            ruint = ruint,
                            biometric_title = biometric_title, 
                            flow_title = flow_title,
                            Feature.Name = Feature.Name,
                            pct_chg = pct_chg,
                            sampres = sampres,
                            startdate = startdate,
                            enddate = enddate)
          elf_pct_chg (pct_inputs)
          
          filename <- paste(adminid,"pctchg.png", sep="_")
          ggsave(file=filename, path = save_directory, width=8, height=5)
        } else {
          print (paste("Y-Intercept is negative, not generating barplot"));        
        }  
      } else {
        print (paste("Slope is negative, not generating barplot"));        
      } 
      
      } else {
        print(paste("... Skipping slope is 'NA' for ", search_code,")", sep=''));
      }   
      
    } else {
      print(paste("... Skipping (fewer than 4 datapoints in upper quantile to the left of x-axis breakpoint in ", search_code,")", sep=''));
    }   
    
  } else {
    print(paste("... Skipping (fewer than 4 datapoints to the left of x-axis breakpoint in ", search_code,")", sep=''));
  }   
  
  } else {
    print(paste("... Skipping slope is 'NA' to the right of x-axis breakpoint in ", search_code,")", sep=''));
  }  
  
  } else {
    print(paste("... Skipping (fewer than 4 datapoints in upper quantile to the right of x-axis breakpoint in ", search_code,")", sep=''));
  }   
  
  } else {
    print(paste("... Skipping (fewer than 4 datapoints to the right of x-axis breakpoint in ", search_code,")", sep=''));
  }
  
  } else {
    print(paste("... Skipping (No breaks are found using this piece-wise method in ", search_code,")", sep=''));
  }
  return(a)
} #close function
