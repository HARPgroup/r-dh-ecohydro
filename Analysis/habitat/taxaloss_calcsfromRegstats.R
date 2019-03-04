
#Cycle through the slope for each monthly regression for each huc8 to calculate % taxa loss and # taxa loss

#Store in a data table and export to CSV

##USGS taxa loss calculations from regression summary statistics 
#primarily works for IFIM sites because it selects the flow input from MAF.  
##created by JLRAPP 07/11/2018  



library(data.table);
library(scales);
require ("data.table")

remove(list=ls())
graphics.off() 
setwd("D:\\Jkrstolic\\R\\deqEcoflows\\Habitat\\TaxaLoss_PI_PressHUC8\\RapidanCaseStudy")

# table with monthly, mean annual flow, DA regression stats for any HUC8s of interest.  In this case all IFIM sites.  
#needs a flow input, here I used MAF for the IFIM site to represent stream size.  
#This data file only includes HUC8 with IFIM.  
Input_reg_stats = read.csv(file="RapidanHUC8_inputFile_bpj-huc6bp-rcc.csv",header=TRUE) #IFIMSITES_HUC8_bpj-huc6_bp_rccMonthly_RegressionStats.csv
data.table.reg_stats <- data.table(Input_reg_stats)
run.date         <- format(Sys.Date(),"%Y%m%d")


#User Inputs _____________________________________________
inputs <- list(
  Huc8 <- data.table.reg_stats$HUC8,   #designate the column containing huc8 records $IFIM.study.HUC8
  numbHuc <- length(Huc8), #Variables needed for calculations
  xvar       <- data.table.reg_stats$in_xvar,
  MAF  <- data.table.reg_stats$MAF, #mean annual flow of IFIM site representing stream size
  slope <- data.table.reg_stats$out_m,
  b <- data.table.reg_stats$out_b,
  TaxalossRound <- 2
  )

for (J in 1:numbHuc) {
  
  #???y=m[ln(11-z)] = # change in taxa
  data.table.reg_stats$nt_numblost_10pct <- as.numeric( data.table.reg_stats$out_m * log(1/(1 - 0.1)))
  data.table.reg_stats$nt_numblost_20pct <- as.numeric( data.table.reg_stats$out_m * log(1/(1 - 0.2)))
  data.table.reg_stats$nt_numblost_30pct <- as.numeric( data.table.reg_stats$out_m * log(1/(1 - 0.3)))
  data.table.reg_stats$nt_numblost_40pct <- as.numeric( data.table.reg_stats$out_m * log(1/(1 - 0.4)))
  data.table.reg_stats$nt_numblost_50pct <- as.numeric( data.table.reg_stats$out_m * log(1/(1 - 0.5)))
  
  ###%taxa loss = (m[ln (1/1-z) ]  / [m *ln(x) +b] )*100 = percent change in taxa
  data.table.reg_stats$nt_pctlost_10pct <- as.numeric((slope * log(1/(1 - 0.1)) / (slope* log(MAF)+ b))*100)
  data.table.reg_stats$nt_pctlost_20pct <- as.numeric((slope * log(1/(1 - 0.2)) / (slope* log(MAF)+ b))*100)
  data.table.reg_stats$nt_pctlost_30pct <- as.numeric((slope * log(1/(1 - 0.3)) / (slope* log(MAF)+ b))*100)
  data.table.reg_stats$nt_pctlost_40pct <- as.numeric((slope * log(1/(1 - 0.4)) / (slope* log(MAF)+ b))*100)
  data.table.reg_stats$nt_pctlost_50pct <- as.numeric((slope * log(1/(1 - 0.5)) / (slope* log(MAF)+ b))*100)}

   
  #Save to a file
  write.csv(data.table.reg_stats, file = paste("HUC8_RapidanHUC8TaxaLoss_bpj-huc6bp-rcc",".", run.date,".csv", sep=""), row.names = F, quote = FALSE)
  
