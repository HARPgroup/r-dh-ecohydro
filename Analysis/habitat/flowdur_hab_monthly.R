### CREATE IMAGE FILES of AUGUST LOW FLOW - FLOW DURATION PLOT for ALL USGS GAGES
rm(list = ls()) # clear variables
## Load necessary libraries
library('zoo')
library('IHA')
library("stringr")
library("ggplot2")

site <- "http://deq1.bse.vt.edu/d.dh" 

#update to file location of config.local.private
#config_file <- "C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools\\"
basepath ='C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/';

#----------------------------------------------------------------------------------------
#load functions
source(paste(basepath,'r-dh-ecohydro/config.local.private',sep='/'))
#save_directory <- "/var/www/html/images/dh/dev"
save_directory <- paste(basepath,"plots",sep="")
dir.create(save_directory, showWarnings = FALSE) #create "plots" directory if doesn't exist 
source(paste(hydro_tools,"/USGS/usgs_gage_functions.R", sep = ""))
source(paste(hydro_tools,"/LowFlow/fn_iha.R", sep = ""));
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
rest_uname = FALSE;
rest_pw = FALSE;
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token(site, token, rest_uname, rest_pw);

ifim_sites <- read.table(paste(habitat_files,'ifim_sites_df.txt',sep='')) #loads ifim sites text file
ifim_sites <- ifim_sites[-1,]

#i<-1
for (i in 1:length(ifim_sites$V1)){

ifim_featureid <- as.character(ifim_sites$V1[i])
  
#===============================================================
# BEGIN RETRIEVE IFIM DATA
#===============================================================
ifim_dataframe <- vahydro_prop_matrix(ifim_featureid,'ifim_habitat_table')

ifim_site <- getFeature(list(hydroid = ifim_featureid), token, site, feature)
ifim_site_name <- as.character(ifim_site$name)

inputs = list(varkey = 'usgs_siteid',featureid = ifim_featureid,entity_type = 'dh_feature')
gageprop <- getProperty(inputs,site,prop)
gage <- as.character(gageprop$propcode)
gage_factor <- as.numeric(as.character(gageprop$propvalue))

qmin <- min(ifim_dataframe$discharge)
qmax <- max(ifim_dataframe$discharge)

  data <- streamgage_historic(gage)

  #----------------------------------------------
  # FLOW WEIGHTING TO IFIM SITE LOCATION USING GAGE WEIGHTING FACTOR 
  data$Flow <- data$Flow*gage_factor
  #---------------------------------  
  
  #Add column of month names
  data [grep('-01-', data$Date, perl= TRUE), "month" ] <- "Jan"
  data [grep('-02-', data$Date, perl= TRUE), "month" ] <- "Feb"
  data [grep('-03-', data$Date, perl= TRUE), "month" ] <- "Mar"
  data [grep('-04-', data$Date, perl= TRUE), "month" ] <- "Apr"
  data [grep('-05-', data$Date, perl= TRUE), "month" ] <- "May"
  data [grep('-06-', data$Date, perl= TRUE), "month" ] <- "Jun"
  data [grep('-07-', data$Date, perl= TRUE), "month" ] <- "Jul"
  data [grep('-08-', data$Date, perl= TRUE), "month" ] <- "Aug"
  data [grep('-09-', data$Date, perl= TRUE), "month" ] <- "Sep"
  data [grep('-10-', data$Date, perl= TRUE), "month" ] <- "Oct"
  data [grep('-11-', data$Date, perl= TRUE), "month" ] <- "Nov"
  data [grep('-12-', data$Date, perl= TRUE), "month" ] <- "Dec"
 
   
  Total_data <- data
      Total.nrow.below <- nrow(Total_data[Total_data[, "Flow"] < qmin,])
      Total.nrow.above <- nrow(Total_data[Total_data[, "Flow"] > qmax,])
      Total_hab_pct <- 100-((Total.nrow.below + Total.nrow.above)/nrow(Total_data))*100
  
  Jan_data <- data[which(data$month == "Jan"),]
      Jan.nrow.below <- nrow(Jan_data[Jan_data[, "Flow"] < qmin,])
      Jan.nrow.above <- nrow(Jan_data[Jan_data[, "Flow"] > qmax,])
      Jan_hab_pct <- 100-((Jan.nrow.below + Jan.nrow.above)/nrow(Jan_data))*100
      
  Feb_data <- data[which(data$month == "Feb"),]
      Feb.nrow.below <- nrow(Feb_data[Feb_data[, "Flow"] < qmin,])
      Feb.nrow.above <- nrow(Feb_data[Feb_data[, "Flow"] > qmax,])
      Feb_hab_pct <- 100-((Feb.nrow.below + Feb.nrow.above)/nrow(Feb_data))*100
  
  Mar_data <- data[which(data$month == "Mar"),]
      Mar.nrow.below <- nrow(Mar_data[Mar_data[, "Flow"] < qmin,])
      Mar.nrow.above <- nrow(Mar_data[Mar_data[, "Flow"] > qmax,])
      Mar_hab_pct <- 100-((Mar.nrow.below + Mar.nrow.above)/nrow(Mar_data))*100
  
  Apr_data <- data[which(data$month == "Apr"),]
      Apr.nrow.below <- nrow(Apr_data[Apr_data[, "Flow"] < qmin,])
      Apr.nrow.above <- nrow(Apr_data[Apr_data[, "Flow"] > qmax,])
      Apr_hab_pct <- 100-((Apr.nrow.below + Apr.nrow.above)/nrow(Apr_data))*100
  
  May_data <- data[which(data$month == "May"),]
      May.nrow.below <- nrow(May_data[May_data[, "Flow"] < qmin,])
      May.nrow.above <- nrow(May_data[May_data[, "Flow"] > qmax,])
      May_hab_pct <- 100-((May.nrow.below + May.nrow.above)/nrow(May_data))*100
  
  Jun_data <- data[which(data$month == "Jun"),]
      Jun.nrow.below <- nrow(Jun_data[Jun_data[, "Flow"] < qmin,])
      Jun.nrow.above <- nrow(Jun_data[Jun_data[, "Flow"] > qmax,])
      Jun_hab_pct <- 100-((Jun.nrow.below + Jun.nrow.above)/nrow(Jun_data))*100
  
  Jul_data <- data[which(data$month == "Jul"),]
      Jul.nrow.below <- nrow(Jul_data[Jul_data[, "Flow"] < qmin,])
      Jul.nrow.above <- nrow(Jul_data[Jul_data[, "Flow"] > qmax,])
      Jul_hab_pct <- 100-((Jul.nrow.below + Jul.nrow.above)/nrow(Jul_data))*100
  
  Aug_data <- data[which(data$month == "Aug"),]
      Aug.nrow.below <- nrow(Aug_data[Aug_data[, "Flow"] < qmin,])
      Aug.nrow.above <- nrow(Aug_data[Aug_data[, "Flow"] > qmax,])
      Aug_hab_pct <- 100-((Aug.nrow.below + Aug.nrow.above)/nrow(Aug_data))*100
  
  Sep_data <- data[which(data$month == "Sep"),]
      Sep.nrow.below <- nrow(Sep_data[Sep_data[, "Flow"] < qmin,])
      Sep.nrow.above <- nrow(Sep_data[Sep_data[, "Flow"] > qmax,])
      Sep_hab_pct <- 100-((Sep.nrow.below + Sep.nrow.above)/nrow(Sep_data))*100
  
  Oct_data <- data[which(data$month == "Oct"),]
      Oct.nrow.below <- nrow(Oct_data[Oct_data[, "Flow"] < qmin,])
      Oct.nrow.above <- nrow(Oct_data[Oct_data[, "Flow"] > qmax,])
      Oct_hab_pct <- 100-((Oct.nrow.below + Oct.nrow.above)/nrow(Oct_data))*100
  
  Nov_data <- data[which(data$month == "Nov"),]
      Nov.nrow.below <- nrow(Nov_data[Nov_data[, "Flow"] < qmin,])
      Nov.nrow.above <- nrow(Nov_data[Nov_data[, "Flow"] > qmax,])
      Nov_hab_pct <- 100-((Nov.nrow.below + Nov.nrow.above)/nrow(Nov_data))*100
  
  Dec_data <- data[which(data$month == "Dec"),]
      Dec.nrow.below <- nrow(Dec_data[Dec_data[, "Flow"] < qmin,])
      Dec.nrow.above <- nrow(Dec_data[Dec_data[, "Flow"] > qmax,])
      Dec_hab_pct <- 100-((Dec.nrow.below + Dec.nrow.above)/nrow(Dec_data))*100
  
  
  df <- data.frame(month=c("Total",
                           "Jan",
                           "Feb",
                           "Mar",
                           "Apr",
                           "May",
                           "Jun",
                           "Jul",
                           "Aug",
                           "Sep",
                           "Oct",
                           "Nov",
                           "Dec"),
                   pct=c(Total_hab_pct,
                         Jan_hab_pct,
                         Feb_hab_pct,
                         Mar_hab_pct,
                         Apr_hab_pct,
                         May_hab_pct,
                         Jun_hab_pct,
                         Jul_hab_pct,
                         Aug_hab_pct,
                         Sep_hab_pct,
                         Oct_hab_pct,
                         Nov_hab_pct,
                         Dec_hab_pct),
                   order=c(1,
                         2,
                         3,
                         4,
                         5,
                         6,
                         7,
                         8,
                         9,
                         10,
                         11,
                         12,
                         13))
  
  ggplot(df, aes(x = reorder(month, order), y = pct))+
    geom_bar(stat="identity", width=0.5)+
    labs(title = paste("Percent Monthly Historic Gage Flow Coverage by WUA Table",sep=""),
       subtitle = paste(ifim_site_name," (Flows Area-Weighted from Gage ",gage,")",sep=""))+
    scale_y_continuous(limits = c(0, 100))+
    geom_text(aes(label=paste(round(pct,1),"%",sep="")), position=position_dodge(width=0.9), vjust=-0.25)+
    xlab("Month")+ 
    ylab("Percent Flow Coverage")
  
  filename <- paste(ifim_site_name,"monthly_coverage_bar.png", sep="_")
  ggsave(file=filename, path = save_directory, width=14, height=8)
 
} #end of ifim site for-loop