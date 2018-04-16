library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);

elf_cleandata <- function (data, inputs, startdate = FALSE, enddate = FALSE) {
  
  #makes sure all metric values are numeric and not factorial (fixes error with ni, total)
  data$y_value <- as.numeric(data$y_value)
  #Subset by date range 
  data$tstime <- as.Date(data$tstime,origin="1970-01-01")
  
  if (typeof(startdate) != 'logical') {
    data <- subset(data, tstime > startdate)
  }
  if (typeof(enddate) != 'logical') {
    data <- subset(data, tstime < enddate)
  }
  
  #ADD COLUMN OF RATIO OF DRAINAGE AREA TO MEAN FLOW 
  data["ratio"] <- (data$drainage_area)/(data$qmean_annual)
  #REMOVE ALL STATIONS WHERE THE RATIO OF DA:Q IS GREATER THAN 1000
  data<-data[!(data$ratio > 1000),]
  
  #USE ONLY MAX NT VALUE FOR EACH STATION
  if(inputs$station_agg == "max"){ 
    aa <- data[order(data$hydrocode, data$y_value, decreasing=TRUE),]
    aa <- aa[!duplicated(aa$hydrocode),]
    aa <- aa[order(aa$hydrocode, aa$y_value),]
    data <- aa
  }
  
  #subsets data to exclude anything with a flowmetric value greater than the "xaxis_thresh" specified in the user inputs file
  data <- subset(data, x_value >= .001 & x_value < inputs$xaxis_thresh);
  
  #Export data as spreadsheet
  ##write.table(data, paste(save_directory,"data.tsv",sep=""), sep="\t")
  
  print(paste("Found ", nrow(data), sep=''));
  #If statement needed in case geographic region does not contain more than 3 points
  if(nrow(data) <= 3) {
    print("... Skipping (fewer than 3 datapoints)")
    return(FALSE)
  } 
  
  
  #Skip if there is only 1 or 2 unique flow metric values for this watershed (either only a single EDAS station, or multiple with the same flow metric, which would result in a vertical bar of points in the plot)
  station_x_value <- data$x_value
  remove_da_duplicates <- unique(station_x_value, incomparables = FALSE)
  if(length(remove_da_duplicates) == 1 | length(remove_da_duplicates) == 2) {
    print("... Skipping (the points are all organized in 1 or 2 vertical lines in )");
    return(FALSE) 
  } #closes bar of points skip if-statement (rare)
  
  #Skip if there is only 1 or 2 unique biometric values for this watershed
  station_y_value <- data$y_value
  remove_metric_duplicates <- unique(station_y_value, incomparables = FALSE)
  if(length(remove_metric_duplicates) == 1 | length(remove_metric_duplicates) == 2) {
    print("... Skipping (the points are all organized in 1 or 2 horizontal lines in )");
    return(FALSE) 
  } #closes bar of points skip if-statement (rare)
  return(data)
  
}

elf_assemble_batch <- function(inputs = list()){
  
  #Load inputs
  x_metric <- inputs$x_metric 
  y_metric <- inputs$y_metric 
  ws_ftype <- inputs$ws_ftype
  target_hydrocode <- inputs$target_hydrocode
  offset_x_metric <- inputs$offset_x_metric
  offset_y_metric <- inputs$offset_y_metric
  offset_ws_ftype <- inputs$offset_ws_ftype
  offset_hydrocode <- inputs$offset_hydrocode
  site <- inputs$site
  xaxis_thresh <- inputs$xaxis_thresh
  sampres <- inputs$sampres
  analysis_timespan <- inputs$analysis_timespan
  station_agg <- inputs$station_agg
  quantreg <- inputs$quantreg 
  ymax <- inputs$ymax   
  pw_it <- inputs$pw_it  
  pw_it_RS <- inputs$pw_it_RS 
  pw_it_RS_IFIM <- inputs$pw_it_RS_IFIM  
  twopoint <- inputs$twopoint
  token <- inputs$token

for (l in offset_ws_ftype:length(ws_ftype)) {
     
  print(paste("ws_ftype ",l,". of ",length(ws_ftype),". ",ws_ftype[l],sep=""))
  #Automatic bundle specification (WILL BE ELIMINATED ONCE WE UPDATE VAHYDRO STORAGE SCHEME)
  if(ws_ftype[l] == "hwi_region"){
    bundle <- "ecoregion"
  } else if(ws_ftype[l] == "state") {
    bundle <- "landunit" 
  } else if(ws_ftype[l] == "ecoregion_iii") {
    bundle <- "ecoregion" 
  } else if(ws_ftype[l] == "ecoregion_iv") {
    bundle <- "ecoregion" 
  } else if(ws_ftype[l] == "ecoiii_huc6") {
    bundle <- "ecoregion" 
  } else {
    bundle <- "watershed" 
  }
  #Pull in full list of Virginia watersheds for the specified ftype
  #If we define a hydrocode > 'XXXXXX' it will retrieve that single one
  HUClist_url_base <- paste(site,"/?q=elfgen_regions_export/",bundle, sep = "");
  if (!(target_hydrocode == '')) {
    HUClist_url_full <- paste(HUClist_url_base, ws_ftype[l], target_hydrocode, sep = "/");
  } else {
    HUClist_url_full <- paste(HUClist_url_base, ws_ftype[l], sep = "/");
  }
  #print(HUClist_url_full)
  HUClist <- read.table(HUClist_url_full,header = TRUE, sep = ",")
  Watershed_Hydrocode <- HUClist$Hydrocode
  Feature.Name <- HUClist$Feature.Name
  Hydroid <- HUClist$HydroID
  
for (k in offset_y_metric:length(y_metric)) {
  print(paste("y_metric ", k, ". of ",length(y_metric),". Beginning loop for ", y_metric[k], sep=''));
  for (j in offset_x_metric:length(x_metric)) {
    print(paste("x_metric ", j, ". of 14. Beginning loop for ", x_metric[j], sep=''));
    for (i in offset_hydrocode:length(Watershed_Hydrocode)) {
      print(paste("Feature ", i, ". of ",length(Watershed_Hydrocode),". Searching for stations from ", Watershed_Hydrocode[i], sep=''));
      search_code <- Watershed_Hydrocode[i];
      Feature.Name_code <- as.character(Feature.Name[i]);
      Hydroid_code <- Hydroid[i];
      ws_ftype_code <- ws_ftype[l]
      x_metric_code <-  x_metric[j];
      y_metric_code <-  y_metric[k];

      data <- vahydro_fe_data(
        search_code,
        x_metric_code,y_metric_code,
        bundle,ws_ftype_code,sampres
      );
      data$tstime <- as.Date(data$tstime,origin="1970-01-01")
      # clean up data
      
      if (inputs$analysis_timespan != 'full') {
        #Need to convert timespan paramteter into startdate and endate format for subsetting data 
        startdate <- paste(unlist(strsplit(inputs$analysis_timespan, "[-]"))[[1]],"-01-01",sep="")
        enddate <- paste(unlist(strsplit(inputs$analysis_timespan, "[-]"))[[2]],"-12-31",sep="")
        print(paste("startdate: ", startdate))
        print(paste("enddate: ", enddate))
        date_label = "subset: "
      } else {        
        print ("min function")
        startdate <- min(data$tstime)
        enddate <- max(data$tstime)   #no dates set with REST, only "full" for analysis_timespan propcode
        print ("done min function")
        date_label = "full timespan: "
      }
      
      data <- elf_cleandata(data, inputs, startdate, enddate)
      startdate <- paste(date_label,startdate,sep="")
      startdate <- paste(date_label,startdate,sep="") #if plotting for full timespan, display start and end dates above plot
      
      if (typeof(data) == 'logical') {
        next
      }
#---------------------------------------------------------------------     
      
      #Load Functions               
      source(paste(fxn_locations,"elf_quantreg.R", sep = ""));       #loads elf_quantreg function
      source(paste(fxn_locations,"elf_ymax.R", sep = ""));           #loads elf_ymax function
      source(paste(fxn_locations,"elf_pw_it.R", sep = ""));          #loads ef_pw_it function
      source(paste(fxn_locations,"elf_twopoint.R", sep = ""));       #loads elf_twopoint function
      source(paste(fxn_locations,"elf_pw_it_RS.R", sep = ""));       #loads ef_pw_it_RS function
      source(paste(fxn_locations,"elf_pct_chg.R", sep =""));         #loads percent change barplot function
      source(paste(fxn_locations,"elf_store_data.R", sep = ""));     #loads function used to store ELF stats to VAHydro
      source(paste(fxn_locations,"elf_pw_it_RS_IFIM.R", sep = ""));  #loads elf_pw_it_RS_IFIM function for overlaying WUA curves on ELFs
      
      if(quantreg == "YES") {print(paste("PLOTTING - method quantreg breakpoint ...",sep="")) 
                            elf_quantreg (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
      if(ymax == "YES") {print(paste("PLOTTING - method quantreg breakpoint at y-max...",sep="")) 
                            elf_ymax (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
      if(pw_it == "YES") {print(paste("PLOTTING - method quantreg breakpoint using piecewise function...",sep="")) 
                            elf_pw_it (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
      if(twopoint == "YES") {print(paste("PLOTTING - method two-point function...",sep=""))
                            elf_twopoint (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}

      if(pw_it_RS == "YES") {print(paste("PLOTTING - method quantreg breakpoint using piecewise function (Including regression to the right of breakpoint)...",sep=""))
                            plt <- elf_pw_it_RS (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
      print(class(plt))
      if(pw_it_RS_IFIM == "YES") {print(paste("PLOTTING - method quantreg breakpoint using piecewise function (Including regression to the right of breakpoint)...",sep=""))
                                  elf_pw_it_RS_IFIM (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
      
      return(plt)
        } #closes watershed for loop  
      } #closes x_metric for loop
    } #closes y_metric for loop
  } #closes ws_ftype for loop
} #close function