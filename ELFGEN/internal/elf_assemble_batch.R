library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);
library(rgeos); #used for geospatial processing 
library(sp); #contains SpatialPolygonsDataFrame()

elf_run_method <- function( method, inputs, data, x_metric_code, y_metric_code, ws_ftype_code, 
    Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate, geom
  ) {
  
  if(method == "quantreg") {
    print(paste("PLOTTING - method quantreg breakpoint ...",sep="")) 
    plt <- elf_quantreg (
      inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, 
      Hydroid_code, search_code, token, startdate, enddate, geom
    )
    return;
  }
  
  if(method == "ymax") {
    print(paste("PLOTTING - method ymax quantreg breakpoint at y-max...",sep="")) 
    plt <- elf_ymax (
      inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, 
      Hydroid_code, search_code, token, startdate, enddate, geom
    )
    return;
  }
  
  if(method == "pwit") {
    print(paste("PLOTTING - method quantreg breakpoint using piecewise function...",sep="")) 
    plt <- elf_pw_it (
      inputs, data, x_metric_code, y_metric_code, ws_ftype_code, 
      Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate, geom
    )
    return;
  }
  
  if(method == "twopoint") {
    print(paste("PLOTTING - method two-point function...",sep=""))
    plt <- elf_twopoint (
      inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, 
      Hydroid_code, search_code, token, startdate, enddate
    )
    return;
  }
  
  if(method == "pwit_RS") {
    print(paste("PLOTTING - method quantreg breakpoint using piecewise function (Including regression to the right of breakpoint)...",sep=""))
    plt <-  elf_pw_it_RS (
      inputs, data, x_metric_code, y_metric_code, ws_ftype_code, 
      Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate, geom
    )
    return;
  }
  
  if(method == "pw_it_RS_IFIM") {
    print(paste("PLOTTING - method quantreg breakpoint using piecewise function (Including regression to the right of breakpoint)...",sep=""))
    plt <- elf_pw_it_RS_IFIM (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)
    return(plt)
  }
  
}

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
  batchlist = FALSE;
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
    print(paste("Searching ", HUClist_url_full, " for target_hydrocode ", target_hydrocode, sep=''))
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
          if (typeof(data) == 'logical') {
            next
          }  
          
          # now, add this to a master list to return
          if (batchlist == FALSE) {
            batchlist = data.frame(
              target_hydrocode = search_code, 
              hydroid = Hydroid_code,
              name = Feature.Name_code, 
              method = inputs$method,
              ws_ftype = ws_ftype_code, 
              bundle = bundle, 
              dataset_tag = inputs$dataset_tag, 
              x_metric = x_metric_code, 
              y_metric = y_metric_code,
              sampres = sampres
            )
          } else {
            batchlist <- rbind(
              batchlist, data.frame(
                target_hydrocode = search_code, 
                hydroid = Hydroid_code,
                name = Feature.Name_code, 
                method = inputs$method,
                ws_ftype = ws_ftype_code, 
                bundle = bundle, 
                dataset_tag = inputs$dataset_tag, 
                x_metric = x_metric_code, 
                y_metric = y_metric_code,
                sampres = sampres
              )
            )
          }
        } #closes watershed for loop  
      } #closes x_metric for loop
    } #closes y_metric for loop
  } #closes ws_ftype for loop
  return(batchlist)
} #close function


base.plot <- function(geom, data, full_dataset, upper.quant,
                      yaxis_thresh, quantile,
                      plot_title, xaxis_title, yaxis_title,
                      EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend
                      ) {

  #Load Virginia geometry
  VADF <- read.csv("VADF.csv")

  
  wsdataProjected <- SpatialPolygonsDataFrame(readWKT(geom),data.frame("id"), match.ID = TRUE)
  #class(dataProjected)
  wsdataProjected@data$id <- rownames(wsdataProjected@data)
  watershedPoints <- fortify(wsdataProjected, region = "id")
  watershedDF <- merge(watershedPoints, wsdataProjected@data, by = "id")
  #watershedDF <- VADF
  
  map <- ggplotGrob(ggplot(data = VADF, aes(x=long, y=lat, group = group))+
                      theme(panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank())+
                      geom_polygon(data = VADF, fill = "gray")+
                      geom_polygon(data = watershedDF, color="forestgreen", fill = NA,lwd=0.5)+
                      scale_x_continuous(limits = c(-85, -74))+
                      scale_y_continuous(limits = c(35, 41))+
                      theme(axis.title.x=element_blank(),
                            axis.text.x=element_blank(),
                            axis.ticks.x=element_blank(),
                            axis.title.y=element_blank(),
                            axis.text.y=element_blank(),
                            axis.ticks.y=element_blank())
  )
  
  result <- ggplot(data, aes(x=x_value,y=y_value)) + ylim(0,yaxis_thresh) + 
    geom_point(data = full_dataset,aes(colour="aliceblue")) +
    geom_point(data = data,aes(colour="blue")) + 
    stat_smooth(method = "lm",fullrange=FALSE,level = .95, data = upper.quant, aes(x=x_value,y=y_value,color = "red")) +
    geom_point(data = upper.quant, aes(x=x_value,y=y_value,color = "black")) + 
    geom_quantile(data = data, quantiles= quantile,show.legend = TRUE,aes(color="red")) + 
    geom_smooth(data = data, method="lm",formula=y ~ x,show.legend = TRUE, aes(colour="yellow"),se=FALSE) + 
    geom_smooth(data = upper.quant, formula = y ~ x, method = "lm", show.legend = TRUE, aes(x=x_value,y=y_value,color = "green"),se=FALSE) + 

    #add map to upper right of plot
    annotation_custom(
      grob = map,
      xmin = 4.55,
      xmax = 8,
      ymin = yaxis_thresh-(0.1*yaxis_thresh),
      ymax = yaxis_thresh+(0.3*yaxis_thresh)
    )+
    
    ggtitle(plot_title) + 
    theme(
      plot.title = element_text(size = 12, face = "bold"),axis.text = element_text(colour = "blue")
    ) +
    labs(x=xaxis_title,y=yaxis_title) + 
    scale_x_log10(
      limits = c(0.001,15000),
      breaks = trans_breaks("log10", function(x) {10^x}),
      labels = trans_format("log10", math_format(10^.x))
    ) + 
    annotation_logticks(sides = "b")+
    theme(legend.key=element_rect(fill='white')) +
    #Add legend
    scale_color_manual(
      "Legend",
      values=c("gray66","forestgreen","blue","orange","black","red"),
      labels=c("Full Dataset",EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend,"Regression (Data Subset)")
    ) + 
    guides(
      colour = guide_legend(
        override.aes = list(
          size=c(1,1,1,1,1,1),
          linetype=c(0,0,0,1,1,1), 
          shape=c(16,16,16,NA,NA,NA)
        ),
        label.position = "right"
      )
    ); 
  return(result)
}

