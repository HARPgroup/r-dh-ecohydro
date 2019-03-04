#-------------------------------------------------------------------------------
library(sbtools) #USGS ScienceBase R Package, https://journal.r-project.org/archive/2016-1/winslow-chamberlain-appling-etal.pdf
library(RJSONIO) #required for processing nhdplus JSON data
library(ggplot2) #required for plotting 
#-------------------------------------------------------------------------------
# USER INPUTS 

HUC8.code <- '02070008' #VA TEST: 02070008
#-------------------------------------------------------------------------------


ichthy_item = item_get("5446a5a1e4b0f888a81b816d") #Get item using its ScienceBase unique identifier
#names(ichthy_item)
#ichthy_item$citation
#parent = item_get(ichthy_item$parentId)
#item_list_children(parent)
#item_list_files(ichthy_item)[1]
ichthy_download = item_file_download(ichthy_item, dest_dir = tempdir()) #file downloaded into temp directory, as long as file exists it will not be re-downloaded
ichthy_filename <- item_list_files(ichthy_item)[1,1]
ichthy.dataframe <- read.csv(file=paste(tempdir(),ichthy_filename,sep="\\"), header=TRUE, sep=",")



#-------------------------------------------------------------------------------


#HUC8.code <- '02070008'
HUC8.code <- gsub("(?<![0-9])0+", "", HUC8.code, perl = TRUE) #remove leading zero (conforms to ichthy table format)
HUC8.rows <- ichthy.dataframe[which(ichthy.dataframe$HUC8 == HUC8.code),] 


watershed.df <- data.frame(HUC8=character(),
                           COMID=character(), 
                           NT_TOTAL=character(), 
                           stringsAsFactors=FALSE)


#i<-1
for (i in 1:length(HUC8.rows$COMID_NHDv2)) {
  print(paste("PROCESSING COMID ",i," OF ",length(HUC8.rows$COMID_NHDv2), sep = ''))
  
  COMID <- HUC8.rows[i,]$COMID
  COMID.rows <- HUC8.rows[which(HUC8.rows$COMID_NHDv2 == COMID),] #single comid
  
  # SKIP COMID IF THAT NHDPlusV2 SEGMENT ISNT IN ICHTHY DATASET
  if (length(COMID.rows$ID) == 0) {
    print(paste("NO ICHTHY DATA FOR COMID ", COMID, " (SKIPPING)", sep = ''))
    next
  }
  
  
  COMID.Taxa.All <- COMID.rows$Name_Taxa
  NT.TOTAL.ALL <- length(COMID.Taxa.All)
  NT.TOTAL.UNIQUE <- length(unique(COMID.Taxa.All)) 
  print(paste("ICHTHY DATA FOR COMID ",COMID," (NT TOTAL = ",NT.TOTAL.UNIQUE,")",sep = ''))
  #HUC8.rows[i,"NT_TOTAL"] <- NT.TOTAL.UNIQUE
  
  watershed.df.i <- data.frame(HUC8.code,COMID_NHDv2 = COMID,NT.TOTAL.UNIQUE)
  watershed.df <- rbind(watershed.df,watershed.df.i)

  
}

  watershed.df <- unique(watershed.df[, 1:3]) #remove duplicates (so each comid appears once)
  
  #j<-1
  for (j in 1:length(watershed.df$COMID_NHDv2)) {
  COMID <- watershed.df[j,]$COMID_NHDv2
  #COMID.URL <- paste('https://ofmpub.epa.gov/waters10/Watershed_Characterization.Control?pPermanentIdentifier=',COMID,'&optOutFormat=JSON&optOutPrettyPrint=0&optOutGeomFormat=GEOJSON',sep="")  
  COMID.URL <- paste('https://ofmpub.epa.gov/waters10/nhdplus.jsonv25?ppermanentidentifier=',COMID,'&pFilenameOverride=AUTO',sep="")
  json_file <- fromJSON(COMID.URL)
  
  COMID.MAF <- json_file$`output`$header$attributes[[4]]$value #MAF in cfs
  watershed.df[j,"MAF"] <- COMID.MAF
  
  
  print(paste("PROCESSING ",j," OF ",length(watershed.df$COMID_NHDv2)," (COMID ",COMID,"), MAF = ",COMID.MAF,sep = ''))
  
  }
  
  #-------------------------------------------------------------------------------
  
  ggplot(watershed.df, aes(x=MAF,y=NT.TOTAL.UNIQUE)) +
    geom_point(data = watershed.df)+
    scale_x_log10(
      limits = c(0.001, 15000),
      breaks = c(0.001, 0.01, 0.1, 1.0, 10, 100, 1000, 10000),
      labels = c("0.001", "0.01", "0.1", "1.0", "10", "100", "1,000", "10,000")
    )
  
  #-------------------------------------------------------------------------------
  
  
  
  
  
  
  