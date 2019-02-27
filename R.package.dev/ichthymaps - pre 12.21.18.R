
#-------------------------------------------------------------------------------
library(sbtools) #USGS ScienceBase R Package, https://journal.r-project.org/archive/2016-1/winslow-chamberlain-appling-etal.pdf
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
#-------------------------------------------------------------------------------
nhdplus_item = item_get("57645ff2e4b07657d19ba8e8") #Get item using its ScienceBase unique identifier

names(nhdplus_item)
nhdplus_item$citation
parent = item_get(nhdplus_item$parentId)
item_list_children(parent)
item_list_files(nhdplus_item)




nhdplus_download = item_file_download(nhdplus_item, dest_dir = tempdir()) #file downloaded into temp directory, as long as file exists it will not be re-downloaded
nhdplus_filename <- item_list_files(nhdplus_item)[1,1]
nhdplus.dataframe <- read.csv(file=paste(tempdir(),nhdplus_filename,sep="\\"), header=TRUE, sep=",")



#-------------------------------------------------------------------------------











# USER INPUTS 

watershed.code <- "nhd_huc8_02080104"

#-------------------------------------------------------------------------------

ichthy.url.csv <- "https://www.sciencebase.gov/catalog/file/get/5446a5a1e4b0f888a81b816d?f=__disk__25%2Fed%2F4a%2F25ed4a840a109d160d081bf144a66f615cb765cd"
ichthy.dataframe <- read.csv(file=ichthy.url.csv, header=TRUE, sep=",")


datasite <- 'http://deq1.bse.vt.edu/d.bet'
vahydro.url <- paste(datasite,"contained_nhdplusv2",watershed.code ,sep="/")
print(paste("Using ", vahydro.url, sep = ''))
NHDPlusV2.SEGS <- read.csv(vahydro.url, header = TRUE, sep = ",")


#i<-200
for (i in 1:length(NHDPlusV2.SEGS$COMID)) {
  print(paste("PROCESSING COMID ",i," OF ",length(NHDPlusV2.SEGS$COMID), sep = ''))
  
COMID <- NHDPlusV2.SEGS[i,]$COMID
COMID.rows <- ichthy.dataframe[which(ichthy.dataframe$COMID_NHDv2 == COMID),] #single comid

  # SKIP COMID IF THAT NHDPlusV2 SEGMENT ISNT IN ICHTHY DATASET
  if (length(COMID.rows$ID) == 0) {
    print(paste("NO ICHTHY DATA FOR COMID ", COMID, " (SKIPPING)", sep = ''))
    next
  }


COMID.Taxa.All <- COMID.rows$Name_Taxa
NT.TOTAL.ALL <- length(COMID.Taxa.All)
NT.TOTAL.UNIQUE <- length(unique(COMID.Taxa.All)) 
print(paste("ICHTHY DATA FOR COMID ",COMID," (NT TOTAL = ",NT.TOTAL.UNIQUE,")",sep = ''))
NHDPlusV2.SEGS[i,"NT_TOTAL"] <- NT.TOTAL.UNIQUE


}

#NHDPlusV2.SEGS.with.ichthy <- NHDPlusV2.SEGS
#NHDPlusV2.SEGS.with.ichthy <- NHDPlusV2.SEGS.HASichthy[-which(is.na(NHDPlusV2.SEGS$NT_TOTAL)), ]

library(ggplot2);

ggplot(NHDPlusV2.SEGS, aes(x=MAF_CFS,y=NT_TOTAL)) +
  geom_point(data = NHDPlusV2.SEGS)+
  scale_x_log10(
    limits = c(0.001, 15000),
    breaks = c(0.001, 0.01, 0.1, 1.0, 10, 100, 1000, 10000),
    labels = c("0.001", "0.01", "0.1", "1.0", "10", "100", "1,000", "10,000")
  )



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

watershed.code <- '02080104'
watershed.zip <- paste('https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_',watershed.code,'_HU8_Shape.zip',sep='')

temp <- tempfile()
download.file(watershed.zip)
#zip.path <- paste(temp,".zip",sep="")
file.rename(temp, paste(temp,".zip",sep="")) #ensure file has the extension .zip 
#data <- unz(paste(temp,".zip",sep=""),filename='\\Shape\\WBDHU8.shp')

t <- unzip(paste(temp,".zip",sep=""))

#library(rgdal)
#zipmap <- readOGR(dsn = paste(temp,".zip\\Shape\\",sep=""), layer = "WBDHU8.shp" )



data.shape <- readOGR("./Shape/WBDHU8.shp")
plot(data.shape)

library(rgdal) # is this needed?
projection <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 
data.shape.reprojected <- spTransform(data.shape, CRS=projection)    
plot(data.shape.reprojected)


#-------------------------------------------------------------------------------------------------


nhdplus.zip <- paste('ftp://www.horizon-systems.com/NHDPlus/NHDPlusV21/Data/NHDPlusMA/NHDPlusV21_MA_02_NHDPlusCatchment_01.7z',sep='')

nhdplus.temp <- tempfile()

download.file(nhdplus.zip, nhdplus.temp )
#zip.path <- paste(temp,".zip",sep="")
file.rename(nhdplus.temp, paste(nhdplus.temp,".zip",sep="")) #ensure file has the extension .zip 
#data <- unz(paste(temp,".zip",sep=""),filename='\\Shape\\WBDHU8.shp')

t <- unzip(paste(nhdplus.temp,".7z",sep=""))




#-------------------------------------------------------------------------------------------------
library(RJSONIO);
COMID <- 4508680 
#COMID.URL <- paste('https://ofmpub.epa.gov/waters10/Watershed_Characterization.Control?pPermanentIdentifier=',COMID,'&optOutFormat=JSON&optOutPrettyPrint=0&optOutGeomFormat=GEOJSON',sep="")  
COMID.URL <- paste('https://ofmpub.epa.gov/waters10/nhdplus.jsonv25?ppermanentidentifier=',COMID,'&pFilenameOverride=AUTO',sep="")
json_file <- fromJSON(COMID.URL)


json_file$`output`$header$attributes[[4]]$value

geom_json <- json_file$output$catchment_shape
geom <- geojson2wkt(geom_json)
#-------------------------------------------------------------------------------------------------











#file <- paste(temp,'Shape','WBDHU8.shp',sep='\\')
data <- unz(temp,filename='\\Shape\\WBDHU8.shp')

file.rename(temp, paste(temp,".zip",sep=""))

#data <- read.table(unz(temp, "WBDHU8.shp"))
#data <- unz(temp,temp)
unlink(temp)

library(memisc)
#UnZip(data ,item,dir=tempdir(),package=NULL)
Shape <- UnZip(paste(temp,".zip",sep=""),"Shape",package="memisc")

Shape <- unz(paste(temp,".zip",sep=""),"Shape")





library(rgdal)

Projection <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

#States<- spTransform(States, CRS=Projection)    



data.test <- spTransform(data, CRS=Projection)    


