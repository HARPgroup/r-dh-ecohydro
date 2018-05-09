### Read in Flow Time Series from USGS Gages

# Read in information about the study reaches
#setwd("C:/Users/Grad/Google Drive/Flow-Ecology")
setwd("C:/Users/Kinsey/Google Drive/Flow-Ecology")
raw.info <- read.csv("information.csv")
attach(raw.info)

gageid <- sprintf("%08d", gageid) #add leading zero to USGS gage IDs

# Initialize variables
flow.matrix <- vector("list", dim(raw.info)[1]) #set up list to store all flow time series
startdate <- c()
enddate <- c()

# Loop through each site
for (i in 1:dim(raw.info)[1]) {

	## Load Streamflow Data from USGS Gage
	url_base <- "http://waterservices.usgs.gov/nwis/dv/?variable=00060&format=rdb&startDT=1838-01-01&site="
	url <- paste(url_base, gageid[i], "&endDT=2014-09-30", sep="") #set end of water year 2014 as end date
raw.data <- read.table(url, skip = 25, comment.char = "#") #skip first 25 linse of header
# Print status message
print(paste("Reading data from gage ", gageid[i], "...", sep=""))
# Check to make sure all header rows are removed
if (raw.data[1,1] != "USGS") {
raw.data <- raw.data[-1,] #remove first line
}
# Store start and end dates
startdate[i] <- as.character(raw.data[1,3]) #read first date on record (column 3)
enddate[i] <- as.character(raw.data[dim(raw.data)[1],3]) #read last date on record (column 3)
# Weight streamflow if a study site drainage area is provided
if (is.na(da.wua.sqmi[i]) == "FALSE") {
raw.data[,4] <- as.numeric(as.vector(raw.data[,4]))*(da.wua.sqmi[i] / da.gage.sqmi[i])
}
# Store date & time in flow.matrix list
flow.matrix[[i]] <- raw.data[,3:4]
}