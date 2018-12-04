### CREATE IMAGE FILES of AUGUST LOW FLOW - FLOW DURATION PLOT for ALL USGS GAGES
rm(list = ls()) # clear variables
## Load necessary libraries
library('zoo')
library('IHA')
library("stringr")

site <- "http://deq1.bse.vt.edu/d.dh" 

#update to file location of config.local.private
#config_file <- "C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools\\"
basepath ='C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/';

#----------------------------------------------------------------------------------------
#load functions
source(paste(basepath,'r-dh-ecohydro/config.local.private',sep='/'))
#save_directory <- "/var/www/html/images/dh/dev"
save_directory <- paste(basepath,"plots",sep="")
#dir.create(save_directory, showWarnings = FALSE) #create "plots" directory if doesn't exist 
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
#ifim_featureid <- 397299
#ifim_featureid <-397295

ifim_dataframe <- vahydro_prop_matrix(ifim_featureid,'ifim_habitat_table')

ifim_site <- getFeature(list(hydroid = ifim_featureid), token, site, feature)
ifim_site_name <- as.character(ifim_site$name)

inputs = list(varkey = 'usgs_siteid',featureid = ifim_featureid,entity_type = 'dh_feature')
gageprop <- getProperty(inputs,site,prop)
gage <- as.character(gageprop$propcode)
gage_factor <- as.numeric(as.character(gageprop$propvalue))

#===============================================================
#===============================================================

qmin <- min(ifim_dataframe$discharge)
qmax <- max(ifim_dataframe$discharge)
#polygon(c(x,rev(x)),c(y2,rev(y1)),col="skyblue")

#===============================================================
#===============================================================

## Initialize ALF
alf <- c()
NP.alf <- c()


## Get active streamflow gages in VA
# URL to list of all active streamgages in VA
####url <- "http://waterservices.usgs.gov/nwis/dv/?format=rdb&stateCd=va&siteStatus=active&variable=00060"

# determining how many active gages there are to specify when to stop reading data
####all_info <- scan(url, what="character", skip=10, nlines=1)
####num_sites <- as.numeric(all_info[6])
####gage_info <- scan(url, what="character", skip=11, sep="\t", nlines=num_sites)

# setting up a pattern to pull only the gage numbers from the string
####pattern <- "([[:digit:]]{8})"
####x <- str_extract_all(gage_info, pattern)

# set the gage numbers as a vector
####gage <- as.character(x)

#i <- 1
## Calculations & Plot for Each Gage 
for (i in 1:length(gage)) {   
  
  if (nchar(gage[i]) == 8) {
    data <- streamgage_historic(gage[i])
  } else {
    next
  }
  
  #----------------------------------------------
  # FLOW WEIGHTING TO IFIM SITE LOCATION USING GAGE WEIGHTING FACTOR 
  data$Flow <- data$Flow*gage_factor
  
  #----------------------------------------------
  
  
  # Continuing even with error with file
  if (class(data)=="try-error") { 
    print(paste("Error: empty file ", url))
    alf[i] <- "FILE ERROR"
    
    NP <- seq(0, 100, 1) #make vector of all possible non-exceed probs
    sortflow <- rep(NA, times=length(NP)) #create vector of 0s for all NP values
    
    plot(NP, sortflow, pch=20, log='y', main=gage[i], ylim=c(1,100),  
         xlab="Percent Non-Exceedence (%)", ylab="Flow (cfs)")
    text((par()$usr[2]/2),10,  "No flow values available.") #Add error message in center of empty plot
    
    dev.off()  #close plot
    next
  }
  
  date <- as.Date(data$Date) #store vector of all dates 
  startdate <- min(date)
  enddate <- max(date)
  discharge <- as.numeric(as.vector(data$Flow)) #store vector of all discharge vals
  
  f3 <- zoo(as.numeric(as.vector(data$Flow)), order.by = as.Date(data$Date))
  
  # Omit any missing values from the data set and continuing on
  missing <- f3[!complete.cases(f3),]                     
  not_missing <- na.omit(f3) 
  
  # Check if all flow values are missing
  if (length(not_missing)==0) {
    NP <- seq(0, 100, 1) #make vector of all possible non-exceed probs
    sortflow <- rep(NA, times=length(NP)) #create vector of 0s for all NP values
    
    # Create plot file - showing ERROR
    file.name <- paste("usgs_", gage[i], "_alf_line_flowdur.png", sep="") #create name for file
    file.location <- paste("/var/www/html/images/dh/", file.name, sep="") #attach file name to proper directory
    write(c(), file = file.location)  #create file where image will be saved
    png(file.location)  #start writing to that file	
    
    plot(NP, sortflow, pch=20, log='y', main=gage[i], ylim=c(1,100),  
         xlab="Percent Non-Exceedence (%)", ylab="Flow (cfs)")
    text((par()$usr[2]/2),10,  "No flow values available.") #Add error message in center of empty plot
    
    dev.off()  #close plot
    next 
  }
  
  ## Create Flow Duration Curve
  #Sort/rank average daily discharges from largest to smallest value.
  sortflow <- sort(discharge, decreasing=TRUE)
  n <- length(sortflow)
  
  #Assign each discharge value a rank (M), starting with 1 for largest value.
  m <- 1:n
  
  #Calculate exceedence probability (P) as: P = 100 * [ M / (n + 1) ]
  P <- 100 * (m/(n+1))
  #Calculate non-exceedence probability (NP) as: NP = 100 - P
  NP <- 100 - P
  
  #-----------------------------------------------------------------------------------------
  #Calculate August Low Flow
  alf[i] <- fn_iha_mlf(f3,"August")
  
  ## Find Location of Aug Low Flow on Flow Duration Curve
  location.alf <- which.min(abs(sortflow-as.numeric(alf[i])))
  curve.alf <- sortflow[location.alf]
  NP.alf[i] <- NP[location.alf]
  
  #-----------------------------------------------------------------------------------------
  #Calculate 7q10
  x7q10 <- fn_iha_7q10(f3)
  
  ## Find Location of 7Q10 on Flow Duration Curve
  location.7q10 <- which.min(abs(sortflow-as.numeric(x7q10)))
  curve.7q10 <- sortflow[location.7q10]
  NP.7q10 <- NP[location.7q10]
  
  #-----------------------------------------------------------------------------------------
  
  ## Create Plot File
  file.name <- paste("/",ifim_site_name,"_usgs_", gage[i], "_flowdur.png", sep="") #create name for file
  file.location <- paste(save_directory, file.name, sep="") #attach file name to proper directory
  write(c(), file = file.location)  #create file where image will be saved
  png(file.location)  #start writing to that file	
  
  plot(NP, sortflow, pch=20, log='y', 
       main=paste(ifim_site_name, " IFIM Site\n", "Gage ",gage[i]," Area-Weighted Flow Duration Curve","\n",startdate," to ",enddate,sep=""), 
       
       xlab="Percent Non-Exceedence (%)", ylab="Flow (cfs)")
  #Add August Low Flow point and lines
  #points(NP.alf[i], curve.alf, pch=3, col="red")
  #abline(h=curve.alf, lty=2, col="red")
  #abline(v=NP.alf[i], lty=2, col="red")
  
  #abline(h=x7q10, lty=3, col="blue")
  #abline(v=NP.7q10, lty=3, col="blue")
  
  #abline(h=qmin, lty=1, col="gray")
  #abline(h=qmax, lty=1, col="gray")
  rect(-4, qmin, 104, qmax, col = "gray")
  
  points(NP, sortflow, pch=20)
  
  legend("topleft", c("Flow Duration Curve", 
                      paste("WUA Discharge Range = ",qmin," to ",qmax," cfs",sep="")), 
         pch=c(20, 15), col=c("black", "gray"), bg="white")
  
 # panel.first = rect(c(1,700), 100, c(3,10), 1e6, col='green', border=NA)
  #polygon(c(0:100, rev(0:100)), c(qmin, rev(qmax)),col = "grey30", border = NA)
  
  #legend("topleft", c("Flow Duration Curve", 
  #                    paste("ALF = ",curve.alf," cfs",sep=""), 
  #                    paste("7Q10 = ",round(x7q10,2)," cfs",sep="")), 
  #       pch=c(20, NA, NA), lty=c(NA, 2,3), col=c("black", "red", "blue"), bg="white")
  
  dev.off()  #close plot
}
} #end of ifim site for loop