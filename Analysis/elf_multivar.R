library(quantreg);

options(timeout=2048);
library('gdata')
source("/var/www/R/config.local.private");
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
datasite <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh

# Load Default inputs
source(paste(fxn_locations,"elf_default_inputs.R", sep = ""));
#Load Functions               
source(paste(fxn_locations,"elf_retrieve_data.R", sep = ""));  #loads function used to retrieve F:E data from VAHydro
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
source(paste(fxn_locations,"elf_assemble_batch.R", sep = ""));

bundle <- 'watershed'; # ecoregion, watershed, landunit
ftype <- 'nhd_huc6'; # state, hwi_region, nhd_huc8, nhd_huc6, ecoregion_iii, ecoregion_iv, ecoiii_huc6
metric <- 'aqbio_nt_minnow';
#selected <- '0301010104';
#selected <- '030101';
#selected <- '020801';
selected <- '060102';
#selected <- 'nhd_huc8_06010205';
#selected <- 'all';
sampres = 'species';
quantile <- 0.8;
inputs$xaxis_thresh = 530;

# vahydro_fe_data(Watershed_Hydrocode,x_metric_code,
#  y_metric_code,bundle,ws_ftype_code,sampres, data, datasite = '') 
data <- vahydro_fe_multi_data(
  bundle = bundle, 
  ftype = ftype,
  metric = metric,
  selected = selected,
  datasite = datasite
);

data["ratio"] <- (data$da_sqmi)/(data$annual)
data["qnorm"] <- (data$annual)/log(data$da_sqmi)
dme <- subset(data, da_sqmi >= .001 & da_sqmi < inputs$xaxis_thresh);

#calculate the quantile regression
up90 <- rq(metric_value ~ log(annual),data = dme, tau = quantile)
#find the values of upper quantile values of y for each value of DA based on the quantile regression
newy <- c(log(dme$annual)*coef(up90)[2]+coef(up90)[1])
#create a subset of the data that only includes the stations with NT values higher than the y values just calculated
upper.quant <- subset(dme, dme$metric_value > newy)
# DA only
fit1 <- lm(metric_value ~ log(da_sqmi), data = upper.quant)
summary(fit1);

fit2 <- lm(metric_value ~ log(da_sqmi) + log(annual), data = upper.quant)
summary(fit2);

fit3 <- lm(metric_value ~ log(da_sqmi) + log(jun) + log(jul), data = upper.quant)
summary(fit3);

fit2norm <- lm(metric_value ~ log(da_sqmi) + qnorm, data = upper.quant)
summary(fit2norm);

library(MASS)
fit14 <- lm(metric_value ~ log(da_sqmi) + log(jan) + log(feb) + 
            log(mar) + log(apr) + log(may)
          + log(jun) + log(jul) + log(aug) 
          + log(sep) + log(oct) + log(nov) + log(dec)
          ,data=upper.quant
)
step <- stepAIC(fit14, k=4, direction="both")
step$anova # display results
summary(step)
