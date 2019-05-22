##Diagnostics for Regressions adapted from icthymaps script
##Scripts to create ELF plots likethose designed by DEQ
###updated by JLRAPP 07/18/2018
#Re-run lines 184-194 after the full ELF process has run to get the reported values in the console
#or in the plot area. 
#expand plot to a very large window, or make a separate output so that the four-part plots draw.

###
remove(list=ls())
graphics.off() 
setwd("D:\\Jkrstolic\\R\\deqEcoflows\\Regression_diagnostic example\\")


IcthyData = read.csv(file="maximum_NT_Total_FISH.csv",header=TRUE) # Reg_diag_HUC10data.csv

require("quantreg")
library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);
library(car);
require ("data.table")
library(relimp, pos=28)
#grab unique list of HUCs or regions from the column of interest, or insert a list.. 
#List doesn't work for regression diagnostics. I would run them one at a time.  
### add code to create a data file to send regression terms to###
###Store Quantile REgression stats first then Linear Regression Stats second###
QregTerms<- data.frame(HUC = "", linemodR2 = NA, linemodP = NA, linemodB = NA, linemodM = NA, linemodN = NA, quantpInt = NA, quantP = NA, quantB = NA, quantM = NA, quantN = NA,
                      stringsAsFactors = FALSE)
#HUC ->60102,60101,50702,50500,30401,30102,20802,20700,30101,20403,20801
RegionsList  = list(020801) #0207000409, 0207001004, 0208010201, 0208010301, 0208020305, 0208020602, 0208020604,0301010103, 0601010203 

for (A in RegionsList) {

  myData <- (IcthyData)
  data.table.Region <- data.table(myData)
  data.Region <- data.table.Region[HUC6 ==  A ] #60102] [HUC10 ==  A ] change depending on which hucs you run

  
  xlab <- "log_MAF"
  ylab <-"NT_Taxa"
  HUC <- A
  #HUC <- 60102
  
  Varj <-data.Region$y_value
  
  Vari <- (data.Region$x_value)
 
  
  myRQ.tau        <- 0.80
  myReg.decimals  <- 2
  myReg.decimals.p <- 3
  myText.Reg.cex  <- 1
  myText.Reg.line <- 0.1
  myText.Reg.side <- 3
  myQuant.color <- "dark blue"
  fun.info.date         <- format(Sys.Date(),"%Y%m%d")
  fun.dir <- getwd()
  fun.FileExt <- "PNG" 
  myText.Reg.cex  <- 1
  myText.Reg.line <- 0.1
  myText.RQ.line <- 2
  myText.Reg.side <- 3
  myText.RQ.side <- 1
  myRQ.col        <- "green"
  myRQ.lty        <- "solid"
  myReg.lty       <- "solid"
  myCI.col        <- "red"
  myReg.decimals  <- 2
  myReg.decimals.p <- 3
  myRQ.min.N       <- 7
  myReg.col.neg    <- "green"
  myPI.col         <- "gray"
  
  
  ##Quantile Regression: 
  myRQ <- rq(Varj ~ log(Vari), subset= (data.Region$x_value <500 & data.Region$x_value >0.01), tau=myRQ.tau)
  predict.rq(myRQ)
  #added from Joey's
  newy <- c((log(Vari) * coef(myRQ)[2]) + coef(myRQ)[1])            #find the upper quantile values of y for each value of DA based on the quantile regression
  upper.quant <- subset(data.Region, Varj > newy)                        #create a subset of the data that only includes the stations with NT values higher than the y values just calculated
  print(paste("Upper quantile has ", nrow(upper.quant), "values"));
  
  ##Linear Regression of the upper 20% of data
  regupper <- lm(upper.quant$y_value ~ log(upper.quant$x_value),data = upper.quant)  
  ru <- summary(regupper)                                                  #regression for upper quantile
  ruconfInt <- predict(regupper, data = upper.quant, interval="confidence") 
  ruPredInt <- predict(regupper, data = upper.quant, interval="prediction") 
  
  #mean of all Y values
  meanY <- mean(newy)    #calculate mean
  print (paste("mean of all upper quantile values = ", meanY));
  
  #Summary parameters stored in variables
  #ruconfInt
  myReg.r.neg <- summary(regupper)$r.squared
  myReg.p.neg <- summary(regupper)$coefficients[8]
  myReg.b.neg <- summary(regupper)$coefficients[1]
  myReg.m.neg <- summary(regupper)$coefficients[2]
  rucount <- length(upper.quant$y_value)
  #mtext(round(myReg.r.neg,myReg.decimals), side=myText.Reg.side,line=myText.Reg.line+1,cex=myText.Reg.cex, adj=0, col=myReg.col.neg)
  #mtext(round(myReg.p.neg,myReg.decimals.p), side=myText.Reg.side,line=myText.Reg.line,cex=myText.Reg.cex, adj=0, col=myReg.col.neg)
  
  
  myRQ.b.pos <- summary(myRQ)$coefficients[1]
  myRQ.m.pos <- summary(myRQ)$coefficients[2]
  myRQ.p.pos <- summary(myRQ)$coefficients[8] #8 is the pvalue for slope
  myRQ.pInt.pos <- summary(myRQ)$coefficients[7] #7 is the pvalue for intercept
  myRQ.line.x0.pos <- 0  #was 0min(Vari)
  myRQ.line.x1.pos <- max(Vari)
  myRQ.line.y0.pos <- myRQ.m.pos * myRQ.line.x0.pos + myRQ.b.pos
  myRQ.line.y1.pos <- myRQ.m.pos * myRQ.line.x1.pos + myRQ.b.pos
  fullcount <- length(data.Region$y_value)
  #segments(myRQ.line.x0.pos, myRQ.line.y0.pos, myRQ.line.x1.pos, myRQ.line.y1.pos, myRQ.col, lty=myRQ.lty)
  #mtext(round(myRQ$coefficients[2],myReg.decimals),side=myText.RQ.side,line=myText.RQ.line,cex=myText.Reg.cex , adj=1, col=myRQ.col)
  
  
  
 
  #Plot titles
  plot_title <- paste("IcthyMaps_ELF ", ylab,"_",xlab, " HUC = 0",HUC, sep="");#Feature.Name," (",sampres," grouping)\n",startdate," to ",enddate,"\n\nQuantile Regression: (breakpoint at DA = 500 sqkm)",sep=""); #,"\n","\n",search_code,"  (",y_metric,")  vs  (",x_metric,")","\n",sep="");
  xaxis_title <- paste(xlab, "\n","\n","m: ",signif(myReg.m.neg, digits = 3),"    b: ",signif(myReg.b.neg, digits = 3),"    r^2: ",signif(myReg.r.neg, digits = 3), "    p: ",signif(myReg.p.neg, digits = 3),"\n","    Upper ",((1 -  myRQ.tau)*100),"% n: ",signif(rucount, digits = 3),"    Data Subset n: ",signif(fullcount, digits = 3),sep="");
  yaxis_title <- paste(ylab);
  EDAS_upper_legend <- paste("Data Subset (Upper ",((1 - myRQ.tau)*100),"%)",sep="");
  Reg_upper_legend <- paste("Regression (Upper ",((1 - myRQ.tau)*100),"%)",sep="");       
  Quantile_Legend <- paste(myRQ.tau," Quantile (Data Subset)",sep=""); 
  EDAS_lower_legend <- paste("Data Subset (Lower ",(100-((1 - myRQ.tau)*100)),"%)",sep="");
  
  print (paste("Plotting ELF"));
  # START - plotting function
  plt <- ggplot(data.Region, aes(x=Vari,y=Varj)) +  ylim(0,max(Varj)) + 
    geom_point(data = data.Region,aes(colour="aliceblue")) + #kept the region data set instead of full dataset
    geom_point(data =  data.Region,aes(colour="blue")) + 
    stat_smooth(method = "lm",formula = y ~ x, fullrange=FALSE,level = .95, data = upper.quant, aes(x=upper.quant$x_value, y=upper.quant$y_value, color = "red")) +
    geom_point(data = upper.quant, aes(x=upper.quant$x_value, y= upper.quant$y_value,color = "black")) + 
    geom_quantile(data = data.Region, quantiles= myRQ.tau,show.legend = TRUE,aes(color="red")) + 
    geom_smooth(data =  data.Region, method="lm",formula = y ~ x, show.legend = TRUE, aes(x=Vari,y=Varj, colour="yellow"),se=FALSE) + 
    geom_smooth(data = upper.quant, method = "lm", formula = y ~ x,  show.legend = TRUE, aes(x=upper.quant$x_value, y= upper.quant$y_value,color = "green"),se=FALSE) + 
    ggtitle(plot_title) + 
  
      #***********
    theme(
      plot.title = element_text(size = 12, face = "bold"),axis.text = element_text(colour = "blue")) +
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
         linetype=c(0,0, 0,1,1,1),
          shape=c(16,16,16,NA,NA,NA)
        ),
        label.position = "right"
      )
    );
  
  # END plotting function
  fun.file <- paste("Quantile_regressionHUC10.0", HUC,"_", ylab,"_",xlab, sep="")
  fun.info.filelocation <- paste(fun.dir,fun.file,sep="/")
  filename <- paste(fun.info.filelocation,".",fun.info.date,".",fun.FileExt,sep="")
   ggsave(file=filename, width=8, height=6, bg="white", pointsize=14)
   #png(filename=File.Out, width=678, height=678, bg="white", pointsize=14)
  #ggsave("icthyPlot.png",width=8, height=6, bg="white", pointsize =14)
  
   #####
   #Re-run these after the full ELF process has run to get the reported values in the console
   #or in the plot area. 
   #expand plot to a very large window, or make a separate output so that the four-part plots draw.
   # Regression Diagnostics here
   summary(regupper)
   oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
   plot(regupper)
   par(oldpar)
   
   shapiro.test(residuals (regupper))
   
   #influence stats
   inflm.ELF <- influence.measures(regupper)
   which(apply(inflm.ELF$is.inf, 1, any))   #identifies the points that are influence points
   summary(inflm.ELF) # only these
   
   
   avPlots(regupper, terms=~., intercept=FALSE, layout=NULL, ask, main, ...)
   
   avp(...)
   
   avPlot(regupper, ...)
   
   ## added variable plots 
   avPlots(regupper, ~ log(upper.quant$x_value), ellipse=TRUE) # av Plot, adjusting for others
   AVconfInt <- predict(regupper, data = upper.quant, interval="confidence")
   avPlot(regupper, log(upper.quant$x_value),
          id=TRUE, col = carPalette()[1], col.lines = carPalette()[2],
          xlab, ylab, pch = 1, lwd = 2, 
          main=paste("Added-Variable Plot:", log(upper.quant$x_value)),
          grid=TRUE,
          ellipse=TRUE,
          marginal.scale=FALSE)
   
   
  ###
   
   #save the regression parameters to a data frame and ultimately to a csv file.
  #define the data frame of 1 row, then send the result to append to the data frame at the beginning of the function
  ThisQregTerms<- data.frame(HUC = "", linemodR2 = NA, linemodP = NA, linemodB = NA, linemodM = NA, linemodN = NA, quantpInt = NA, quantP = NA, quantB = NA, quantM = NA, quantN = NA,
                              stringsAsFactors = FALSE)
  #
  ThisQregTerms[1, "HUC"] <- HUC
  ###Linear Regression terms
  ThisQregTerms[1, "linemodR2"] <- round(myReg.r.neg, myReg.decimals.p)
  ThisQregTerms[1, "linemodP"] <- round(myReg.p.neg, myReg.decimals.p)
  ThisQregTerms[1, "linemodB"] <- round(myReg.b.neg, myReg.decimals.p)
  ThisQregTerms[1, "linemodM"] <- round(myReg.m.neg, myReg.decimals.p)
  ThisQregTerms[1,"linemodN"] <- rucount  #grab n values from linear model of the upper quantile
  ####Quantile regression terms
  ThisQregTerms[1, "quantpInt"] <- round(myRQ.pInt.pos, myReg.decimals.p)
  ThisQregTerms[1, "quantP"] <- round(myRQ.p.pos, myReg.decimals.p)
  ThisQregTerms[1, "quantB"] <- round(myRQ.b.pos, myReg.decimals.p)
  ThisQregTerms[1, "quantM"] <- round(myRQ.m.pos, myReg.decimals.p)
  ThisQregTerms[1,"quantN"] <- fullcount  # n values from full data set used for quantile regression model
  QregTerms = rbind(QregTerms, ThisQregTerms)

#dev.off()
write.csv(QregTerms, file = paste("RegDiagStatsHUC6.",A, ".", fun.info.date,".csv",sep=""), row.names = TRUE, quote = FALSE)# save to file for each different dataset called
}
#