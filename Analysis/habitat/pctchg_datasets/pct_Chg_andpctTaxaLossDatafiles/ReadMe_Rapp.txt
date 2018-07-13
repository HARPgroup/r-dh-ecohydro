I wrote an R script that calculates the #taxa loss and %taxaloss for any regression summary stats list.  It’s named: [taxaloss_calcsfromRegstats.R]

 Input file: [IFIMSITES_HUC8530BPJMonthly_RegressionStats.csv] is the regression statistics joined to the list of IFIM stations, so it’s reduced from [ELF_Stats_Breakpoints.all.all.all.csv] which contains everything that BPJ 530 has for huc8 huc10 etc… 

Output file = [IFIM_TaxaLossMonthlyresults.20180711.csv]  This file contains results for nt total and a few biometrics for fish for all months and maf and da.  I will begin by extracting the NTTotal for all flows to join with the pctchange habitat info.  

Subset file of only NT_Total results
[IFIMTaxaloss-PctChg_Monthlyflows_NTTotal.csv]

