#*******************************************************************************
# 
#  R code and corresponding files to themanuscript:  
#               Umirbekov, A., Peña-Guerrero, M. D., & Müller, D. (2022).
#               Regionalization of climate teleconnections across Central Asian mountains
#               improves the predictability of seasonal precipitation. 
#               Environmental Research Letters, 17(5), 55002. https://doi.org/10.1088/1748-9326/ac6229
#*******************************************************************************

File "CA_oscillations.rds" contains the R script of workflow for: 
	        1) Calculation of long-term annual and seasonal 
                climatologies for the delineated precipitation subregions;
                2) Estimation of Spearman rank correlation between climate 
                oscillation indices and the seasonal precipitation;
	        3) Field significance test via False Discovery Rate'; 
                4) Running SVR-based model for seasonal predictions.

File "subregion_final.shp" is the shapefile of the deliniated precipitation subregions

File "processed_indices.csv" contains indices of climate oscillations investigated in the study.
		All indices were rearranged with respect to their lead-lag times to the seasonal 
		precipitation (totals for Feb-June preciputation). Index name convention:
                - all indices contain three character string at the beginning or middle that corresponds to
		  abrreviation of climate ocillations. Eg. "soi" refers to SOI or Southern Oscillation Index"
		- "_lag_1" suffix denotes a lag with respect to the target year. E.g. "soi12_lag_1" refers 
		  to SOI index in December (i.e. December of the last year)
		- the file also contains 3-month averages of the monthly indices, in such cases they start with
		  prefix "l_". E.g. "l_pdo11_lag_1" means 3-month average of the PDO (Pacific Decadal Oscillation),
                  for Sep, Oct and Nov ("11" here corresponds to the last of the month) in the last year.


File "SVR_parameters.xlsx" contains gamma and cost parameters for each subregion`s SVR model. 
 
