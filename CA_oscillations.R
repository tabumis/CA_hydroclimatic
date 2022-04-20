#*******************************************************************************
# 
#  manuscript:   Umirbekov, A., Peña-Guerrero, M. D., & Müller, D. (2022).
#                Regionalization of climate teleconnections across Central Asian mountains
#                improves the predictability of seasonal precipitation. 
#                Environmental Research Letters, 17(5), 55002. https://doi.org/10.1088/1748-9326/ac6229
#
#  description:  Script for 1) Calculation of long-term annual and seasonal 
#                climatologies for the delineated precipitation subregions;
#                2) Estimation of Spearman rank correlation between climate 
#                oscillation indices and the seasonal precipitation; 3) Field
#                significance test via False Discovery Rate'; 4) Running
#                SVR-based model for seasonal predictions.  
#               
#                compiled in R version 4.1.1, last run on March 03, 2021
#
#  author:       Atabek Umirbekov
#
#*******************************************************************************

### Loading/installing necessary packages


loading<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

loading("rgdal","raster","rgeos", "readr", "dplyr","lubridate","anytime","tidyr",
        "RColorBrewer","gplots","gridExtra","circlize",
        "splitTools","e1071","Metrics","hydroGOF","rminer")

Sys.setlocale("LC_TIME", "English")

######################################################################################
## Setting the scene

# setting up the working directory

setwd(".../CA_oscillations") # indicate here a path to unzipped "CA_oscillations" folder


#uploading shapefiles of the deliniated precipitation subregions over Tian-Shan and Pamir mountains
{
  region_polygons<-rgdal:: readOGR(dsn = ".", layer = "subregions_final")
  # x11()
  plot(region_polygons,lwd=2)
  polygonsLabel(region_polygons, labels = region_polygons$Name,
                method = "centroid",col="blue")
  region_polygons
}

# Uploading MSWEP data for the study area extent
{
MSWEP<-raster::brick('MSWEP_stack.gri')
plot(MSWEP[[11]], main="Precipitation in Dec 1980")
plot(region_polygons, add=T)


dates<-MSWEP %>% 
  names() %>% 
  gsub("X","",.) %>% 
  as.numeric(.) %>%
  ym(.)  # string of dates
}



#################################################################################################
### 1.Exploring the long-term climatologies for the precipitation subregions

# Estimating annual and seasonal (Feb-Jun) precipitation means:

prcp_list<-list() # this list will keep monthly precipitation averaged for each subregion
for(i in region_polygons$Name){
  
  subregion =raster::subset(region_polygons, region_polygons$Name == i)
  plot(subregion)
  print(region_polygons$Name[region_polygons$Name== i])
  
  sb_prcp <- as.data.frame(t(raster:: extract(MSWEP, subregion, fun=mean, na.rm=TRUE)))
  colnames(sb_prcp)<-i
  

  prcp_list[i]<-sb_prcp
}

{  
  annual_means<-list()
  for(i in 1:length(prcp_list)){
    average_annual<-sum(prcp_list[[i]])/41.83333 # MSWEP data is from 02/1979 until 12/2020. This constitutes 41.8333 years
    annual_means[i]<-round(average_annual,0)
  }
  
  seasonal_means<-list()
  for(i in 1:length(prcp_list)){
    reg_precip<-as.data.frame(prcp_list[[i]])
    reg_precip$Date<-dates
    colnames(reg_precip)[1]<-c(i)
    
    
    average_season<-reg_precip%>%
      filter(month(Date) %in% c(2:6))%>% 
      group_by(Date)
    average_season<-round(sum(average_season[,1])/(42),0) # mean seasonal precipitation
    seasonal_means[i]<-average_season
    
  }
  
  climatology<-as.data.frame(do.call(cbind, list(region_polygons$Name,annual_means, seasonal_means)))
  colnames(climatology)<-c("subregion","mean annual prcp (mm)","mean seasonal prcp (mm)")
  
    
  print(climatology)
}
###Long-term monthly precipitation patterns as boxplots
{
  
  
  for(i in 1:length(prcp_list)){
    prcp_subregion<-as.data.frame(prcp_list[i])
    prcp_subregion$Date<-as.Date(dates, format="%Y-%m-%d")
    
    prcp_subregion<-prcp_subregion[-(1:11),]
    
    
    m <-xts:: xts(prcp_subregion[,-c(2)], order.by= prcp_subregion$Date)
    
    cmonth <- format(time(m), "%b")
    months <- factor(cmonth, levels=unique(cmonth), ordered=TRUE)
    myColors <- ifelse(levels(months)=="Feb", "#3182bd",
                       ifelse(levels(months)=="Mar", "#3182bd", 
                              ifelse(levels(months)=="Apr", "#3182bd",
                                     ifelse(levels(months)=="May","#3182bd",
                                            ifelse(levels(months)=="Jun", "#3182bd",
                                                   "#deebf7" ) ))))
    x11()
    boxplot(zoo::coredata(m) ~ months,
            col=myColors, ylim = c(0, 200),
            ylab="",xlab="",
            outwex=T,title="TRA")
    # tick <- seq_along(GRAPH$names)
    # axis(1, at = tick, labels = F)
    title(main=paste(colnames(prcp_subregion)[1]),line=-1.5,cex.main=1.5,font.main= 1)
    
  }
}
graphics.off()
rm(prcp_subregion,cmonth,i,m, months, myColors)

########################################################################################
## 2. Correlations between the climate oscillations and Feb-Jun precipitation


## Constructing subregion-averaged seasonal precipitation timeseries
{
  seasonal_list<-list()
  for(i in 1:length(prcp_list)){
    prcp_subregion<-as.data.frame(prcp_list[i])
    prcp_subregion$Date<-as.Date(dates, format="%Y-%m-%d")
    
    FMAMJ<-prcp_subregion%>%
      mutate(year = year(Date)) %>%
      filter(month(Date) %in% c(2:6))%>% 
      group_by(year)%>%
      summarise_at(vars(-Date), funs(sum(.)))
    seasonal_list[i]<-FMAMJ[,2]
  }
  
  seasonal_prcp<-as.data.frame(seasonal_list)
  colnames(seasonal_prcp)<-region_polygons$Name
  seasonal_prcp$year<-FMAMJ$year
  
  # print(round(seasonal_prcp[c(1:8),]))
  rm(FMAMJ,prcp_subregion,i,seasonal_list )
}

## Calculating Spearman correlation coefficients between oscillation indices at different lag-times and the seasonal precipitation 
## using SOI index as an example 

# upload a file with climate oscillations timeseries: 
indices <- read_csv("processed_indices.csv", 
                    col_types = cols(...1 = col_skip())) #the monthly indices are arranged in a wide table format, with each row corresponding to each observation year.


# Extracting monthly indices of SOI
soim1<-indices %>%
  dplyr:: select(starts_with(c("year","soi")))%>% # to check other oscillation indices replace "soi" with respective term, e.g. for use "pdo" for PDO and so on
  na.omit()

{
  # Running Spearman rank correlation between monthly SOI indices and the subregion-averaged seasonal precipitation 
  cor_mat<-matrix(, nrow = 17, ncol = 7)
  p_value<-matrix(, nrow = 17, ncol = 7)
  
  for(i in 1:7){
    subregion_prcp<-seasonal_prcp[i]
    subregion_prcp$year<-seasonal_prcp$year
    df_merged<-merge(subregion_prcp,soim1, by="year")
    
    nino<-cor(df_merged,method = "spearman")
    cor_mat[,i]<-nino[2,-c(1:3)]
    
    testRes =corrplot::cor.mtest(df_merged, method = "spearman")
    p_value[,i]<-testRes$p[2,-c(1:3)]
    
    
  }
  
  # Visualizing the Spearman correlation coefficients as a heatmap
  rownames(cor_mat)<-c( "Feb","Mar", "Apr","May", "Jun",
                        "Jul", "Aug","Sep", "Oct","Nov", "Dec","Jan",
                        "Feb","Mar","Apr","May","Jun")
  colnames(cor_mat)<-colnames(seasonal_prcp[-c(8,9)])
  
  colnames(p_value)<-colnames(cor_mat)
  rownames(p_value)<-rownames(cor_mat)
  
  library(corrplot)
  library(RColorBrewer)
  
  col2<-rev(c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','white',
              'white','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'))
  
  
  corplot1<-corrplot::corrplot(cor_mat, p.mat = p_value, is.corr=FALSE,method = 'color',oder="original",
                               tl.col="black",
                               sig.level = c(0.001,0.01,0.05), # these are significance levels that will be indicated as asteriks in the plot 
                               pch.cex = 0.99,pch.col="black",
                               insig = 'label_sig', 
                               col.lim = c(-0.6, 0.6),
                               col=col2,
                               addgrid.col='black',
                               # cl.pos="n"
  )
  }

## Running pixel-level Spearman correlation between the monthly SOI indices and 

# Function to estimate pixel-wise Spearman correlations and p-values from raster stacks, by:
# Abdi H. et al (2016) The El Niño - La Niña cycle and recent trends in supply and demand of net primary productivity in African drylands. Climatic Change 138, 111-125 (2016). https://doi.org/10.1007/s10584-016-1730-1
gridcorts <- function(rasterstack, method, type=c("corel","pval","both")){
  # Values for (layers, ncell, ncol, nrow, method, crs, extent) come straight from the input raster stack
  # e.g. nlayers(rasterstack), ncell(rasterstack)... etc.
  print(paste("Start Gridcorts:",Sys.time()))
  print("Loading parameters")
  layers=nlayers(rasterstack);ncell=ncell(rasterstack);
  ncol=ncol(rasterstack);nrow=nrow(rasterstack);crs=crs(rasterstack);
  extent=extent(rasterstack);pb = txtProgressBar(min = 0, max = ncell, initial = 0)
  print("Done loading parameters")
  mtrx <- as.matrix(rasterstack,ncol=layers)
  empt <- matrix(nrow=ncell, ncol=2)
  print("Initiating loop operation")
  if (type == "corel"){
    for (i in 1:ncell){
      setTxtProgressBar(pb,i)
      if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
        empt[i,1] <- NA 
      } else 
        if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
          empt[i,1] <- NA 
        } else 
          empt[i,1] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$estimate)
    }
    print("Creating empty raster")
    corel <- raster(nrows=nrow,ncols=ncol,crs=crs)
    extent(corel) <- extent
    print("Populating correlation raster")
    values(corel) <- empt[,1]
    print(paste("Ending Gridcorts on",Sys.time()))
    corel
  } 
  else
    if (type == "pval"){
      for (i in 1:ncell){
        setTxtProgressBar(pb,i)
        if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
          empt[i,2] <- NA 
        } else 
          if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
            empt[i,2] <- NA 
          } else 
            empt[i,2] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$p.value)
      }
      pval <- raster(nrows=nrow,ncols=ncol,crs=crs)
      extent(pval) <- extent
      print("Populating significance raster")
      values(pval) <- empt[,2]
      print(paste("Ending Gridcorts on",Sys.time()))
      pval
    }
  else
    if (type == "both"){
      for (i in 1:ncell){
        setTxtProgressBar(pb,i)
        if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
          empt[i,] <- NA 
        } else 
          if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
            empt[i,] <- NA 
          } else {
            empt[i,1] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$estimate) 
            empt[i,2] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$p.value)
          }
      }
      c <- raster(nrows=nrow,ncols=ncol,crs=crs)
      p <- raster(nrows=nrow,ncols=ncol,crs=crs)
      print("Populating raster brick")
      values(c) <- empt[,1]
      values(p) <- empt[,2]
      brk <- brick(c,p)
      extent(brk) <- extent
      names(brk) <- c("Correlation","Pvalue")
      print(paste("Ending Gridcorts on",Sys.time()))
      brk
    }
}

#  We will first convert monthly MSWEP data to seasonal.

{
season_year<-dates[(month(dates) >1) & (month(dates) <7)] # this to 

season_date<-paste("X",season_year, sep="")
season_date<-stringr:: str_replace_all(season_date, "-", "")
season_date<-stringr:: str_sub(season_date, , end=-3)
} # this creates a string of indices that correspond to the months we need to exract from "MSWEP" raster stack file.

season_months_prcp <- raster::subset(MSWEP, season_date) # extracting the months we need, i.e.  Feb 1979, Mar 1979,...June 2020. 5 months X 42 years = 210 layers in the new raster stack 
season_ind<-season_year %>%
  year()  # string of year indices for aggregation, i.e. summing up all selected months within each year

MSWEP_seasonal<- stackApply(season_months_prcp,season_ind , fun = sum) 
MSWEP_seasonal# raster stack of the seasonal precipitation across the study area
{
climatology<-mean(MSWEP_seasonal) 
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")) # colorscale for plotting
sp::plot(climatology,col=(jet.colors(10)), interpolate=F,
         main="Average precipitation during Feb-June season",legend.args=list(text='mm', side=4,font=1, line=2.5, cex=1.1),
         xlim = c(60, 80), ylim = c(35, 45))
plot(region_polygons, lwd=4, add=TRUE)
} # Longterm averages of the seasonal precipitation (spatial distribution)

# Now lets create a raster stack of climate oscillation indices

indices_subset<-indices[indices$year >1978 &indices$year <2021, ] # subsetting indices from 1979 to 2020 for conformity with the seasonal precipitation data
target_oscillation<- indices_subset[,c("year","soi12_lag_1")]# # selecting SOI (December) values for analysis.To check other oscillation indices replace "soi" with respective term, e.g. for use "pdo" for PDO and so on


oscillation<-MSWEP_seasonal # lets create a blank raster stack file which will be consistent with the "MSWEP_seasonal"

for(i in 1:nlayers(oscillation)){
  osc_as_matrix<-matrix(target_oscillation[i,-1], nrow = 100, ncol = 150)
  oscillation[[i]]<-setValues(oscillation[[i]], as.numeric(osc_as_matrix))
} # this loop assigns SOI (Dec) values to the created raster stack file. Each layer in the stack corresponds to each year from 1979 to 2020. All cell values in a single layer are equal 
oscillation

r_stack<-stack(MSWEP_seasonal,oscillation) # Now we combine two rasterstack files ( seasonal precipitation and oscillation indices) into a single ratserstack file  

spearman<-gridcorts(rasterstack = r_stack, method = "spearman", type = "both")
plot(spearman) # maps of (1) correlations and p-values across the study area

# Plot of correlations significance at p=0.05

m <- c(0, 0.05, 1,  0.05, Inf, 0) # reclassification condition which nulifies all p-values greater than 0.05
rclmat <- matrix(m, ncol=3, byrow=TRUE)
pval_binary <- reclassify(spearman[[2]], rclmat)

dfr<-spearman[[1]]*pval_binary # masking out correlation coefficients across the study area, where p-value > 0.05

{  
    cuts = c(0.6,0.5,0.4,0.3,0.2, 0.1, 0, -0.1, -0.2, -0.3, -0.4, -0.5, -0.6)
  
  
  plot(dfr, breaks=cuts, col=col2,
       main="pixel-wise Spearman`s correlation between SOI state in December \n and February-June  precipitation ",
       font.main = 1) 
  plot(region_polygons, lwd=3,border="black", add=T)
}
########################################################################################
###  3. Field significance test via False Discovery Rate (Wilks 2006)

# We use the function "fdrTest" developed by  Lorenz,et al (2016), Does Amazonian deforestation cause global effects; can we be sure?, J. Geophys. Res. Atmos., 121, 5567-5584,doi:10.1002/2015JD024357.
# function code is accessible at https://github.com/ruthlorenz/stat_tests_correlated_climdata/blob/master/FStests/false_discovery_rate_package.R
# p_val = P-values at every grid point, h_val = 0 or 1 at every grid point, depending on significance level
# K = total number of local t-test, nlon*nlat if all grid points included,


fdrTest <- function(p_val, siglev=0.05, ...){
  
  dims_p <- dim(p_val)
  nlon<-dims_p[1]
  nlat<-dims_p[2]
  if (!is.na(dims_p[3])){
    ntim<-dims_p[3]
  } else {
    ntim<-1
    tmp<-p_val
    p_val<-array(NA,dim=c(nlon,nlat,ntim))
    p_val[,,1]<-tmp
  }
  
  h_val<-array(NA,dim=c(nlon,nlat,ntim))
  
  for (t in 1:ntim){
    for (lat in 1:nlat){
      for (lon in 1:nlon){
        if (is.na(p_val[lon,lat,t])){
          h_val[lon,lat,t]<-NA
        } else if (p_val[lon,lat,t] < siglev){
          h_val[lon,lat,t]<-1
        } else {
          h_val[lon,lat,t]<-0
        }
      }
    }
  }
  
  K<-sum(!is.na(p_val[,,1]))
  
  fdr<-array(0,dim=c(nlon,nlat,ntim))
  sig_FDR<-array(0,dim=c(nlon,nlat,ntim))
  p<-array(NA,dim=c(nlon*nlat))
  
  #put all p-values in 1D vector
  prob_1D<-(c(p_val))
  
  # sort vector increasing
  p_sort<-sort(prob_1D, decreasing = FALSE)
  
  # reject those local tests for which max[p(k)<=(siglev^(k/K)]
  for (k in 1:K){
    if (p_sort[k]<=(siglev*(k/K))){
      p[k]<-p_sort[k]
    } else {
      p[k]<-0.0
    }
  }
  p_fdr<-max(p,na.rm=T)
  
  fdr[which(p_val<=p_fdr)] <- 1
  sig_FDR[which(fdr==1 & h_val==1)] <- 1
  
  sig_pts<-array(NA,dim=c(ntim))
  for (j in 1:ntim){
    sig_pts[j]<-(sum(sig_FDR[,,j],na.rm=T))
  }
  
  method <- paste("False Discovery Rate for field significance")
  rval <- list(h.value=sig_FDR, p.value=p_val, field.sig = siglev,
               nr.sigpt=sig_pts, total.test=K, method=method, call=match.call())
  class(rval) <- "fdrFS"
  return(rval)
}


## Groupping p-values by subregions
pvalues_subregion<-list() #this list will store raw and adjusted p-values for each subregion
FDR_test<-list() # this list will store proportion of significant local tests out of total in each subregion
for(i in region_polygons$Name){
  
  subregion =raster::subset(region_polygons, region_polygons$Name == i)
  plot(subregion)
  print(region_polygons$Name[region_polygons$Name== i])
  rasta<-mask(crop(spearman[[2]], subregion),subregion)
  nK<-cellStats(rasta, function(i, ...) sum(!is.na(i)))
  pvalues_subregion[[i]] <-as.matrix(mask(crop(spearman[[2]], subregion),subregion), xy=TRUE)
  pvalues_subregion[[i]] <-fdrTest(pvalues_subregion[[i]],siglev=0.1, K=nk) 
  FDR_test[i]<-round(pvalues_subregion[[i]][["nr.sigpt"]]/pvalues_subregion[[i]][["total.test"]],2)*100
}
FDR_test


##########################################################################################
###  4. Assessment of the seasonal precipitation predictability with support vector regression (SVR)
#(using Western Tian-Shan subregion as a case study)

DATA<-merge(seasonal_prcp,indices, by="year")
# rm(list=ls()[! ls() %in% c("DATA")])


# Extracting variables for the Western Tian-Shan: 
df<-DATA[,c("West Tian-Shan","soi12_lag_1" , "l_pdo11_lag_1" , "eawr10_lag_1" ,  
            "nao12_lag_1" )]
colnames(df)[1]<-"seasonal_prcp"
str(df) #the df dataframe now contains timeserie of predictant variable ("seasonal_prcp) and a set of predictor variables (the oscillation indices at their dominant lead times)

# Splitting the data into training and validation samples, by the stratified sampling approach: 
inds <-splitTools:: partition(df$seasonal_prcp, p = c(train = 0.8, valid = 0.2), seed=1)
dataset <- df[inds$train, ] # training set that contains 80% or 34 observations of the seasonal precipitation 
testdata <- df[inds$valid, ]# validation set that contains 20% or 8 observations of the seasonal precipitation

# Running SVR (radial basis function kernel) with cost and gamma parameters,

svm_model <-  svm(formula = seasonal_prcp ~., data = dataset, type = "nu-regression", 
                  kernel = "radial",
                  gamma = 0.02666013, cost=	4.302088, # gamma and cost parameters were estimated during the calibration process
                  scale = TRUE)
summary(svm_model)


# Performance of the SVR calibrated on the training sample:
predicted_trainset <- predict(svm_model, dataset) 

#MAPE
Metrics::mape(dataset$seasonal_prcp,predicted_trainset) 
# Squared coefficient of determination
hydroGOF::br2(predicted_trainset, dataset$seasonal_prcp)
#KGE
hydroGOF:: KGE(predicted_trainset, dataset$seasonal_prcp)
hydroGOF:: ggof(predicted_trainset, dataset$seasonal_prcp)

# Evaluation of the SVR model on the validation sample:
predicted_validset <- predict(svm_model, testdata)

#MAPE
Metrics::mape(predicted_validset,testdata$seasonal_prcp) 
# GOF
ggof(predicted_validset,testdata$seasonal_prcp) 

# Evaluating the SVR on the full data 
predicted_full <- predict(svm_model, df)
hydroGOF:: ggof(predicted_full, df$seasonal_prcp)

# To check performance of the SVR models for other subregions, the set of selected predictors in "df" should be modified
# so that it includes the oscillation indices  relevant to target subregion. Please check Table 3 in the original 
# manuscript for exact set of climate oscillations and their dominant lead times for each subregion, and change line 485 accordingly.
# The SVR cost and gamma coefficients (line 499) are also determined for each subregion separately, and can be retrieved 
# from "SVR_parameters.xlxs" file. 


