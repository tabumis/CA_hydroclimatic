library(HiClimR)
library(readr)
library(dplyr)
library(lubridate)

#### average seasonal precipitation map across the study domain
averages<- read_csv("C:/Users/User/Downloads/gpcc_avJFMAM_1950_1990.csv")

averages<-averages[,-3]
averages$`Total Precipitation`<-averages$`Total Precipitation`*5 #convert the average to sums
coordinates(averages) <- ~Longitude+Latitude 
gridded(averages) <- TRUE
climatology<-raster(averages)
crs(climatology) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#plot the map
library(maps)
colfunc<-colorRampPalette(c("white","lightblue","cornflowerblue","yellow","red","darkred")) # colorscale for plotting
p<-plot(climatology,col=(colfunc(100)), interpolate=F,
     main="Total precipitation for Jan-May",legend.args=list(text='mm', side=4,font=1, line=2.5, cex=1.1),
     xlab = "Longitude (°)", ylab = "Latitude (°)",
     xlim = c(60, 80), ylim = c(35, 45))

map('world', xlim = c(60, 80), ylim = c(35, 45), add = TRUE, lwd=0.5, col = "black")

writeRaster(climatology,'C:/Users/Umirbekov/Desktop/climatology_JFMAM.tif',overwrite=TRUE)

#####################START HERE#######################################################################

# load GPCC data for the domain (60,80,35,45) and mark NAs (-999)
CA_GPCC <- read_csv("D:/CA_precip/gpcc_1949_2016.csv", 
                       col_types = cols(Time = col_date(format = "%Y-%m")))%>%
  naniar::replace_with_na(.,replace = list(`Total Precipitation` = -999)) 

# subset Jan-May precipitation values and transfrom from long to wide, with rows as timeserie per lat/lon
gpcc_JFMAM<-CA_GPCC%>% filter(month(Time) %in% c(1:5))%>% 
  tidyr::spread(Time, `Total Precipitation`)



## Generate/check longitude and latitude mesh vectors for gridded data
{
x<-gpcc_JFMAM[,-c(0:2)]
x<-as.matrix(x)
lon<-CA_GPCC$Longitude
lat<-CA_GPCC$Latitude
}

## Single-Variate Hierarchical Climate Regionalization (use the "complete" method, and 9 clusters)

y <- HiClimR(x, lon = lon, lat = lat, detrend = F,
             standardize = F,  method = "ward", hybrid = F, kH = NULL,contigConst = 0, 
             members = NULL,nSplit = 1, upperTri = TRUE, verbose = TRUE,  
             validClimR = T, k=9, minSize = 1, alpha = 0.1, 
             plot = TRUE, colPalette = rainbow, hang = -1, labels = FALSE,cex=1)


## Export regions map and mean timeseries into NetCDF-4 file
library(ncdf4)
y.nc <- HiClimR2nc(y=y, ncfile="D:/CA_precip/HiClimR_complete_9.nc",
                   timeunit="months", dataunit="mm")
## The NetCDF-4 file is still open to add other variables or close it
nc_close(y.nc)
nc_data <- nc_open('D:/CA_precip/HiClimR_complete_9.nc')


## Read the netcdf file as raster and vectorize it to polygons
library(raster)
{
d <- raster("D:/CA_precip/HiClimR_complete_9.nc")
crs(d) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
plot(d, col= topo.colors(9),main="Main precipitation regions",legend.args=list(text='regions', side=4,font=1, line=2.5, cex=1.1),
     xlab = "Longitude (°)", ylab = "Latitude (°)",
     xlim = c(60, 80), ylim = c(35, 45))


region_polygons<-rasterToPolygons(d,dissolve=T)
plot(region_polygons)
}

## Subset a single region for further analysis
region = subset(region_polygons, Region.ID== "5")
plot(region)

rm(d) 


## Crop precipitation data for the selected region, calculate mean monthly precipitation of the selected region

{reg_precip <- CA_GPCC%>%
  tidyr::spread(Time, `Total Precipitation`)%>%
  rasterFromXYZ(.) %>% 
  mask(region) %>% 
  as("SpatialPixelsDataFrame")%>%
  as_data_frame()%>% 
  subset(., select=-c(x,y))%>%
  colMeans(.)%>%
  as_data_frame() 

reg_precip$Date<-CA_GPCC%>%
  tidyr::spread(Time, `Total Precipitation`)%>%
  colnames(.)%>%
  as_data_frame(.)%>%
  .[-c(1,2),]

colnames(reg_precip)<-c("mprcp","Date")

reg_precip<-data.table::as.data.table(reg_precip)
reg_precip$Date<-as.Date(reg_precip$Date,"%Y-%m-%d")
}

## construct combinations for cold season precipitation sums

{ 
  OCT_APR<-reg_precip%>%
    mutate(
      stat_year = ifelse(month(Date) %in% c(10:12), year(Date) + 1, year(Date))
    ) %>%
    filter(month(Date) %in% c(1:4, 10:12))%>% 
    group_by(stat_year)%>%
    summarise(
      Oct_Apr = sum(mprcp)
    )
  OCT_MAY<-reg_precip%>%
    mutate(
      stat_year = ifelse(month(Date) %in% c(10:12), year(Date) + 1, year(Date))
    ) %>%
    filter(month(Date) %in% c(1:5, 10:12))%>% 
    group_by(stat_year)%>%
    summarise(
      Oct_May = sum(mprcp)
    )
  
  NOV_MAY<-reg_precip%>%
    mutate(
      stat_year = ifelse(month(Date) %in% c(11:12), year(Date) + 1, year(Date))
    ) %>%
    filter(month(Date) %in% c(1:5, 11:12))%>% 
    group_by(stat_year)%>%
    summarise(
      Nov_May = sum(mprcp)
    )
  
NOV_APR<-reg_precip%>%
  mutate(stat_year = ifelse(month(Date) %in% c(11:12), year(Date) + 1, year(Date))) %>%
    filter(month(Date) %in% c(1:4, 11:12))%>% 
    group_by(stat_year)%>%
    summarise(Nov_Apr = sum(mprcp))
      
  JAN_MAY<-reg_precip%>%
    mutate(stat_year = ifelse(month(Date) %in% c(11:12), year(Date) + 1, year(Date))) %>%
        filter(month(Date) %in% c(1:5))%>% 
        group_by(stat_year)%>%
        summarise(Jan_May = sum(mprcp))

  seasP<-Reduce(function(...) merge(..., by='stat_year', all.x=TRUE), list(NOV_APR,NOV_MAY,OCT_APR,OCT_MAY,JAN_MAY))
  colnames(seasP)[1]<-"year"
}

seasD<-sdis[,c("year","CHINOR","TARTKI","DUPULI","KOMSOMOLABAD","DOMBRACHI","ARYS","HODJIKENT","GARM")]

# Check for best correlation between seasonal precipitation and discharge combinations
library(corrplot)
sPD<-merge(seasP,sdis, by="year",all=T)
{
corrida<-cor(sPD[,-1],use="pairwise.complete.obs", method = "pearson")
res1 <- cor.mtest(corrida, conf.level = .9)
res2 <- cor.mtest(corrida, conf.level = .99)
corrplot::corrplot(corrida, order = "original",
                   p.mat = res1$p, sig.level = .05,insig = "blank",
                   tl.cex=0.5)

}

###########################
{
mprec <-  ncvar_get(nc_data, "timeseries")
length(mprec)
a<-as.data.frame(mprec)
a<-t(a)
colnames(a)<-colnames(x)

m<-as.data.frame(t(a))
m$Date<-anytime::anydate(rownames(m))
str(m)
}

# constructing seasonale precipitation sums for JFMAM
library(lubridate)
{
 
  prcp<-m%>% filter(month(Date) %in% c(1:5))            
  spre= aggregate(prcp[,c(1:9)],
                  by = list(year(m$Date)),
                  FUN = sum)
  colnames(spre)[1]<-"year"
  }


################################

indices<-read.csv("C:/Users/User/Downloads/indices.csv")


# combine predictor and predictant variables in one wide dataframe
{
  indices<-data.table::as.data.table(indices)
  cols<-colnames(indices)
  DATA_Jan<-indices[, data.table::shift(.SD, 1, NA, "lag", TRUE), .SDcols=cols]
  DATA_Jan<-DATA_Jan[,-c(1,2)]
  DATA_Jan <- DATA_Jan %>%dplyr::select(contains(c("10","11","12"))) # select only Oct,Nov and Dec for lagged 
  DATA_Jan$year <-as.integer(indices$year)
  DATA_Jan<-DATA_Jan %>% tidyr::drop_na(year)
  
  DATA_Jan<-merge(DATA_Jan,spre, by="year", all=TRUE)
  }
{ 
  library(foreach)
  library(doParallel)
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) 
  registerDoParallel(cl)
  
  
  library(data.table)
  DATA<-setDT(indices)
  class(DATA)
  
  {nbest = 50      # nmber of best model for each number of predictors
    nvmax = 3    # limit on number of variables
    subsets= 1:100
    selection_method<-"exhaustive"
    column_names<-c("var 1","var 2","var 3","adjusted.R2","LOOCV.R2")
  }
}

#######   JAN-FEB - Determining the models/predictors #########################



library(caret)
{
  train.control <- trainControl(method = "LOOCV")
  cols<-colnames(indices)

  {
    
    
    
    library(leaps)
    regsubsets.out <- regsubsets(V5~ . -year,
                                 data = DATA_Jan,
                                 nbest = nbest,       # nmber of best model for each number of predictors
                                 nvmax = nvmax,    # limit on number of variables
                                 force.in = NULL, force.out = NULL,
                                 method = selection_method,
                                 really.big=T)
    
    Jan_predictors<-coef(regsubsets.out,(subsets))
    
  }
  statList<-foreach::foreach(i=1:length(Jan_predictors),.packages=c('caret') )%dopar%{
    a<-names(Jan_predictors[[i]])[-1]
    f <- paste("V5", "~",paste(names(Jan_predictors[[i]])[-1], collapse=" + "))
    
    Jan<-do.call("lm", list(as.formula(f), data=as.name("DATA_Jan")))
    
    LOOCV_Jan<-train(formula(Jan), data =DATA_Jan, method = "lm", 
                     trControl = train.control,na.action=na.exclude)
    s<-summary(Jan)
    s
    print.noquote(c(a,round(s[["adj.r.squared"]],digits=2),round(LOOCV_Jan[["results"]][["Rsquared"]],digits=2)))
  }
  
  Jan_list<- do.call(rbind.data.frame,statList)
  colnames(Jan_list)<-column_names
  Jan_list$month<- "January"
}

#######   FEB - MAR  Determining the models/predictors #########################
library(tidyverse)
{    
  cols<-colnames(indices)
  cols<-gsub('[[:digit:]]+', '', cols)
  
  
  anscols <- dput(paste(cols,c(1:1), sep=""))
  anscols<-unique(anscols)
  remove<-c("year1","cspmNA1","X1")
  anscols<-anscols[! anscols %in% remove]
 
  add_Feb<-data.table(indices[,c(..anscols,"year")])
  DATA_Feb<-merge(DATA_Jan,add_Feb, by="year", all=TRUE)
  

  
  
  
  
  regsubsets.out <- regsubsets(V5 ~ . -year,
                               data = DATA_Feb,
                               nbest = nbest,       # nmber of best model for each number of predictors
                               nvmax = nvmax,    # limit on number of variables
                               force.in = NULL, force.out = NULL,
                               method = selection_method,
                               really.big=T)
  
  Feb_predictors<-coef(regsubsets.out,(subsets))
  
  
  statList<-foreach::foreach(i=1:length(Feb_predictors),.packages=c('caret'))%dopar%{
    a<-names(Feb_predictors[[i]])[-1]
    f <- paste("V5", "~",paste(names(Feb_predictors[[i]])[-1], collapse=" + "))
    
    Feb<-do.call("lm", list(as.formula(f), data=as.name("DATA_Feb")))
    
    
    LOOCV_Feb<-train(formula(Feb), data =DATA_Feb, method = "lm", 
                     trControl = train.control,na.action=na.exclude)
    s<-summary(Feb)
    
    print.noquote(c(a,round(s[["adj.r.squared"]],digits=2),round(LOOCV_Feb[["results"]][["Rsquared"]],digits=2)))
  }
  
  Feb_list<- do.call(rbind.data.frame,statList)
  colnames(Feb_list)<-column_names
  Feb_list$month<- "February"
}  

#######   MAR - MAY  Determining the models/predictors #########################

{    
  cols<-colnames(indices)
  cols<-gsub('[[:digit:]]+', '', cols)
  
  
  anscols <- dput(paste(cols,c(2:2), sep=""))
  anscols<-unique(anscols)
  remove<-c("year2","X2","cspmNA2")
  anscols<-anscols[! anscols %in% remove]
  
  
  add_Mar<-data.table(indices[,c(..anscols,"year")])
  DATA_Mar<-merge(DATA_Feb,add_Mar, by="year", all=TRUE)
  

  
  

  regsubsets.out <- regsubsets(V5 ~ . -year,
                               data = DATA_Mar,
                               nbest = nbest,       # nmber of best model for each number of predictors
                               nvmax = nvmax,    # limit on number of variables
                               force.in = NULL, force.out = NULL,
                               method = selection_method,
                               really.big=T)
  
  Mar_predictors<-coef(regsubsets.out,(subsets))
  
  
  statList<-foreach::foreach(i=1:length(Mar_predictors),.packages=c('caret'))%dopar%{
    a<-names(Mar_predictors[[i]])[-1]
    f <- paste("V5", "~",paste(names(Mar_predictors[[i]])[-1], collapse=" + "))
    
    Mar<-do.call("lm", list(as.formula(f), data=as.name("DATA_Mar")))
    
    
    LOOCV_Mar<-train(formula(Mar), data =DATA_Mar, method = "lm", 
                     trControl = train.control,na.action=na.exclude)
    s<-summary(Mar)
    
    print.noquote(c(a,round(s[["adj.r.squared"]],digits=2),round(LOOCV_Mar[["results"]][["Rsquared"]],digits=2)))
  }
  
  Mar_list<- do.call(rbind.data.frame,statList)
  colnames(Mar_list)<-column_names
  Mar_list$month<- "March"
}  

###### Construct regression models

# Forecast models for cluster 5 (Amudarya catchment)

m1<-lm(V5~ninm12_lag_1+shm12_lag_1+npim11_lag_1, DATA_Jan)

summary(m1)

plot(m1)


m2<-lm(V5~oni1+naom1+shm1, DATA_Feb)
summary(m2)

m3<-lm(V5~ninm2+naom2+shm1+npi2, DATA_Mar)
summary(m3)

# Forecast models for cluster 15 (Talas)

g1<-lm(J_M~npi1+nif2, DATA_Mar)
summary(g1)


g2<-lm(J_M~npi1, DATA_Feb)
summary(g2)

# Forecast models for cluster 11 (Tashkent)

g1<-lm(J_A~eawm12_lag_1+oni12_lag_1+npim12_lag_1, DATA_Jan)
summary(g1)


g2<-lm(J_A~eawm1+oni1+npim1, DATA_Feb)
summary(g2)

g3<-lm(J_A~eawm1+oni2+npim1, DATA_Mar)
summary(g3)


# Forecast models for cluster 7 (Samarkand)

g1<-lm(J_A~eawm12_lag_1+nifm12_lag_1+pna12_lag_1, DATA_Jan)
summary(g1)


g2<-lm(FMA~eawm1+nifm1+pna1, DATA_Feb)
summary(g2)

g3<-lm(prcp$pre3+prcp$pre4~eawm1+nifm1++pna2, DATA_Mar)
summary(g3)

# Forecast models for cluster 1 (Termez)

m1<-lm(J_M~oni12_lag_1+eawm12_lag_1+pna12_lag_1, DATA_Jan)
summary(m1)


m2<-lm(J_M~eawm12_lag_1+nifm1+pna1, DATA_Feb)
summary(m2)

m3<-lm(M_M~eawm12_lag_1+pna1+nifm2, DATA_Mar)
summary(m3)

###########################################################################

