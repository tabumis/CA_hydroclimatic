{
  setwd("D:/CA_precip/2021-01-23_18-45")
  library(foreach)
  library(doParallel)
  library(naniar)
  library(purrr)
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  
  file_list<-list.files(pattern = "Month")
  
  
  statList<-foreach::foreach(i=1:length(file_list),.packages=c('readr','stringr'))%dopar%{
    
    temp <- read_table2(file_list[i], col_types = cols(`Original;` = col_number(), 
                                                       Flag = col_skip(), `Calculated;` = col_number(), 
                                                       `YYYY-MM-DD;hh:mm;` = col_datetime(format = "%F %*")), 
                        comment = "#")
    
    
    if (sum(temp$`Calculated;`) < sum(temp$`Original;`)){
      temp <- read_table2(file_list[i], col_types = cols(`Original;` = col_number(), 
                                                         Flag = col_skip(), `Calculated;`=col_skip(), 
                                                         `YYYY-MM-DD;hh:mm;` = col_datetime(format = "%F %*")), 
                          comment = "#")
    } else {
      temp <- read_table2(file_list[i], col_types = cols(`Original;` = col_skip(), 
                                                         Flag = col_skip(), `Calculated;` = col_number(), 
                                                         `YYYY-MM-DD;hh:mm;` = col_datetime(format = "%F %*")), 
                          comment = "#")
    }
    
    colnames(temp)[1]<-"Date"
    rname<-str_replace(file_list[i], "_Q_Month.txt", "")
    colnames(temp)[2]<- rname
    temp<-assign(rname, temp,envir=.GlobalEnv)
  }
  
  require(tidyverse);
  CA_GRDC_data<-reduce(statList, full_join, by = "Date", all=T)
  CA_GRDC_data<-arrange(CA_GRDC_data, Date)
  CA_GRDC_data<-replace_with_na_all(data = CA_GRDC_data,
                                    condition = ~.x == -999.)
  rm(statList)
}


# retrieving GRDC station data
{
  library(readxl)
  GRDC_Stations <- read_excel("D:/CA_precip/GRDC_Stations.xlsx")
  library(data.table)
  

library(data.table)
setnames( CA_GRDC_data,as.character(GRDC_Stations$grdc_no), GRDC_Stations$station,skip_absent=TRUE)
names(CA_GRDC_data)
b<-dplyr:: select(CA_GRDC_data, -c(`DAZGON`,`PAULGAN`,`USTIE`))
b<-CA_GRDC_data[c(475:990),]

anyNA(b)}



# constructing seadonal discharge timeseries
{
  c<-b%>% filter(month(Date) %in% c(4:9))             
 sdis= aggregate(c,
              by = list(year(c$Date)),
              FUN = mean)
sdis$year<-year(sdis$Date)
sdis<-sdis[,-c(1,2)]
cond1 <- sapply(sdis, function(col) sum(!is.na(col)) < 30) # condistion that at least 20 years of observations are available

mask <- !(cond1)
sdis<-sdis[,mask,drop=F]


}
# correlatiion matrix
{
library(corrplot)

s<-sdis[ , !(names(sdis) %in% "year")]

cordis<-cor(s,use="pairwise.complete.obs", method = "pearson")
res1 <- cor.mtest(s, conf.level = .95)
res2 <- cor.mtest(s, conf.level = .99)

col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))

P<-corrplot(cordis, order="hclust",hclust.method="ward",addrect = 6, 
            p.mat = res1$p, sig.level = .05,insig = "blank",
            tl.cex=0.5, col = col2(100))

}

# retriving regionalized precipitation timeseries

mprec <- read.csv('C:/Users/Umirbekov/Downloads/8clusters_ts.csv')

mprec$Date<-as.Date(mprec$Date, format = "%Y-%m-%d")

length(mprec)
cluster<-2

{
  prcp<-mprec[,c(cluster,10)]
  
  library(lubridate)
  library(dplyr)
  colnames(prcp)[1]<-"pre"
  prcp<-reshape2::dcast(prcp, year(Date) ~ month(Date) ,value.var = "pre")
  
  boxplot(prcp[,c(2:13)], col="lightblue",
          names=c(1:12),main= c("Cluster",c))
  
  colnames(prcp)[1] <- "year"
  prcp<-prcp%>%
    rename_at(vars(-year), funs(paste0("pre", .)))
  
}
# constructing 3-mth precipitation sums over peak months 
{
prcp<-mprec[,c(cluster,10)]
c<-prcp%>% filter(month(Date) %in% c(2:6))             
spre= aggregate(c["V7"],
                by = list(year(c$Date)),
                FUN = sum)
colnames(spre)[1]<-"year"

rm(c,b)
}
# merging seasonal discharge and precipitation timeseries

df<-merge(sdis,spre,by= "year", all=F)

# try regression now

reg2<-lm(TARTKI~V7, df)
summary(reg2)

reg3<-lm(TAVILDARA~V1, df)
summary(reg3)
reg4<-lm(KERKI~V1, df)
summary(reg4)
reg5<-lm(KOMSOMOLABAD~V1, df)
summary(reg5)
reg6<-lm(DOMBRACHI~V1, df)
summary(reg6)


ggplotRegression(reg2)
ggplotRegression(reg3)

# Plot fitted

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

