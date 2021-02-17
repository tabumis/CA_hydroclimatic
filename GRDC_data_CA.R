{
  setwd("C:/Users/User/Downloads/2021-01-23_18-45")
  library(foreach)
  library(doParallel)
  library(naniar)
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  
  file_list<-list.files(pattern = "Month")
  
  
  statList<-foreach::foreach(i=1:length(file_list),.packages=c('readr','stringr','naniar'))%dopar%{
    
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

# substracting data across the stations that is fully available for 1960-1980 period
{
  a<-CA_GRDC_data[-c(1:654,836:1017),] 
  
library(dplyr)
  b<-a%>%
    select_if(~ !any(is.na(.)))
 
# rename the resulted dataframe columns correspondigly to the station name
{library(readxl)
GRDC_Stations <- read_excel("C:/Users/User/Downloads/grdc_stations (1)/GRDC_Stations.xlsx")
library(data.table)
setnames( b,as.character(GRDC_Stations$grdc_no), GRDC_Stations$station,skip_absent=TRUE)
names(b)
b<-select(b, -c(`TOKTOGUL RESERVOIR`,`ARYS`,`CHIRAKCHI`,`VARGANZA`,`BEKABAD`,`KAL`,`KAZALINSK`,`TYUMEN-ARYK`,
                `TULEKEN`,`HODJIKENT`,`KERKI`,`UCH-TEREK`))
}

# compute correlations and plot cormatrix
library(corrplot)

c<-cor(b[,-1],use="complete.obs", method = "pearson")
res1 <- cor.mtest(b[,-1], conf.level = .95)
res2 <- cor.mtest(b[,-1], conf.level = .99)

col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                          "cyan", "#007FFF", "blue", "#00007F"))

P<-corrplot(c, order="AOE",hclust.method="ward",addrect = 6, 
         p.mat = res1$p, sig.level = .05,insig = "blank",
         tl.cex=0.5, col = col2(100))

}

# to clarify: UCH-TEREK (Qoradaryo), TASH-Kurgan (Isfara)


# classify watersheds by accending order

{
clus.order<- as.data.frame(P)
clus.order$station<-rownames(clus.order)
clus.order$cluster<-seq(1:30)
clus.order<-clus.order[,c(31,32)]
}

# save as excel file 

library(writexl)

write_xlsx(clus.order,"D:/CA_precip/CA_precip_GIS/clustered_watersheds.xlsx")
