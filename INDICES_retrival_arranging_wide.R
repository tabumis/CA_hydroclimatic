{
  library(readr)
  library(dplyr)
  library(naniar)
  library(ggplot2)
  library(lubridate)

#NINO 3.4 Index
{
  df <- read_delim("https://psl.noaa.gov/data/correlation/nina34.data", " ",skip=1)
n<-dim(df)[1]
df<-df[1:(n-3),]
colnames(df)<-c("year","1", "2",  "3" , "4",  "5" , "6" , "7" , "8" , "9",  "10", "11", "12")
df <- replace_with_na_all(data = df,
                          condition = ~.x == -99.99)
nino<-df%>%
  rename_at(vars(-year), funs(paste0("nin", .)))

DF<-reshape2::melt(df, id.vars="year",
                   variable.name="monate",
                   value.name="value")

nino[] <- lapply(nino, function(x) as.numeric(as.character(x)))
a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
a$Date<- anytime::anydate(a$Date)
a<-dplyr::arrange(a, Date)
a<-na.omit(a)
a$value<-as.numeric(a$value)
a$scaled<-scale(a$value)

a$ma<-pracma::movavg(a$scaled, 3,type="s")

ggplot2::ggplot(a[c(20:400),])+
  geom_line(aes(y=ma, x=Date),col="red")+
  geom_line(aes(y=scaled, x=Date))
b<-a[,c("Date","scaled")]
c<-a[,c("Date","ma")]

nino<-reshape2::dcast(b, year(Date) ~ month(Date),value.var='scaled')
ninm<-reshape2::dcast(c, year(Date) ~ month(Date),value.var='ma')
colnames(nino)[1]<-"year"
colnames(ninm)[1]<-"year"
nino<-nino%>%
  rename_at(vars(-year), funs(paste0("nino", .)))
ninm<-ninm%>%
  rename_at(vars(-year), funs(paste0("ninm", .)))
rm(a,b,c,DF,df)

}

#ONI Index
  {
    df <- read_delim("https://psl.noaa.gov/data/correlation/oni.data", " ",skip=2)
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    n<-dim(df)[1]
    df<-df[1:(n),]
    colnames(df)<-c("year","1", "2",  "3" , "4",  "5" , "6" , "7" , "8" , "9",  "10", "11", "12")
    
    df <- replace_with_na_all(data = df,
                              condition = ~.x == -99.9)
    
    df<-head(df,-8)
    oni<-df%>%
      rename_at(vars(-year), funs(paste0("oni", .)))
    oni[] <- lapply(oni, function(x) as.numeric(as.character(x)))
    
    rm(df)
  }
# Nino4 (nif) and its 3-month moving average (nifm)
  {
    df <- read_delim("https://psl.noaa.gov/data/correlation/nina4.data", " ",skip=1)
    n<-dim(df)[1]
    df<-df[1:(n-3),]
    colnames(df)<-c("year","1", "2",  "3" , "4",  "5" , "6" , "7" , "8" , "9",  "10", "11", "12")
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    df <- replace_with_na_all(data = df,
                              condition = ~.x == -99.99)
    nif<-df%>%
      rename_at(vars(-year), funs(paste0("nif", .)))
    
    
    DF<-reshape2::melt(df, id.vars="year",
                       variable.name="monate",
                       value.name="value")
    
    
    a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
    a$Date<- anytime::anydate(a$Date)
    a<-dplyr::arrange(a, Date)
    a<-na.omit(a)
    a$value<-as.numeric(a$value)
    a$scaled<-scale(a$value)
    
    a$ma<-pracma::movavg(a$scaled, 3,type="s")
    
    ggplot2::ggplot(a[c(20:400),])+
      geom_line(aes(y=ma, x=Date),col="red")+
      geom_line(aes(y=scaled, x=Date))
    b<-a[,c("Date","scaled")]
    c<-a[,c("Date","ma")]
    
    nif<-reshape2::dcast(b, year(Date) ~ month(Date),value.var='scaled')
    nifm<-reshape2::dcast(c, year(Date) ~ month(Date),value.var='ma')
    colnames(nif)[1]<-"year"
    colnames(nifm)[1]<-"year"
    nif<-nif%>%
      rename_at(vars(-year), funs(paste0("nif", .)))
    nifm<-nifm%>%
      rename_at(vars(-year), funs(paste0("nifm", .)))
    rm(a,b,c,DF,df)
  }
# ENSO phases (warm, cold, neutral)
  nphase <- read_delim("D:/CA_precip/nino_phases.txt", 
                            "\t", escape_double = FALSE, col_types = cols(nicol1 = col_double()), 
                            trim_ws = TRUE)
#Atlantic Multidecadal Oscillation (AMO) Index
    {
    df <- read_delim("https://psl.noaa.gov/data/correlation/amon.us.data", " ",skip=2)
    
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    n<-dim(df)[1]
    df<-df[1:(n),]
    
    colnames(df)<-c("year","1", "2",  "3" , "4",  "5" , "6" , "7" , "8" , "9",  "10", "11", "12")
    df <- replace_with_na_all(data = df,
                              condition = ~.x == -99.99)
    
    
    
    amo<-df%>%
      rename_at(vars(-year), funs(paste0("amo", .)))
    
    
    amo[] <- lapply(amo, function(x) as.numeric(as.character(x)))
    DF<-reshape2::melt(df, id.vars="year",
                       variable.name="monate",
                       value.name="value")
    a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
    a$Date<- anytime::anydate(a$Date)
    a<-dplyr::arrange(a, Date)
    a<-na.omit(a)
    a$value<-as.numeric(a$value)
    
    
    a$ma<-pracma::movavg(a$value, 3,type="s")
    
    ggplot2::ggplot(a[c(20:400),])+
      geom_line(aes(y=ma, x=Date),col="red")+
      geom_line(aes(y=value, x=Date))
    b<-a[,c("Date","value")]
    c<-a[,c("Date","ma")]
    
    amo<-reshape2::dcast(b, year(Date) ~ month(Date),value.var='value')
    amom<-reshape2::dcast(c, year(Date) ~ month(Date),value.var='ma')
    colnames(amo)[1]<-"year"
    colnames(amom)[1]<-"year"
    amo<-amo%>%
      rename_at(vars(-year), funs(paste0("amo", .)))
    amom<-amom%>%
      rename_at(vars(-year), funs(paste0("amom", .)))
    rm(a,b,c,DF,df)
  }

#NAO Index and its 3-month running average
  
  {
    df <- read_table2("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table", col_names = F)
    
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    n<-dim(df)[1]
    df<-df[1:(n),]
    colnames(df)<-c("year","1", "2",  "3" , "4",  "5" , "6" , "7" , "8" , "9",  "10", "11", "12")
    
    df <- replace_with_na_all(data = df,
                              condition = ~.x == -999.)
    nao<-df%>%
      rename_at(vars(-year), funs(paste0("nao", .)))
    nao[] <- lapply(nao, function(x) as.numeric(as.character(x)))
    DF<-reshape2::melt(df, id.vars="year",
                       variable.name="monate",
                       value.name="value")
   
    a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
    a$Date<- anytime::anydate(a$Date)
    a<-dplyr::arrange(a, Date)
    a<-na.omit(a)
    a$value<-as.numeric(a$value)

    a$ma<-pracma::movavg(a$value, 3,type="s")
    
    ggplot2::ggplot(a[c(20:400),])+
      geom_line(aes(y=ma, x=Date),col="red")+
      geom_line(aes(y=value, x=Date))
    b<-a[,c("Date","value")]
    c<-a[,c("Date","ma")]
    
    nao<-reshape2::dcast(b, year(Date) ~ month(Date),value.var='value')
    naom<-reshape2::dcast(c, year(Date) ~ month(Date),value.var='ma')
    colnames(nao)[1]<-"year"
    colnames(naom)[1]<-"year"
    nao<-nao%>%
      rename_at(vars(-year), funs(paste0("nao", .)))
    naom<-naom%>%
      rename_at(vars(-year), funs(paste0("naom", .)))
    rm(a,b,c,DF,df)
  }


  
#QBO Index (20hPaN)
{
  library(lubridate)
    qbo <- read_table2("https://www.geo.fu-berlin.de/met/ag/strat/produkte/qbo/qbo.dat", 
                     col_types = cols(`10hPaN` = col_skip(), 
                                      `15hPaN` = col_skip(), `70hPaN` = col_skip(), 
                                      `30hPaN` = col_skip(), `40hPaN` = col_skip(), 
                                      `50hPaN` = col_skip(), IIIII = col_skip(), 
                                      YYMM = col_date(format = "%y%m")), skip = 7)
    qbo$YYMM<-as.Date(ifelse(qbo$YYMM > Sys.Date(), format(qbo$YYMM, "19%y-%m-%d"), format(qbo$YYMM)))
    colnames(qbo)<-c("Date","qbo")
    qbo<-qbo[qbo$Date >= "1953-01-01" & qbo$Date <= "2020-12-27",]
    
    qbo$Date<-zoo::as.yearmon(qbo$Date,"%y-%m")
    qbo$qbo<-as.double(qbo$qbo)
    a<-qbo
    a$scaled<-scale(a$qbo)
    a$ma<-pracma::movavg(a$scaled, 3,type="s")
    b<-a[,c("Date","scaled")]
    c<-a[,c("Date","ma")]
    
    qbo<-reshape2::dcast(b, year(Date) ~ month(Date),value.var='scaled')
    qbom<-reshape2::dcast(c, year(Date) ~ month(Date),value.var='ma')
    colnames(qbo)[1]<-"year"
    colnames(qbom)[1]<-"year"
    qbo<-qbo%>%
      rename_at(vars(-year), funs(paste0("qbo", .)))
    qbom<-qbom%>%
      rename_at(vars(-year), funs(paste0("qbom", .)))
    rm(a,b,c,DF,df)
  }  
 


#EAWE Index
{
  eawe <- read_table2("ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/eawr_index.tim", 
                     skip = 7)
  eawe<-reshape2::dcast(eawe, YEAR ~ MONTH ,value.var = "INDEX")
  colnames(eawe)[1]<-"year"

  eawe <- replace_with_na_all(data = eawe,
                            condition = ~.x == -99.90)
 
 
   DF<-reshape2::melt(eawe, id.vars="year",
                     variable.name="monate",
                     value.name="value")
  
  a<-DF%>%tidyr::unite("year",year:monate, sep="-")
  a$year<- anytime::anydate(a$year)
  a<-dplyr::arrange(a, year)
  a<-na.omit(a)
  a$value<-as.numeric(a$value)

  
  a$ma<-pracma::movavg(a$value, 3,type="s")
  
  ggplot2::ggplot(a[c(20:400),])+
    geom_line(aes(y=ma, x=Date),col="red")+
    geom_line(aes(y=value, x=Date))
  b<-a[,c("year","value")]
  c<-a[,c("year","ma")]
  
  eawr<-reshape2::dcast(b, year(year) ~ month(year),value.var='value')
  eawrm<-reshape2::dcast(c, year(year) ~ month(year),value.var='ma')
  colnames(eawr)[1]<-"year"
  colnames(eawrm)[1]<-"year"
  eawr<-eawr%>%
    rename_at(vars(-year), funs(paste0("eawr", .)))
  eawrm<-eawrm%>%
    rename_at(vars(-year), funs(paste0("eawrm", .)))
  rm(a,b,c,DF,df, eawe)
  
}


  
#SCAND Index
{
  scand <- read_table2("ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/scand_index.tim", 
                      skip = 7)
  scand<-reshape2::dcast(scand, YEAR ~ MONTH ,value.var = "INDEX")
  colnames(scand)[1]<-"year"
  scand <- replace_with_na_all(data = scand,
                              condition = ~.x == -99.90)
  DF<-reshape2::melt(scand, id.vars="year",
                     variable.name="monate",
                     value.name="value")
  
  a<-DF%>%tidyr::unite("year",year:monate, sep="-")
  a$year<- anytime::anydate(a$year)
  a<-dplyr::arrange(a, year)
  a<-na.omit(a)
  a$value<-as.numeric(a$value)

  a$ma<-pracma::movavg(a$value, 3,type="s")
  
  ggplot2::ggplot(a[c(20:400),])+
    geom_line(aes(y=ma, x=Date),col="red")+
    geom_line(aes(y=value, x=Date))
  b<-a[,c("year","value")]
  c<-a[,c("year","ma")]
  
  scan<-reshape2::dcast(b, year(year) ~ month(year),value.var='value')
  scanm<-reshape2::dcast(c, year(year) ~ month(year),value.var='ma')
  colnames(scan)[1]<-"year"
  colnames(scanm)[1]<-"year"
  scan<-scan%>%
    rename_at(vars(-year), funs(paste0("scan", .)))
  scanm<-scanm%>%
    rename_at(vars(-year), funs(paste0("scanm", .)))
  rm(a,b,c,DF,scand)
}
  


# Southern Oscillation Index (SOI)

{
  df <- read_delim("https://crudata.uea.ac.uk/cru/data/soi/soi.dat", " ",skip=12)
  n<-dim(df)[1]
  df<-df[1:(n),]
  colnames(df)<-c("year","1", "2",  "3" , "4",  "5" , "6" , "7" , "8" , "9",  "10", "11", "12")
  df<-df[1:(length(df)-1)]
   df <- replace_with_na_all(data = df,
                            condition = ~.x == -99.99)

  df[] <- lapply(df, function(x) as.numeric(as.character(x)))
  df<-df[-c(1:71),]
  
  DF<-reshape2::melt(df, id.vars="year",
                     variable.name="monate",
                     value.name="value")
  
  a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
  a$Date<- anytime::anydate(a$Date)
  a<-dplyr::arrange(a, Date)
  a<-na.omit(a)
  a$value<-as.numeric(a$value)
  a$ma<-pracma::movavg(a$value, 3,type="s")
  
  ggplot2::ggplot(a[c(20:400),])+
    geom_line(aes(y=ma, x=Date),col="red")+
    geom_line(aes(y=value, x=Date))
  b<-a[,c("Date","value")]
  c<-a[,c("Date","ma")]
  
  soi<-reshape2::dcast(b, year(Date) ~ month(Date),value.var='value')
  soim<-reshape2::dcast(c, year(Date) ~ month(Date),value.var='ma')
  colnames(soi)[1]<-"year"
  colnames(soim)[1]<-"year"
  soi<-soi%>%
    rename_at(vars(-year), funs(paste0("soi", .)))
  soim<-soim%>%
    rename_at(vars(-year), funs(paste0("soim", .)))
  rm(a,b,c,DF,df)
  
}
 
# Dipole Mode Index (DMI)

{
  df <- read_delim("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/dmi.had.long.data", " ",skip=1)
  df[] <- lapply(df, function(x) as.numeric(as.character(x)))
  n<-dim(df)[1]
  df<-df[1:(n),]
  colnames(df)<-c("year","1", "2",  "3" , "4",  "5" , "6" , "7" , "8" , "9",  "10", "11", "12")

  df <- replace_with_na_all(data = df,
                            condition = ~.x == -9999.000)
  df<-head(df,-7)
  df<-df[-c(1:79),]
  df[] <- lapply(df, function(x) as.numeric(as.character(x)))
  
  DF<-reshape2::melt(df, id.vars="year",
                     variable.name="monate",
                     value.name="value")
  
  a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
  a$Date<- anytime::anydate(a$Date)
  a<-dplyr::arrange(a, Date)
  a<-na.omit(a)
  a$value<-as.numeric(a$value)
  a$ma<-pracma::movavg(a$value, 3,type="s")
  
  ggplot2::ggplot(a[c(20:400),])+
    geom_line(aes(y=ma, x=Date),col="red")+
    geom_line(aes(y=value, x=Date))
  b<-a[,c("Date","value")]
  c<-a[,c("Date","ma")]
  
  dmi<-reshape2::dcast(b, year(Date) ~ month(Date),value.var='value')
  dmim<-reshape2::dcast(c, year(Date) ~ month(Date),value.var='ma')
  colnames(dmi)[1]<-"year"
  colnames(dmim)[1]<-"year"
  dmi<-dmi%>%
    rename_at(vars(-year), funs(paste0("dmi", .)))
  dmim<-dmim%>%
    rename_at(vars(-year), funs(paste0("dmim", .)))
  rm(a,b,c,DF,df)
}

  
 
    # Arctic Ocsillation

  {
    df <- read_delim("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii.table", " ",skip=0)
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    n<-dim(df)[1]
    df<-df[1:(n),]
    colnames(df)<-c("year","1", "2",  "3" , "4",  "5" , "6" , "7" , "8" , "9",  "10", "11", "12")
    
    df <- replace_with_na_all(data = df,
                              condition = ~.x == -9999.000)
 
    
    DF<-reshape2::melt(df, id.vars="year",
                       variable.name="monate",
                       value.name="value")
    

    a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
    a$Date<- anytime::anydate(a$Date)
    a<-dplyr::arrange(a, Date)
    a<-na.omit(a)
    a$value<-as.numeric(a$value)
    a$ma<-pracma::movavg(a$value, 3,type="s")
    
    ggplot2::ggplot(a[c(20:400),])+
      geom_line(aes(y=ma, x=Date),col="red")+
      geom_line(aes(y=value, x=Date))
    b<-a[,c("Date","value")]
    c<-a[,c("Date","ma")]
    
    ao<-reshape2::dcast(b, year(Date) ~ month(Date),value.var='value')
    aom<-reshape2::dcast(c, year(Date) ~ month(Date),value.var='ma')
    colnames(ao)[1]<-"year"
    colnames(aom)[1]<-"year"
    ao<-ao%>%
      rename_at(vars(-year), funs(paste0("ao", .)))
    aom<-aom%>%
      rename_at(vars(-year), funs(paste0("aom", .)))
    rm(a,b,c,DF,df)
    
  }


# PDO

  {
    df <- read_delim("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/pdo.long.data", " ",skip=1)
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    n<-dim(df)[1]
    df<-df[1:(n),]
    colnames(df)<-c("year","1", "2",  "3" , "4",  "5" , "6" , "7" , "8" , "9",  "10", "11", "12")
    df<-head(df,-10)
    df <- replace_with_na_all(data = df,
                              condition = ~.x == -9.900)
 
    df<-df[-c(1:49),]
    
    DF<-reshape2::melt(df, id.vars="year",
                       variable.name="monate",
                       value.name="value")
    
    
    DF<-reshape2::melt(df, id.vars="year",
                       variable.name="monate",
                       value.name="value")
    
    
    DF<-reshape2::melt(df, id.vars="year",
                       variable.name="monate",
                       value.name="value")
    
    a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
    a$Date<- anytime::anydate(a$Date)
    a<-dplyr::arrange(a, Date)
    a<-na.omit(a)
    a$value<-as.numeric(a$value)
    a$ma<-pracma::movavg(a$value, 3,type="s")
    
    ggplot2::ggplot(a[c(20:400),])+
      geom_line(aes(y=ma, x=Date),col="red")+
      geom_line(aes(y=value, x=Date))
    b<-a[,c("Date","value")]
    c<-a[,c("Date","ma")]
    pdo<-reshape2::dcast(b, year(Date) ~ month(Date),value.var='value')
    pdom<-reshape2::dcast(c, year(Date) ~ month(Date),value.var='ma')
    colnames(pdo)[1]<-"year"
    colnames(pdom)[1]<-"year"
    pdo<-pdo%>%
      rename_at(vars(-year), funs(paste0("pdo", .)))
    pdom<-pdom%>%
      rename_at(vars(-year), funs(paste0("pdom", .)))
    rm(a,b,c,DF,df)
  }
# North Pacific (NP) Index

  
  {
    df <- read_delim("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/np.long.data", " ",skip=1)
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    n<-dim(df)[1]
    df<-df[1:(n),]
    colnames(df)<-c("year","1", "2",  "3" , "4",  "5" , "6" , "7" , "8" , "9",  "10", "11", "12")
    df<-head(df,-6)
    df <- replace_with_na_all(data = df,
                              condition = ~.x == -999.000)
    
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    df<-df[-c(1:50),]
    
    DF<-reshape2::melt(df, id.vars="year",
                       variable.name="monate",
                       value.name="value")
    
    a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
    a$Date<- anytime::anydate(a$Date)
    a<-dplyr::arrange(a, Date)
    a<-na.omit(a)
    a$value<-as.numeric(a$value)
    a$scaled<-scale(a$value)
    
    a$ma<-pracma::movavg(a$scaled, 3,type="s")
    
    ggplot2::ggplot(a[c(20:400),])+
      geom_line(aes(y=ma, x=Date),col="red")+
      geom_line(aes(y=scaled, x=Date))
    b<-a[,c("Date","scaled")]
    c<-a[,c("Date","ma")]
    
    npi<-reshape2::dcast(b, year(Date) ~ month(Date),value.var='scaled')
    npim<-reshape2::dcast(c, year(Date) ~ month(Date),value.var='ma')
    colnames(npi)[1]<-"year"
    colnames(npim)[1]<-"year"
    npi<-npi%>%
      rename_at(vars(-year), funs(paste0("npi", .)))
    npim<-npim%>%
      rename_at(vars(-year), funs(paste0("npim", .)))
    rm(a,b,c,DF,df)
  }

# Caspian sea pattern
  
  
  {
    df <- read_delim("https://crudata.uea.ac.uk/cru/data/ncp/ncp.dat"," ",skip=0)
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    colnames(df)<-c("year","month","csp")
    df<-reshape2::dcast(df, year ~ month ,value.var = "csp")
    df<-df[,-14]
    csp<-df%>%
      rename_at(vars(-year), funs(paste0("csp", .)))
    
    
    DF<-reshape2::melt(df, id.vars="year",
                       variable.name="monate",
                       value.name="value")
    
    
    a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
    a$Date<- anytime::anydate(a$Date)
    a<-dplyr::arrange(a, Date)
    
    
    a$ma<-pracma::movavg(a$value, 3,type="s")
    ggplot2::ggplot(a[c(20:400),])+
      geom_line(aes(y=ma, x=Date),col="red")+
      geom_line(aes(y=value, x=Date))
    a<-a[,-2]
    cspm<-reshape2::dcast(a, year(Date) ~ month(Date),value.var='ma')
    colnames(cspm)[1]<-"year"
    
    cspm<-cspm%>%
      rename_at(vars(-year), funs(paste0("cspm", .)))
    rm(a,DF,df)
  }
 
  #Mediterranean Oscillation Indices
  {
    df <- read_delim("https://crudata.uea.ac.uk/cru/data/moi/moig.dat"," ",skip=0)
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    colnames(df)<-c("year","month","moi")
    df<-reshape2::dcast(df, year ~ month ,value.var = "moi")
    df<-df[,-14]
    moi<-df%>%
      rename_at(vars(-year), funs(paste0("moi", .)))
    
    
    
    DF<-reshape2::melt(df, id.vars="year",
                       variable.name="monate",
                       value.name="value")
    
    
    a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
    a$Date<- anytime::anydate(a$Date)
    a<-dplyr::arrange(a, Date)
    
    
    a$ma<-pracma::movavg(a$value, 3,type="s")
    a<-a[,-2]
    moim<-reshape2::dcast(a, year(Date) ~ month(Date),value.var='ma')
    colnames(moim)[1]<-"year"
    
    moim<-moim%>%
      rename_at(vars(-year), funs(paste0("moim", .)))
    rm(a,DF,df)
  }


# Siberian High (sh) and its 3-month moving average (shm)

{
  library(readr)
  siberian_h <- read_csv("D:/CA_precip/SH_anom.csv",
                            col_types = cols(Time = col_date(format = "%Y-%m")))
  
 
df<-siberian_h
colnames(df)<-c("T","mean")
df$T<-zoo::as.yearmon(df$T,"%Y-%m")


df<-reshape2::dcast(df, year(T) ~ month(T),value.var='mean')
colnames(df)[1] <- "year"

sh<-df%>%
  rename_at(vars(-year), funs(paste0("sh", .)))




DF<-reshape2::melt(df, id.vars="year",
                   variable.name="monate",
                   value.name="value")


a<-DF%>%tidyr::unite("Date",year:monate, sep="-")
a$Date<- anytime::anydate(a$Date)
a<-dplyr::arrange(a, Date)


a$ma<-pracma::movavg(a$value, 3,type="s")

a<-a[,-2]
shm<-reshape2::dcast(a, year(Date) ~ month(Date),value.var='ma')
colnames(shm)[1]<-"year"

shm<-shm%>%
  rename_at(vars(-year), funs(paste0("shm", .)))
rm(a,DF,df, siberian_h)

}
}
############################################
############################################
############################################

indices<-Reduce(function(...) merge(..., by='year', all.x=TRUE), list(amo, amom, ao, aom, csp,cspm, dmi,dmim,eawr,eawrm, moi,moim,nao,naom,nif,nifm,ninm, nino,npi,npim,oni,pdo,pdom, qbo,qbom,scan,scanm, sh, shm, soi, soim))

rm(amo, amom, ao, aom, csp,cspm, dmi,dmim,eawr,eawrm, moi,moim,nao,naom,nif,nifm,ninm, nino,npi,npim,oni,pdo,pdom, qbo,qbom,scan,scanm, sh, shm, soi, soim)

write.csv(indices,"D:/CA_precip/indices.csv")     

