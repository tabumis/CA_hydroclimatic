{library(readxl)
PRCP <- read_excel("C:/Users/User/Downloads/Precip_v1 (2).xls")
View(PRCP)

meta <- read_excel("C:/Users/User/Downloads/Precip_v1 (2).xls",sheet = "Info_prec")


library(dplyr)

full_stations<-meta%>%
  filter(meta[,c(11:22)]>99)
}
write.csv(full_stations,"D:/CA_precip/stations.csv" )
{
precip<-PRCP%>%
  filter(Name=="Kulyab")
prcp<-precip[,-c(1:6)]
colnames(prcp)<-c("year","1", "2",  "3" , "4",  "5" , "6" , "7" , "8" , "9",  "10", "11", "12")
library(naniar)
prcp <- replace_with_na_all(data = prcp,
                          condition = ~.x == -999.0)
prcp<-prcp%>%
  rename_at(vars(-year), funs(paste0("pre", .)))
}
#plot monthly climatology

prcp<-reshape2::dcast(prcp, year(Date) ~ month(Date) ,value.var = "pre")

boxplot(prcp[,c(2:13)], col="lightblue",
        names=c(1:12),main= c("Cluster",c))

colnames(prcp)[1] <- "year"
prcp<-prcp%>%
  rename_at(vars(-year), funs(paste0("pre", .)))




indices<-read.csv("D:/DATA/OUTPUT/validation/indices.csv")

DATA<-merge(indices,prcp, by="year")
rm(full_stations,meta,PRCP,precip)
{ 
  library(foreach)
  library(doParallel)
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) 
  registerDoParallel(cl)
  
  
  library(data.table)
  DATA<-setDT(DATA)
  class(DATA)
  
  {nbest = 50      # nmber of best model for each number of predictors
  nvmax = 3    # limit on number of variables
  subsets= 1:100
  selection_method<-"forward"
  column_names<-c("var 1","var 2","var 3","adjusted.R2","LOOCV.R2")
  }
}



  #######   JAN-FEB - Determining the models/predictors #########################
  library(caret)
  {
    train.control <- trainControl(method = "LOOCV")
    cols<-colnames(indices)
    remove<-c("X" ,"nin1",  "nin2" ,  "nin3" ,  "nin4"  , "nin5",
              "nao1",   "nao2" ,  "nao3" ,  "nao4"   ,"nao5",
              "qbo1"  , "qbo2"  , "qbo3" ,  "qbo4" ,  "qbo5",
              "eaw1" ,  "eaw2"  , "eaw3"  , "eaw4"  , "eaw5",
              "sca1" ,  "sca2" ,  "sca3" ,  "sca4","sca5",
              "npi1" ,  "npi2" ,  "npi3","npi4",   "npi5",   "npi6",
              "dmi1"  , "dmi2" ,  "dmi3" ,  "dmi4" ,  "dmi5",
              "ao1"   , "ao2"  ,  "ao3"  ,  "ao4"  ,  "ao5",
              "pdo1" ,  "pdo2" , "pdo3" ,  "pdo4" ,  "pdo5",
              "csp1",   "csp2",   "csp3",   "csp4", "csp5",
              "moi1",   "moi2",   "moi3",   "moi4",   "moi5")
    cols<-cols[! cols %in% remove1]
    {
      
      DATA_Jan<-DATA[, data.table::shift(.SD, 1, NA, "lag", TRUE), .SDcols=cols]
      DATA_Jan<-DATA_Jan[,-1]
      DATA_Jan$year<-DATA$year
      
      
      P<-as.data.frame(prcp$p3+prcp$p4+prcp$p5)
      
      
      colnames(P)<-"psum"
      P$year<-prcp$year
    
      
      DATA_Jan<-merge(DATA_Jan,P, by="year", all=TRUE)
      
      
      library(leaps)
      regsubsets.out <- regsubsets(psum~ . -year,
                                   data = DATA_Jan,
                                   nbest = nbest,       # nmber of best model for each number of predictors
                                   nvmax = nvmax,    # limit on number of variables
                                   force.in = "nin12_lag_1", force.out = NULL,
                                   method = selection_method,
                                   really.big=T)
      
      Jan_predictors<-coef(regsubsets.out,(subsets))
      
    }
    statList<-foreach::foreach(i=1:length(Jan_predictors),.packages=c('caret') )%dopar%{
      a<-names(Jan_predictors[[i]])[-1]
      f <- paste("psum", "~",paste(names(Jan_predictors[[i]])[-1], collapse=" + "))
      
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
  remove<-c("esca1","year1","X1")
  anscols<-anscols[! anscols %in% remove]

  
  add_Feb<-data.table(DATA[,c(..anscols,"year")])
  DATA_Feb<-merge(DATA_Jan,add_Feb, by="year", all=TRUE)
  
  P<-as.data.frame(prcp$p3+prcp$p4+prcp$p5)
  
  
  colnames(P)<-"FM"
  P$year<-prcp$year
  
  
  DATA_Feb<-merge(DATA_Feb,P, by="year", all=TRUE)
  
  regsubsets.out <- regsubsets(FM ~ . -year-psum,
                               data = DATA_Feb,
                               nbest = nbest,       # nmber of best model for each number of predictors
                               nvmax = nvmax,    # limit on number of variables
                               force.in = NULL, force.out = NULL,
                               method = selection_method )
  
  Feb_predictors<-coef(regsubsets.out,(subsets))
  
  
  statList<-foreach::foreach(i=1:length(Feb_predictors),.packages=c('caret'))%dopar%{
    a<-names(Feb_predictors[[i]])[-1]
    f <- paste("FM", "~",paste(names(Feb_predictors[[i]])[-1], collapse=" + "))
    
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
  remove<-c("esca2","year2","X2")
  anscols<-anscols[! anscols %in% remove]
  
  
  add_Mar<-data.table(DATA[,c(..anscols,"year")])
  DATA_Mar<-merge(DATA_Feb,add_Mar, by="year", all=TRUE)
  
  P<-as.data.frame(prcp$p3+prcp$p4+prcp$p5)
  
  
  colnames(P)<-"MAM"
  P$year<-prcp$year
  
  
  DATA_Mar<-merge(DATA_Mar,P, by="year", all=TRUE)
  
  regsubsets.out <- regsubsets(MAM ~ . -year-psum-FM,
                               data = DATA_Mar,
                               nbest = nbest,       # nmber of best model for each number of predictors
                               nvmax = nvmax,    # limit on number of variables
                               force.in = NULL, force.out = NULL,
                               method = selection_method )
  
  Mar_predictors<-coef(regsubsets.out,(subsets))
  
  
  statList<-foreach::foreach(i=1:length(Mar_predictors),.packages=c('caret'))%dopar%{
    a<-names(Mar_predictors[[i]])[-1]
    f <- paste("MAM", "~",paste(names(Mar_predictors[[i]])[-1], collapse=" + "))
    
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
