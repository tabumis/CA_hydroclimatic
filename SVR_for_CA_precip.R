V5~oni12_lag_1+eawrm12_lag_1+naom12_lag_1+shm12_lag_1+scanm12_lag_1+pdom12_lag_1
df<-DATA_Jan[,c("year","V5","ninm12_lag_1","naom12_lag_1","shm12_lag_1","pdom11_lag_1")]
df<-DATA_Mar[,c("year","V5","ninm2","naom2","shm1")]
df<-na.omit(df)
df$V5<-scale(df$V5)
hist(df$V5)


set.seed(123) #randomization`

#random split 
{
  trainIndex <- createDataPartition(df$V5,p=0.7,list=FALSE)

#splitting data into training/testing data using the trainIndex object
dataset <- df[trainIndex,] 
testdata <- df[-trainIndex,] 
}

library(e1071)
svm_model <-  svm(formula = V5 ~ ., data = dataset, type = "eps-regression")
summary(svm_model)


predictYsvm <- predict(svm_model, dataset)

errval <- dataset$V5 - predictYsvm 
svr_RMSE <- Metrics::rmse(dataset$V5,predictYsvm)   
print(paste('svr RMSE = ', 
            svr_RMSE))


tuneResult1 <- tune(svm, V5 ~.,  data = dataset, 
                    ranges = list(epsilon = seq(0,1,0.1), cost = 2^(seq(0.5,8,.5))))

plot(tuneResult1)                   

tuneResult <- tune(svm, V5 ~.,  data = dataset,
                   ranges = list(epsilon = seq(tuneResult1$best.model$epsilon*0.5,
                                               tuneResult1$best.model$epsilon*1.5, length.out = 31),
                                 cost = seq(2^(log2(tuneResult1$best.model$cost)-1),
                                            2^(log2(tuneResult1$best.model$cost)+1),
                                            length=6))
)                                  
plot(tuneResult)
print(tuneResult)

tunedVals <-tuneResult$best.model
predictYsvm2 <- predict(tunedVals, dataset)


#check patterns of the actual and predicted
compare<-as.data.frame(dataset$V5)
colnames(compare)[1]<-"actual"
compare$predicted<-predictYsvm2

plot(compare)
hist(compare$actual)
hist(compare$predicted)

#compare actual and predicted values on barplot
compare<-as.matrix(t(compare))
barplot(compare, main="Mar-May forecast performance ",
        xlab="years", col=c("darkblue","red"),
        legend = rownames(compare), beside=TRUE,
        xpd=F)

# estimate generalized R-sqr for the train data
{compare<-as.data.frame(dataset$V5)
colnames(compare)[1]<-"actual"
compare$predicted<-predictYsvm2
train_model<-lm(actual~predicted,compare)
summary(train_model)}

{actual <- dataset$V5
  preds <- predict(tunedVals, dataset)
  rss <- sum((preds - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss/tss
  rsq
}
# predict on test/new data

predict_test <- predict(tunedVals, testdata)
compare_test<-as.data.frame(testdata$V5)
colnames(compare_test)[1]<-"actual"
compare_test$predicted<-predict_test

#compare actual and predicted values on barplot
compare_test<-as.matrix(t(compare_test))
barplot(compare_test, main="Jan-May forecast performance on new data",
        xlab="years", col=c("darkblue","red"),
        legend = rownames(compare_test), beside=TRUE,
        xpd=F)


{compare_test<-as.data.frame(testdata$V5)
colnames(compare_test)[1]<-"actual"
compare_test$predicted<-predict_test
test_model<-lm(actual~predicted,compare_test)
summary(test_model)}

{actual <- testdata$V5
  preds <- predict(tunedVals, testdata)
  rss <- sum((preds - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss/tss
  rsq
}
