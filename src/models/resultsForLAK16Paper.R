library(ggplot2)
require(caret)
require(kernlab)
require(psych)

sessions <- c("luisSession1","luisSession2","luisSession3","luisSession4")



# Loading/cleaning data ################################################################

loadData <- function(){

    dataset <- get(load("./data/totaldatasetwvideo.Rda"))
    cleaner <- dataset[dataset$value.Activity!="TEC" & dataset$value.Activity!="OFF",]
    cleaner <- cleaner[,-c(13,66:70,123)]
    cleaner$value.Activity <- factor(cleaner$value.Activity)
    cleaner    
}

loadCleanData <- function(){
    
    #dataset <- get(load("./data/totaldataset.Rda"))
    dataset <- get(load("./data/totaldatasetwvideo.Rda"))
    
    # Noninteresting values
    cleaner <- dataset[dataset$value.Activity!="TEC" & dataset$value.Activity!="OFF" & dataset$value.Social!="IND",]
    cleaner$value.Activity <- factor(cleaner$value.Activity)
    cleaner$value.Social <- factor(cleaner$value.Social)
    
    # Unneeded variables
    cleaner <- cleaner[,-c(13,66:70,123)]
    
    # Remove outliers, substituting them with NAs
    # returns a data vector of the same length, but with the outliers (defined as x times the sd, or x times the inter-quartile range)
    replaceOutliers <- function(data, value=NA, coef=3, method="sd"){
        newdata <- data
        # Method based on the inter quartile range IQR (as in boxplot)
        if(method=="iqr"){
            ind <- boxplot.stats(newdata, coef=coef)
            newdata[newdata < ind$stats[1] | newdata > ind$stats[5]] <- NA
        }else if(method=="sd"){
            # Alternative method: coef is the number of SDs around the mean for defining an outlier
            newdata[newdata < mean(newdata, na.rm=T)-coef*sd(newdata, na.rm=T) | newdata > mean(newdata, na.rm=T)+coef*sd(newdata, na.rm=T)] <- NA
        }else{
            stop('Invalid method!')
        }
        newdata
    }
    # We do the actual replacement of outliers
    noout <- cleaner
    for(i in c(3:12,15:122)){ # Before the video features
        #for(i in c(3:12,15:148)){ # Also in the video features... probably not needed to de-outlie them, as outliers may contain useful information in this case
        noout[,i] <- replaceOutliers(noout[,i], coef=5) # 5 sigma is pretty huge!
    }
    
    # Remove NAs?
    #data <- cleaner[complete.cases(cleaner),]
    data <- noout[complete.cases(noout),]

    data

}


loadCleanData1s <- function(){
    
    #dataset <- get(load("./data/totaldataset.Rda"))
    dataset <- get(load("./data/totaldatasetwvideo1sINCOMPLETE.Rda"))
    
    # Noninteresting values
    cleaner <- dataset[dataset$value.Activity!="TEC" & dataset$value.Activity!="OFF" & dataset$value.Social!="IND",]
    cleaner$value.Activity <- factor(cleaner$value.Activity)
    cleaner$value.Social <- factor(cleaner$value.Social)
    
    # Unneeded variables
    #cleaner <- cleaner[,-c(13,66:70,123)]
    cleaner <- cleaner[,-c(13,51:65,66:70)] # The scond half of FFT does not have values in 1s!
    # Remove outliers, substituting them with NAs
    # returns a data vector of the same length, but with the outliers (defined as x times the sd, or x times the inter-quartile range)
    replaceOutliers <- function(data, value=NA, coef=3, method="sd"){
        newdata <- data
        # Method based on the inter quartile range IQR (as in boxplot)
        if(method=="iqr"){
            ind <- boxplot.stats(newdata, coef=coef)
            newdata[newdata < ind$stats[1] | newdata > ind$stats[5]] <- NA
        }else if(method=="sd"){
            # Alternative method: coef is the number of SDs around the mean for defining an outlier
            newdata[newdata < mean(newdata, na.rm=T)-coef*sd(newdata, na.rm=T) | newdata > mean(newdata, na.rm=T)+coef*sd(newdata, na.rm=T)] <- NA
        }else{
            stop('Invalid method!')
        }
        newdata
    }
    # We do the actual replacement of outliers
    noout <- cleaner
    for(i in c(3:12,15:99)){ # Before the video features, without Ks data
    #for(i in c(3:12,15:122)){ # Before the video features
        #for(i in c(3:12,15:148)){ # Also in the video features... probably not needed to de-outlie them, as outliers may contain useful information in this case
        noout[,i] <- replaceOutliers(noout[,i], coef=5) # 5 sigma is pretty huge!
    }
    
    # Remove NAs?
    #data <- cleaner[complete.cases(cleaner),]
    data <- noout[complete.cases(noout),]
    
    data
    
}





# Train best X variables, progressively #############################################################

getBestModels <- function(data, ranking, varsToTry){
    
    
    #library(doMC)
    #registerDoMC(cores = 5)
    

    
    perf <- data.frame(vars=numeric(), model=character(), acc=numeric(), kappa=numeric(), inacc=numeric(), inkappa=numeric())
    for (vars in varsToTry){
        
        # We build the dataset to train
        seldata <- data[,c(1,14,ranking$var[1:vars])]
        metrics <- data.frame(model=character(), session=numeric(), acc=numeric(), kappa=numeric(), inacc=numeric(), inkappa=numeric())
        for(i in 1:4){#We use each of the sessions as test set, in turn
            
            # We divide in training and test set, using different sessions
            test <- seldata[seldata$session==sessions[i],]
            train <- seldata[seldata$session!=sessions[i],]
            
            # RF
            set.seed(66)
            rfFit <- train(value.Activity~.,data=train[,-1],method="rf", prox=T, importance=TRUE,metric="Kappa")
            cm <- confusionMatrix(predict(rfFit,newdata=test),test$value.Activity)
            #print(cm)
            print(summary(test$value.Activity))
            print(paste(i,"Accuracy of Random Forest with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
            print(summary(predicted <- predict(rfFit,newdata=test)))
            newmetric <- data.frame(model="rf", session=i, 
                                    inacc=rfFit$results$Accuracy[which.max(rfFit$results$Kappa)], 
                                    inkappa=max(rfFit$results$Kappa), 
                                    acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
            metrics <- rbind(metrics,newmetric)

            # SVM
            set.seed(66)
            library(e1071)
            #svmFit <- svm(value.Activity ~ ., data = train[,-c(ncol(train))])
            svmFit2 <- tune.svm(value.Activity ~ ., data = train[,-1], 
                                gamma = 2^(-1:1), cost = 2^(2:4), 
                                tunecontrol = tune.control(sampling = "boot", nboot=25))
            cm <- confusionMatrix(predict(svmFit2$best.model,newdata=test),test$value.Activity)
            print(paste(i,"Accuracy of SVM with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
            newmetric <- data.frame(model="svm", session=i, 
                                    inacc=svmFit2$best.performance, 
                                    inkappa=NA, 
                                    acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
            metrics <- rbind(metrics,newmetric)

        }
        metrics$model <- factor(metrics$model)
        ag <- aggregate(cbind(acc,kappa,inacc,inkappa)~model, data=metrics, mean)
        print(vars)
        newperf <- data.frame(vars=rep(vars, nrow(ag)), model=ag[,"model"], acc=ag[,"acc"], kappa=ag[,"kappa"], inacc=ag[,"inacc"], inkappa=ag[,"inkappa"])
        perf<-rbind(perf,newperf)
        print(qplot(x=perf$vars, y=perf$kappa, geom="line", col=factor(perf$model), main=vars))
        
    }
    perf$model <- factor(perf$model)
    #print(perf)
    qplot(x=perf$vars, y=perf$kappa, geom="line", col=perf$model)
    
    
    perf
}



getBestModelsSoc <- function(data, ranking, varsToTry){
  
  
  #library(doMC)
  #registerDoMC(cores = 5)
  
  
  
  perf <- data.frame(vars=numeric(), model=character(), acc=numeric(), kappa=numeric(), inacc=numeric(), inkappa=numeric())
  for (vars in varsToTry){
    
    # We build the dataset to train
    seldata <- data[,c(1,13,ranking$var[1:vars])]
    metrics <- data.frame(model=character(), session=numeric(), acc=numeric(), kappa=numeric(), inacc=numeric(), inkappa=numeric())
    for(i in 1:4){#We use each of the sessions as test set, in turn
      
      # We divide in training and test set, using different sessions
      test <- seldata[seldata$session==sessions[i],]
      train <- seldata[seldata$session!=sessions[i],]
      
      # GBM
      set.seed(66)
      gbmFit <- train(value.Social~.,data=train[,-1],method="gbm", verbose=F,metric="Kappa")
      test$predicted <- predict(gbmFit,newdata=test)
      cm <- confusionMatrix(test$predicted,test$value.Social)
      print(paste(i,"Accuracy of stochastic Gradient Boosting with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
      newmetric <- data.frame(model="gbm", session=i, 
                              inacc=gbmFit$results$Accuracy[which.max(gbmFit$results$Kappa)], 
                              inkappa=max(gbmFit$results$Kappa), 
                              acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
      metrics <- rbind(metrics,newmetric)

      # SVM
      set.seed(66)
      library(e1071)
      #svmFit <- svm(value.Activity ~ ., data = train[,-c(ncol(train)-1)])
      svmFit2 <- tune.svm(value.Social ~ ., data = train[,-1], 
                          gamma = 2^(-1:1), cost = 2^(2:4), 
                          tunecontrol = tune.control(sampling = "boot", nboot=25))
      cm <- confusionMatrix(predict(svmFit2$best.model,newdata=test),test$value.Social)
      print(paste(i,"Accuracy of SVM with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
      newmetric <- data.frame(model="svm", session=i, 
                              inacc=svmFit2$best.performance, 
                              inkappa=NA, 
                              acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
      metrics <- rbind(metrics,newmetric)
      
    }
    metrics$model <- factor(metrics$model)
    ag <- aggregate(cbind(acc,kappa,inacc,inkappa)~model, data=metrics, mean)
    print(vars)
    newperf <- data.frame(vars=rep(vars, nrow(ag)), model=ag[,"model"], acc=ag[,"acc"], kappa=ag[,"kappa"], inacc=ag[,"inacc"], inkappa=ag[,"inkappa"])
    perf<-rbind(perf,newperf)
    print(qplot(x=perf$vars, y=perf$kappa, geom="line", col=factor(perf$model), main=vars))
    
  }
  perf$model <- factor(perf$model)
  #print(perf)
  qplot(x=perf$vars, y=perf$kappa, geom="line", col=perf$model)
  
  
  perf
}






# Train best X variables, progressively, adding Markov chain probabilities #############################################################

# Filters a factor vector, changing value only when confidence is over the threshold
filterPredictionsMCAct <- function(predicted, confidence, weight, markov){
    
    lastPrediction <- NA
    filtered <- factor(levels=levels(predicted))
    for(i in 1:length(predicted)){
        
        if(is.na(lastPrediction)){ # If we do not have a previous prediction yet, we leave the prediction as it is
            filtered[i] <- predicted[i]
            lastPrediction <- predicted[i]
        }else{ # If there is a previous prediction, we add the (weighted) markov probs to the RF probs
            mcprobs <- markov[as.character(lastPrediction),]
            rfprobs <- confidence[i,]
            predprobs = data.frame(EXP=numeric(), MON=numeric(), QUE=numeric(), REP=numeric(), TDT=numeric())
            predprobs[1,"EXP"] <- (weight*mcprobs["EXP"])+rfprobs["EXP"]
            predprobs[1,"MON"] <- (weight*mcprobs["MON"])+rfprobs["MON"]
            predprobs[1,"QUE"] <- (weight*mcprobs["QUE"])+rfprobs["QUE"]
            predprobs[1,"REP"] <- (weight*mcprobs["REP"])+rfprobs["REP"]
            predprobs[1,"TDT"] <- (weight*mcprobs["TDT"])+rfprobs["TDT"]
            
            prediction <- names(which.max(predprobs))
            filtered[i] <- prediction
            lastPrediction <- filtered[i]
        }
        
    }
    
    filtered
}

filterPredictionsMCSoc <- function(predicted, confidence, weight, markov){
  
  lastPrediction <- NA
  filtered <- factor(levels=levels(predicted))
  for(i in 1:length(predicted)){
    
    if(is.na(lastPrediction)){ # If we do not have a previous prediction yet, we leave the prediction as it is
      filtered[i] <- predicted[i]
      lastPrediction <- predicted[i]
    }else{ # If there is a previous prediction, we add the (weighted) markov probs to the RF probs
      mcprobs <- markov[as.character(lastPrediction),]
      rfprobs <- confidence[i,]
      predprobs = data.frame(CLS=numeric(), GRP=numeric())
      predprobs[1,"CLS"] <- (weight*mcprobs["CLS"])+rfprobs["CLS"]
      predprobs[1,"GRP"] <- (weight*mcprobs["GRP"])+rfprobs["GRP"]
      
      prediction <- names(which.max(predprobs))
      filtered[i] <- prediction
      lastPrediction <- filtered[i]
    }
    
  }
  
  filtered
}



getBestMarkovModels <- function(data, ranking, varsToTry, weights){
    
    
    #library(doMC)
    #registerDoMC(cores = 5)
    
    
    perf <- data.frame(vars=numeric(), model=character(), acc=numeric(), kappa=numeric())
    for (vars in varsToTry){
        for(weight in weights){
            # We build the dataset to train
            seldata <- data[,c(1,14,ranking$var[1:vars])]
            metrics <- data.frame(model=character(), session=numeric(), acc=numeric(), kappa=numeric(), inacc=numeric(), inkappa=numeric())
            for(i in 1:4){#We use each of the sessions as test set, in turn
                
                # We divide in training and test set, using different sessions
                test <- seldata[seldata$session==sessions[i],]
                train <- seldata[seldata$session!=sessions[i],]
                
                # RF
                set.seed(66)
                rfFit <- train(value.Activity~.,data=train[,-1],method="rf", prox=T, importance=TRUE,metric="Kappa")
                test$predicted <- predict(rfFit,newdata=test)
                cm <- confusionMatrix(test$predicted,test$value.Activity)
                print(paste(i,"Accuracy of Random Forest with out-of-session test data: ", cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
                newmetric <- data.frame(model="rf", session=i, 
                                        inacc=rfFit$results$Accuracy[which.max(rfFit$results$Kappa)], 
                                        inkappa=max(rfFit$results$Kappa), 
                                        acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
                metrics <- rbind(metrics,newmetric)
                
                # We train and adjust for the markov chain
                require('markovchain')
                mcAct <- markovchainFit(data = train$value.Activity)
                tmAct <- mcAct$estimate
                show(tmAct)
                confidences <- predict(rfFit,newdata=test,"prob")
                test$predicted2 <- filterPredictionsMCAct(test$predicted,confidences,weight,tmAct)
                cm <- confusionMatrix(test$predicted2,test$value.Activity)
                print(paste(weight,i,"Accuracy of Random Forest MARKOV CHAIN with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
                newmetric <- data.frame(model=paste("rfMC",weight,sep=""), 
                                        inacc=NA, 
                                        inkappa=NA, 
                                        session=i, acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
                metrics <- rbind(metrics,newmetric)
                
                
                
                # SVM
#                 set.seed(66)
#                 library(e1071)
#                 #svmFit <- svm(value.Activity ~ ., data = train[,-c(ncol(train)-1)])
#                 svmFit2 <- tune.svm(value.Activity ~ ., data = train[,-1], probability=T,
#                                     gamma = 2^(-1:1), cost = 2^(2:4), 
#                                     tunecontrol = tune.control(sampling = "boot", nboot=25))
#                 test$predicted <- predict(svmFit2$best.model,newdata=test)
#                 cm <- confusionMatrix(test$predicted,test$value.Activity)
#                 print(paste(i,"Accuracy of SVM with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#                 newmetric <- data.frame(model="svm", session=i, 
#                                         inacc=svmFit2$best.performance, 
#                                         inkappa=NA, 
#                                         acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#                 metrics <- rbind(metrics,newmetric)
#                 
#                 confidences <- predict(svmFit2$best.model,newdata=test,probability=T)
#                 test$predicted2 <- filterPredictionsMCAct(test$predicted,attr(confidences, "probabilities"),weight,tmAct)
#                 cm <- confusionMatrix(test$predicted2,test$value.Activity)
#                 print(paste(weight,i,"Accuracy of SVM MARKOV CHAIN with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#                 newmetric <- data.frame(model=paste("svmMC",weight,sep=""), 
#                                         inacc=NA, 
#                                         inkappa=NA, 
#                                         session=i, acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#                 metrics <- rbind(metrics,newmetric)
#                 
                
            }
            metrics$model <- factor(metrics$model)
            ag <- aggregate(cbind(acc,kappa)~model, data=metrics, mean)
            print(vars)
            newperf <- data.frame(vars=rep(vars, nrow(ag)), model=ag[,"model"], acc=ag[,"acc"], kappa=ag[,"kappa"])
            perf<-rbind(perf,newperf)
            print(qplot(x=perf$vars, y=perf$kappa, geom="line", col=factor(perf$model), main=paste(vars,weight)))
        }
    }
    perf$model <- factor(perf$model)
    #print(perf)
    qplot(x=perf$vars, y=perf$kappa, geom="line", col=perf$model)
    
    
    perf
}


getBestMarkovModelsSoc <- function(data, ranking, varsToTry, weights){
  
  
  #library(doMC)
  #registerDoMC(cores = 5)
  
  
  perf <- data.frame(vars=numeric(), model=character(), acc=numeric(), kappa=numeric())
  for (vars in varsToTry){
    for(weight in weights){
      # We build the dataset to train
      seldata <- data[,c(1,13,ranking$var[1:vars])]
      metrics <- data.frame(model=character(), session=numeric(), acc=numeric(), kappa=numeric(), inacc=numeric(), inkappa=numeric())
      for(i in 1:4){#We use each of the sessions as test set, in turn
        
        # We divide in training and test set, using different sessions
        test <- seldata[seldata$session==sessions[i],]
        train <- seldata[seldata$session!=sessions[i],]
        
        # GBM
        set.seed(66)
        gbmFit <- train(value.Social~.,data=train[,-1],method="gbm", verbose=F,metric="Kappa")
        test$predicted <- predict(gbmFit,newdata=test)
        cm <- confusionMatrix(test$predicted,test$value.Social)
        print(paste(i,"Accuracy of stochastic Gradient Boosting with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
        newmetric <- data.frame(model="gbm", session=i, 
                                inacc=gbmFit$results$Accuracy[which.max(gbmFit$results$Kappa)], 
                                inkappa=max(gbmFit$results$Kappa), 
                                acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
        metrics <- rbind(metrics,newmetric)
        # We train and adjust for the markov chain
        require('markovchain')
        mcAct <- markovchainFit(data = train$value.Social)
        tmAct <- mcAct$estimate
        show(tmAct)
        confidences <- predict(gbmFit,newdata=test,"prob")
        test$predicted2 <- filterPredictionsMCSoc(test$predicted,confidences,weight,tmAct)
        cm <- confusionMatrix(test$predicted2,test$value.Social)
        print(paste(weight,i,"Accuracy of Random Forest MARKOV CHAIN with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
        newmetric <- data.frame(model=paste("rfMC",weight,sep=""), 
                                inacc=NA, 
                                inkappa=NA, 
                                session=i, acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
        metrics <- rbind(metrics,newmetric)
        
        
        
        # SVM
        set.seed(66)
        library(e1071)
        #svmFit <- svm(value.Activity ~ ., data = train[,-c(ncol(train)-1)])
        svmFit2 <- tune.svm(value.Social ~ ., data = train[,-1], probability=T,
                            gamma = 2^(-1:1), cost = 2^(2:4), 
                            tunecontrol = tune.control(sampling = "boot", nboot=25))
        test$predicted <- predict(svmFit2$best.model,newdata=test)
        cm <- confusionMatrix(test$predicted,test$value.Social)
        print(paste(i,"Accuracy of SVM with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
        newmetric <- data.frame(model="svm", session=i, 
                                inacc=svmFit2$best.performance, 
                                inkappa=NA, 
                                acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
        metrics <- rbind(metrics,newmetric)
        
        confidences <- predict(svmFit2$best.model,newdata=test,probability=T)
        test$predicted2 <- filterPredictionsMCSoc(test$predicted,attr(confidences, "probabilities"),weight,tmAct)
        cm <- confusionMatrix(test$predicted2,test$value.Social)
        print(paste(weight,i,"Accuracy of SVM MARKOV CHAIN with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
        newmetric <- data.frame(model=paste("svmMC",weight,sep=""), 
                                inacc=NA, 
                                inkappa=NA, 
                                session=i, acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
        metrics <- rbind(metrics,newmetric)
        
        
      }
      metrics$model <- factor(metrics$model)
      ag <- aggregate(cbind(acc,kappa)~model, data=metrics, mean)
      print(vars)
      newperf <- data.frame(vars=rep(vars, nrow(ag)), model=ag[,"model"], acc=ag[,"acc"], kappa=ag[,"kappa"])
      perf<-rbind(perf,newperf)
      print(qplot(x=perf$vars, y=perf$kappa, geom="line", col=factor(perf$model), main=paste(vars,weight)))
    }
  }
  perf$model <- factor(perf$model)
  #print(perf)
  qplot(x=perf$vars, y=perf$kappa, geom="line", col=perf$model)
  
  
  perf
}






# Train in whole dataset #####################################################################

getWinningModels <- function(data, type="all"){
    
    # We select the columns depending on the data source
    if(type=="all") cols <- c(1:10,13:108,11,12)
#     else if(type=="et") cols <- c(1:10,11,12)
#     else if(type=="eeg") cols <- c(13:24,11,12)
#     else if(type=="acc") cols <- c(25:62,11,12)
#     else if(type=="audio") cols <- c(63:120,11,12)
#     else if(type=="video") cols <- c(121:146,11,12)
#     else if(type=="etaudiovideo") cols <- c(1:10,63:146,11,12)
#     else if(type=="audiovideo") cols <- c(63:146,11,12)
    
    
    
    
#    library(doMC)
#    registerDoMC(cores = 5)
    
    metrics <- data.frame(model=character(), session=numeric(), inacc=numeric(), inkappa=numeric(), acc=numeric(), kappa=numeric())
    for(i in 1:4){#We use each of the sessions as test set, in turn
        
        # We divide in training and test set, using different sessions
        test <- data[data$session==sessions[i],]
        train <- data[data$session!=sessions[i],]
        
        # We eliminate the session, time and reorder the columns to have the target variables last
        train <- train[,-c(1,2)]
        # train <- train[,c(1:10,13:120,11,12)]
        train <- train[,cols]
        
        # Remove variables with no variance
        nzv <- length(nearZeroVar(train))
        if(nzv>0) train <- train[,-nearZeroVar(train)]
        # Remove highly-correlated (>0.9) variables
        correlated <- findCorrelation(cor(train[,-c(ncol(train)-1,ncol(train))], use = "complete.obs"))
        if(length(correlated)>0) train <- train[, -correlated]
#         # decision tree
#         set.seed(66)
#         treeFit <- train(value.Activity~.,data=train[,-c(ncol(train)-1)],method="rpart",metric="Kappa")
#         cm <- confusionMatrix(predict(treeFit,newdata=test),test$value.Activity)
#         print(paste(i,"Accuracy of Decision Tree with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="rpart", session=i, 
#                                 inacc=treeFit$results$Accuracy[which.max(treeFit$results$Kappa)], 
#                                 inkappa=max(treeFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
#         
#         # naive bayes
#         set.seed(66)
#         nbFit <- train(value.Activity~.,data=train[,-c(ncol(train)-1)],method="nb",metric="Kappa")
#         cm <- confusionMatrix(predict(nbFit,newdata=test),test$value.Activity)
#         print(paste(i,"Accuracy of Naive Bayes with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="nb", session=i, 
#                                 inacc=nbFit$results$Accuracy[which.max(nbFit$results$Kappa)], 
#                                 inkappa=max(nbFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
#         
#         # k nearest neighbors
#         set.seed(66)
#         knnFit <- train(value.Activity~.,data=train[,-c(ncol(train)-1)],method="knn",metric="Kappa")
#         cm <- confusionMatrix(predict(knnFit,newdata=test),test$value.Activity)
#         print(paste(i,"Accuracy of KNN with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="knn", session=i, 
#                                 inacc=knnFit$results$Accuracy[which.max(knnFit$results$Kappa)], 
#                                 inkappa=max(knnFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
#         
#         # gradient boosting 
#         set.seed(66)
#         gbmFit <- train(value.Activity~.,data=train[,-c(ncol(train)-1)],method="gbm", verbose=F,metric="Kappa")
#         cm <- confusionMatrix(predict(gbmFit,newdata=test),test$value.Activity)
#         print(paste(i,"Accuracy of stochastic Gradient Boosting with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="gbm", session=i, 
#                                 inacc=gbmFit$results$Accuracy[which.max(gbmFit$results$Kappa)], 
#                                 inkappa=max(gbmFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
#         
#         # Bagged CART
#         set.seed(66)
#         bcartFit <- train(value.Activity~.,data=train[,-c(ncol(train)-1)],method="treebag",metric="Kappa")
#         cm <- confusionMatrix(predict(bcartFit,newdata=test),test$value.Activity)
#         print(paste(i,"Accuracy of Bagged Trees with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="bcart", session=i, 
#                                 inacc=bcartFit$results$Accuracy[which.max(bcartFit$results$Kappa)], 
#                                 inkappa=max(bcartFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
        
        # Random forest
        set.seed(66)
        rfFit <- train(value.Activity~.,data=train[,-c(ncol(train)-1)],method="rf", prox=T, importance=TRUE,metric="Kappa")
        cm <- confusionMatrix(predict(rfFit,newdata=test),test$value.Activity)
        print(paste(i,"Accuracy of Random Forest with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
        newmetric <- data.frame(model="rf", session=i, 
                                inacc=rfFit$results$Accuracy[which.max(rfFit$results$Kappa)], 
                                inkappa=max(rfFit$results$Kappa), 
                                acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
        metrics <- rbind(metrics,newmetric)
        
#         # Penalized Multinomial Regression
#         set.seed(66)
#         multFit <- train(value.Activity~.,data=train[,-c(ncol(train)-1)],method="multinom",metric="Kappa")
#         cm <- confusionMatrix(predict(multFit,newdata=test),test$value.Activity)
#         print(paste(i,"Accuracy of Penalized Multinomial Regression with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="multinom", session=i, 
#                                 inacc=multFit$results$Accuracy[which.max(multFit$results$Kappa)], 
#                                 inkappa=max(multFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
#         
#         # SVM
#         set.seed(66)
#         library(e1071)
#         #svmFit <- svm(value.Activity ~ ., data = train[,-c(ncol(train)-1)])
#         svmFit2 <- tune.svm(value.Activity ~ ., data = train[,-c(ncol(train)-1)], 
#                             gamma = 2^(-1:1), cost = 2^(2:4), 
#                             tunecontrol = tune.control(sampling = "boot", nboot=25))
#         cm <- confusionMatrix(predict(svmFit2$best.model,newdata=test),test$value.Activity)
#         print(paste(i,"Accuracy of SVM with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="svm", session=i, 
#                                 inacc=svmFit2$best.performance, 
#                                 inkappa=NA, 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
        
        # results <- resamples(listlist(NB=nbFit, TREE=treeFit, BAG=bcartFit, RF=rfFit, KNN=knnFit, GBM=gbmFit, SVM=svmFit, MULTINOM=multFit))
        #summary(results)
        #bwplot(results)
        
    }
    metrics$model <- factor(metrics$model)
    ag <- aggregate(cbind(acc,kappa,inacc,inkappa)~model, data=metrics, mean)
    print(ag)
    ag2 <- aggregate(cbind(acc,kappa,inacc,inkappa)~model, data=metrics, sd)
    print(ag2)
    
    metrics
}


getWinningModelsSoc <- function(data, type="all"){
    
    # We select the columns depending on the data source
    if(type=="all") cols <- c(1:10,13:146,11,12)
    else if(type=="et") cols <- c(1:10,11,12)
    else if(type=="eeg") cols <- c(13:24,11,12)
    else if(type=="acc") cols <- c(25:62,11,12)
    else if(type=="audio") cols <- c(63:120,11,12)
    else if(type=="video") cols <- c(121:146,11,12)
    else if(type=="etaudiovideo") cols <- c(1:10,63:146,11,12)
    else if(type=="audiovideo") cols <- c(63:146,11,12)
    
    
    
    
#     library(doMC)
#     registerDoMC(cores = 5)
    
    metrics <- data.frame(model=character(), session=numeric(), inacc=numeric(), inkappa=numeric(), acc=numeric(), kappa=numeric())
    for(i in 1:4){#We use each of the sessions as test set, in turn
        
        # We divide in training and test set, using different sessions
        test <- data[data$session==sessions[i],]
        train <- data[data$session!=sessions[i],]
        
        # We eliminate the session, time and reorder the columns to have the target variables last
        train <- train[,-c(1,2)]
        # train <- train[,c(1:10,13:120,11,12)]
        train <- train[,cols]
        
        # Remove variables with no variance
        nzv <- length(nearZeroVar(train))
        if(nzv>0) train <- train[,-nearZeroVar(train)]
        # Remove highly-correlated (>0.9) variables
        correlated <- findCorrelation(cor(train[,-c(ncol(train)-1,ncol(train))], use = "complete.obs"))
        if(length(correlated)>0) train <- train[, -correlated]
#         # decision tree
#         set.seed(66)
#         treeFit <- train(value.Social~.,data=train[,-c(ncol(train))],method="rpart",metric="Kappa")
#         cm <- confusionMatrix(predict(treeFit,newdata=test),test$value.Social)
#         print(paste(i,"Accuracy of Decision Tree with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="rpart", session=i, 
#                                 inacc=treeFit$results$Accuracy[which.max(treeFit$results$Kappa)], 
#                                 inkappa=max(treeFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
#         
#         # naive bayes
#         set.seed(66)
#         nbFit <- train(value.Social~.,data=train[,-c(ncol(train))],method="nb",metric="Kappa")
#         cm <- confusionMatrix(predict(nbFit,newdata=test),test$value.Social)
#         print(paste(i,"Accuracy of Naive Bayes with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="nb", session=i, 
#                                 inacc=nbFit$results$Accuracy[which.max(nbFit$results$Kappa)], 
#                                 inkappa=max(nbFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
#         
#         # k nearest neighbors
#         set.seed(66)
#         knnFit <- train(value.Social~.,data=train[,-c(ncol(train))],method="knn",metric="Kappa")
#         cm <- confusionMatrix(predict(knnFit,newdata=test),test$value.Social)
#         print(paste(i,"Accuracy of KNN with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="knn", session=i, 
#                                 inacc=knnFit$results$Accuracy[which.max(knnFit$results$Kappa)], 
#                                 inkappa=max(knnFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
        
        # gradient boosting 
        set.seed(66)
        gbmFit <- train(value.Social~.,data=train[,-c(ncol(train))],method="gbm", verbose=F,metric="Kappa")
        cm <- confusionMatrix(predict(gbmFit,newdata=test),test$value.Social)
        print(paste(i,"Accuracy of stochastic Gradient Boosting with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
        newmetric <- data.frame(model="gbm", session=i, 
                                inacc=gbmFit$results$Accuracy[which.max(gbmFit$results$Kappa)], 
                                inkappa=max(gbmFit$results$Kappa), 
                                acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
        metrics <- rbind(metrics,newmetric)
        
#         # Bagged CART
#         set.seed(66)
#         bcartFit <- train(value.Social~.,data=train[,-c(ncol(train))],method="treebag",metric="Kappa")
#         cm <- confusionMatrix(predict(bcartFit,newdata=test),test$value.Social)
#         print(paste(i,"Accuracy of Bagged Trees with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="bcart", session=i, 
#                                 inacc=bcartFit$results$Accuracy[which.max(bcartFit$results$Kappa)], 
#                                 inkappa=max(bcartFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
#         
#         # Random forest
#         set.seed(66)
#         rfFit <- train(value.Social~.,data=train[,-c(ncol(train))],method="rf", prox=T, importance=TRUE,metric="Kappa")
#         cm <- confusionMatrix(predict(rfFit,newdata=test),test$value.Social)
#         print(paste(i,"Accuracy of Random Forest with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="rf", session=i, 
#                                 inacc=rfFit$results$Accuracy[which.max(rfFit$results$Kappa)], 
#                                 inkappa=max(rfFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
#         
#         # Penalized Multinomial Regression
#         set.seed(66)
#         multFit <- train(value.Social~.,data=train[,-c(ncol(train))],method="multinom",metric="Kappa")
#         cm <- confusionMatrix(predict(multFit,newdata=test),test$value.Social)
#         print(paste(i,"Accuracy of Penalized Multinomial Regression with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="multinom", session=i, 
#                                 inacc=multFit$results$Accuracy[which.max(multFit$results$Kappa)], 
#                                 inkappa=max(multFit$results$Kappa), 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
#         
#         # SVM
#         set.seed(66)
#         library(e1071)
#         svmFit <- svm(value.Social ~ ., data = train[,-c(ncol(train))])
#         #svmFit2 <- tune.svm(value.Social ~ ., data = train[,-c(ncol(train))], 
#         #                    gamma = 2^(-1:1), cost = 2^(2:4), 
#         #                    tunecontrol = tune.control(sampling = "boot", nboot=25))
#         cm <- confusionMatrix(predict(svmFit,newdata=test),test$value.Social)
#         cm2 <- confusionMatrix(predict(svmFit,newdata=train),train$value.Social)
#         print(paste(i,"Accuracy of SVM with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
#         newmetric <- data.frame(model="svm", session=i, 
#                                 inacc=cm2$overall["Accuracy"], 
#                                 inkappa=cm2$overall["Kappa"], 
#                                 acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
#         metrics <- rbind(metrics,newmetric)
#         
#         results <- resamples(list(NB=nbFit, TREE=treeFit, BAG=bcartFit, RF=rfFit, KNN=knnFit, GBM=gbmFit, MULTINOM=multFit))
#         print(summary(results))
#         print(bwplot(results))
        
    }
    metrics$model <- factor(metrics$model)
    ag <- aggregate(cbind(acc,kappa,inacc,inkappa)~model, data=metrics, mean)
    print(ag)
    ag2 <- aggregate(cbind(acc,kappa,inacc,inkappa)~model, data=metrics, sd)
    print(ag2)
    
    metrics
}




# Get the ranking from a whole-dataset GBM model

getGBMRankings <- function(data){
  
  # We select the columns depending on the data source
  cols <- c(1:10,13:146,11,12)

  importances <- data.frame()
  
  metrics <- data.frame(model=character(), session=numeric(), inacc=numeric(), inkappa=numeric(), acc=numeric(), kappa=numeric())
  for(i in 1:4){#We use each of the sessions as test set, in turn
    
    # We divide in training and test set, using different sessions
    test <- data[data$session==sessions[i],]
    train <- data[data$session!=sessions[i],]
    
    # We eliminate the session, time and reorder the columns to have the target variables last
    train <- train[,-c(1,2)]
    # train <- train[,c(1:10,13:120,11,12)]
    train <- train[,cols]
    
    # Remove variables with no variance
    nzv <- length(nearZeroVar(train))
    if(nzv>0) train <- train[,-nearZeroVar(train)]
    # Remove highly-correlated (>0.9) variables
    correlated <- findCorrelation(cor(train[,-c(ncol(train)-1,ncol(train))], use = "complete.obs"))
    if(length(correlated)>0) train <- train[, -correlated]

    # Random forest
    set.seed(66)
    gbmFit <- train(value.Social~.,data=train[,-c(ncol(train))],method="gbm", verbose=F,metric="Kappa")
    cm <- confusionMatrix(predict(gbmFit,newdata=test),test$value.Social)
    print(paste(i,"Accuracy of stochastic Gradient Boosting with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
    newmetric <- data.frame(model="gbm", session=i, 
                            inacc=gbmFit$results$Accuracy[which.max(gbmFit$results$Kappa)], 
                            inkappa=max(gbmFit$results$Kappa), 
                            acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
    metrics <- rbind(metrics,newmetric)
    
    # results <- resamples(listlist(NB=nbFit, TREE=treeFit, BAG=bcartFit, RF=rfFit, KNN=knnFit, GBM=gbmFit, SVM=svmFit, MULTINOM=multFit))
    #summary(results)
    #bwplot(results)
    
    #rfImp <- varImp(rfFit, scale = TRUE)
    #print(plot(rfImp, top = 20))
    gbmImp <- as.data.frame(varImp(gbmFit)$importance)
    gbmImp$var <- rownames(gbmImp)
    if(nrow(importances)==0) importances <- gbmImp
    else importances <- merge(importances,gbmImp,by="var",all=T)
    
  }
#   metrics$model <- factor(metrics$model)
#   ag <- aggregate(cbind(acc,kappa,inacc,inkappa)~model, data=metrics, mean)
#   print(ag)
#   ag2 <- aggregate(cbind(acc,kappa,inacc,inkappa)~model, data=metrics, sd)
#   print(ag2)
  
  importances
}


# Get the ranking from a whole-dataset RF model

#TODO: This might be biased by correlated predictors, see http://alandgraf.blogspot.ch/2012/07/random-forest-variable-importance.html for indications of how to get unbiased measure (takes time!)

getRFRankings <- function(data){
  
  # We select the columns depending on the data source
  cols <- c(1:10,13:146,11,12)

  importances <- data.frame()
  
  metrics <- data.frame(model=character(), session=numeric(), inacc=numeric(), inkappa=numeric(), acc=numeric(), kappa=numeric())
  for(i in 1:4){#We use each of the sessions as test set, in turn
    
    # We divide in training and test set, using different sessions
    test <- data[data$session==sessions[i],]
    train <- data[data$session!=sessions[i],]
    
    # We eliminate the session, time and reorder the columns to have the target variables last
    train <- train[,-c(1,2)]
    # train <- train[,c(1:10,13:120,11,12)]
    train <- train[,cols]
    
    # Remove variables with no variance
    nzv <- length(nearZeroVar(train))
    if(nzv>0) train <- train[,-nearZeroVar(train)]
    # Remove highly-correlated (>0.9) variables
    correlated <- findCorrelation(cor(train[,-c(ncol(train)-1,ncol(train))], use = "complete.obs"))
    if(length(correlated)>0) train <- train[, -correlated]
    
    # Random forest
    set.seed(66)
    rfFit <- train(value.Activity~.,data=train[,-c(ncol(train)-1)],method="rf", prox=T, importance=TRUE,metric="Kappa")
    cm <- confusionMatrix(predict(rfFit,newdata=test),test$value.Activity)
    print(paste(i,"Accuracy of Random Forest with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
    newmetric <- data.frame(model="rf", session=i, 
                            inacc=rfFit$results$Accuracy[which.max(rfFit$results$Kappa)], 
                            inkappa=max(rfFit$results$Kappa), 
                            acc=cm$overall["Accuracy"], kappa=cm$overall["Kappa"])
    metrics <- rbind(metrics,newmetric)
    
    # results <- resamples(listlist(NB=nbFit, TREE=treeFit, BAG=bcartFit, RF=rfFit, KNN=knnFit, GBM=gbmFit, SVM=svmFit, MULTINOM=multFit))
    #summary(results)
    #bwplot(results)
    
    #rfImp <- varImp(rfFit, scale = TRUE)
    #print(plot(rfImp, top = 20))
    rfImp <- as.data.frame(importance(rfFit$finalModel, type=1))
    rfImp$var <- rownames(rfImp)
    if(nrow(importances)==0) importances <- rfImp
    else importances <- merge(importances,rfImp,by="var",all=T)
    
  }
  #   metrics$model <- factor(metrics$model)
  #   ag <- aggregate(cbind(acc,kappa,inacc,inkappa)~model, data=metrics, mean)
  #   print(ag)
  #   ag2 <- aggregate(cbind(acc,kappa,inacc,inkappa)~model, data=metrics, sd)
  #   print(ag2)
  
  importances
}




# Cohens variable ranking ########################################################################

# TODO: We could remove highly correlated vars!

getCohenRankingActivity <- function(data){
    
    cohens <- data
    perf <- data.frame(var=numeric(),effect=numeric())
    for (i in c(3:12,15:ncol(cohens))){
        m<-anova(lm(cohens[,i]~cohens$value.Activity,na.action=na.omit))
        
        if(m$Sum[2]>0){
            perf[i,"var"] <- i
            perf[i,"effect"] <- m$Sum[1]/m$Sum[2]
        }
    }
    perf$varname <- names(cohens)[perf$var]
    library(plyr)
    ranking <- arrange(perf,desc(perf$effect))
    ranking <- ranking[complete.cases(ranking),]
    
    print(ranking)
    
    ranking
}

getCohenRankingSocial <- function(data){
    
    cohens <- data
    perf <- data.frame(var=numeric(),effect=numeric())
    for (i in c(3:12,15:ncol(cohens))){
        m<-anova(lm(cohens[,i]~cohens$value.Social,na.action=na.omit))
        
        if(m$Sum[2]>0){
            perf[i,"var"] <- i
            perf[i,"effect"] <- m$Sum[1]/m$Sum[2]
        }
    }
    perf$varname <- names(cohens)[perf$var]
    library(plyr)
    ranking <- arrange(perf,desc(perf$effect))
    ranking <- ranking[complete.cases(ranking),]
    
    print(ranking)
    
    ranking
}


joinMONREP <- function(data){
  
  levels(data$value.Activity) <- c("TDT", "EXP", "QUE", "MONREP", "MONREP")
  
  data
  
}


data <- loadCleanData()

ggplot(data, aes(x = KAA.env.kurt, fill = value.Activity)) + geom_density(alpha = 0.5)
#  metricsall <- getWinningModels(data)
#  metricset <- getWinningModels(data,"et")
#  metricseeg <- getWinningModels(data,"eeg")
#  metricsacc <- getWinningModels(data,"acc")
#  metricsaudio <- getWinningModels(data,"audio")
#  metricsvideo <- getWinningModels(data,"video")
#  metricsetaudiovideo <- getWinningModels(data,"etaudiovideo")
#  metricsaudiovide <- getWinningModels(data,"audiovideo")

#save(metricsall, metricset, metricseeg, metricsacc, metricsaudio, metricsvideo,
#     metricsetaudiovideo, metricsaudiovide, file="metricsactivity.Rda")

#rankAct <- getCohenRankingActivity(data)
#metricsbest <- getBestModels(data,rankAct,c(30,40,50,60,70,80,90,100,110))
#metricsbest <- getBestModels(data,rankAct,c(1:10,15,20,25,30,50,100))
#metricsbest <- getBestModels(data,rankAct,c(80))
#data <- joinMONREP(data)
#rankAct <- getCohenRankingActivity(data)
#metricsbest <- getBestModels(data,rankAct,c(7,80))
#save(metricsbest, file="metricsbestactivity.Rda")
#metricsbest$mod <- metricsbest$model
#metricsbest$Model <- factor(metricsbest$mod,labels=c("Random Forest","SVM"))
#ggplot(data=metricsbest[metricsbest$vars<50,], aes(x=vars, y=kappa, col=Model))+geom_line()+theme_bw()+xlab("Nr. of top variables considered")+ylab("Kappa")+ggtitle("Teacher activity")
#rankSoc <- getCohenRankingSocial(data)
#metricsbestSoc <- getBestModelsSoc(data,rankSoc,c(5,81))

# rfImportances <- getRFRankings(data)
# rfImportances$mean <- apply(rfImportances[,2:5],1,mean,na.rm=T)
# rfImportances <- rfImportances[order(rfImportances$mean,decreasing = T),]
# rfImportances$rank <- 1:nrow(rfImportances)
# print(rfImportances[,c(1,6,7)])

#gbmImportances <- getGBMRankings(data)
#gbmImportances$mean <- apply(gbmImportances[,2:5],1,mean,na.rm=T)
#gbmImportances <- gbmImportances[order(gbmImportances$mean,decreasing = T),]
#gbmImportances$rank <- 1:nrow(gbmImportances)
#print(gbmImportances[,c(1,6,7)])

#metricsmarkov <- getBestMarkovModels(data,rankAct,c(7,80),c(0,0.1,0.2,0.3,0.4,0.5,1))
#metricsmarkovSoc <- getBestMarkovModelsSoc(data,rankSoc,c(5,81),c(0.3,0.5))
#metricsmarkovSoc2 <- getBestMarkovModelsSoc(data,rankSoc,c(5,81),c(0.05,0.1,0.2))

#metricsmarkov <- getBestMarkovModels(data,rankAct,c(7,80),c(0,0.1,0.2,0.3,0.4,0.5,1))
#metricsmarkov2 <- getBestMarkovModels(data,rankAct,c(80),c(0.6,0.7,0.8))

# metricsall <- getWinningModelsSoc(data)
# metricset <- getWinningModelsSoc(data,"et")
# metricseeg <- getWinningModelsSoc(data,"eeg")
# metricsacc <- getWinningModelsSoc(data,"acc")
# metricsaudio <- getWinningModelsSoc(data,"audio")
# metricsvideo <- getWinningModelsSoc(data,"video")
# metricsetaudiovideo <- getWinningModelsSoc(data,"etaudiovideo")
# metricsaudiovide <- getWinningModelsSoc(data,"audiovideo")

#save(metricsall, metricset, metricseeg, metricsacc, metricsaudio, metricsvideo,
#     metricsetaudiovideo, metricsaudiovide, file="metricssocial.Rda")

#aggregate(cbind(acc,kappa,inacc,inkappa)~model, data=metricsall, mean)


# Plotting orchestration graphs
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}


# # ORch graphs session 3 is the only one without gaps
# data <- loadData()
# graphdata <- data[data$session=="luisSession3" & data$value.Activity!="TEC" & data$value.Activity!="OFF",]
# graphdata <- graphdata[complete.cases(graphdata$value.Social),]
# graphdata$value.Activity <- factor(graphdata$value.Activity)
# graphdata <- graphdata[!is.na(graphdata$value.Activity),]
# graphdata$value.Social = factor(graphdata$value.Social,levels(graphdata$value.Social)[c(3,2,1)])
# graphdata <- graphdata[order(graphdata$time),]
# graphdata$planned.Activity <- graphdata$value.Activity
# graphdata$planned.Social <- graphdata$value.Social
# 
# #write.csv(graphdata[,c("session","time","value.Activity","planned.Activity","value.Social","planned.Social")], file = "orchgraphdata.csv")
# dataplan <- read.csv("orchgraphdata-mod.csv")
# graphdata$planned.Activity <- factor(dataplan$planned.Activity,levels=c("TDT","EXP","QUE","MON","REP"))
# graphdata$planned.Social <- factor(dataplan$planned.Social, levels=c("IND","GRP","CLS"))
# 
# levels(graphdata$value.Activity) <- c("Task distribution","Explanation","Questioning","Monitoring","Repairs")
# levels(graphdata$value.Social) <- c("Individual","Small group","Whole class")
# levels(graphdata$planned.Activity) <- c("Task distribution","Explanation","Questioning","Monitoring","Repairs")
# levels(graphdata$planned.Social) <- c("Individual","Small group","Whole class")
# 
# # The planned values
# g1 <- ggplot(graphdata,aes(x=time/60000,y=planned.Social)) + 
#     geom_segment(aes(colour = planned.Activity, xend=(time+(5000))/60000, yend=planned.Social), size=20, alpha=0.5) + 
#     scale_colour_brewer(palette="Set1") + theme_bw() + theme(legend.position="top") + theme(legend.text = element_text(size = 14)) + theme(plot.title = element_text(face="bold", size=18)) +
#     xlab("Time (min.)") + ylab("Social plane") + ggtitle("Lesson plan") + theme(axis.text = element_text(size=14)) +
#     guides(color = guide_legend(title = "Teacher activity", title.theme = element_text(size=15,angle=0,face="bold")))
# 
# 
# # The actual values
# g2 <- ggplot(graphdata[graphdata$value.Social!="Individual",],aes(x=time/60000,y=value.Social)) + 
#     geom_segment(aes(colour = value.Activity, xend=(time+(5000))/60000, yend=value.Social), size=20, alpha=0.5) + 
#     scale_colour_brewer(palette="Set1") + theme_bw() + theme(legend.position="top") + theme(legend.text = element_text(size = 14)) + theme(plot.title = element_text(face="bold", size=18)) +
#     xlab("Time (min.)") + ylab("Social plane") + ggtitle("Actual enactment") + theme(axis.text = element_text(size=14)) +
#     guides(color = guide_legend(title = "Teacher activity", title.theme = element_text(size=15,angle=0,face="bold"))) 
# 
# # The predicted values
# # First, we train the best models, and predict
# 
# data <- loadCleanData()
# library(doMC)
# registerDoMC(cores = 5)
# rankAct <- getCohenRankingActivity(data)
# rankSoc <- getCohenRankingSocial(data)
# # We do this RF + MC by hand on the other sessions, and predict activity for session 3
# #metricsmarkov2 <- getBestMarkovModels(data,rankAct,c(80),c(0.6))
# vars <- 80
# weight <- 0.6
# i<-3
# seldata <- data[,c(1,2,14,rankAct$var[1:vars])]
# test <- seldata[seldata$session==sessions[i],]
# train <- seldata[seldata$session!=sessions[i],]
# set.seed(66)
# rfFit <- train(value.Activity~.,data=train[,-c(1,2)],method="rf", prox=T, importance=TRUE,metric="Kappa")
# test$predicted <- predict(rfFit,newdata=test)
# cm <- confusionMatrix(test$predicted,test$value.Activity)
# print(paste(i,"Accuracy of Random Forest with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
# # We train and adjust for the markov chain
# require('markovchain')
# mcAct <- markovchainFit(data = train$value.Activity)
# tmAct <- mcAct$estimate
# show(tmAct)
# confidences <- predict(rfFit,newdata=test,"prob")
# test$predicted2 <- filterPredictionsMCAct(test$predicted,confidences,weight,tmAct)
# cm <- confusionMatrix(test$predicted2,test$value.Activity)
# print(paste(weight,i,"Accuracy of Random Forest MARKOV CHAIN with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
# graphtest <- test
# graphtest$predicted.Activity <- test$predicted2
# 
# # We do this GBM by hand on the other sessions, and predict social for session 3
# #metricsbest <- getBestModelsSoc(data,rankSoc,c(81))
# vars <- 81
# i<-3
# seldata <- data[,c(1,2,13,rankSoc$var[1:vars])]
# test <- seldata[seldata$session==sessions[i],]
# train <- seldata[seldata$session!=sessions[i],]
# set.seed(66)
# gbmFit <- train(value.Social~.,data=train[,-c(1,2)],method="gbm", verbose=F,metric="Kappa")
# test$predicted <- predict(gbmFit,newdata=test)
# cm <- confusionMatrix(test$predicted,test$value.Social)
# print(paste(i,"Accuracy of Random Forest with out-of-session test data: ",cm$overall["Accuracy"]," - Kappa: ",cm$overall["Kappa"]))
# graphtest$predicted.Social <- test$predicted
# 
# graphtest$predicted.Social = factor(graphtest$predicted.Social,levels(graphtest$predicted.Social)[c(2,1)])
# levels(graphtest$predicted.Social) <- c("Small group","Whole class")
# levels(graphtest$predicted.Activity) <- c("Task distribution","Explanation","Questioning","Monitoring","Repairs")
# 
# 
# 
# 
# g3 <- ggplot(graphtest,aes(x=time/60000,y=predicted.Social)) + 
#     geom_segment(aes(colour = predicted.Activity, xend=(time+(5000))/60000, yend=predicted.Social), size=20, alpha=0.5) + 
#     scale_colour_brewer(palette="Set1") + theme_bw() + theme(legend.position="top") + theme(legend.text = element_text(size = 14)) + theme(plot.title = element_text(face="bold", size=18)) +
#     xlab("Time (min.)") + ylab("Social plane") + ggtitle("Automatically extracted enactment") + theme(axis.text = element_text(size=14)) +
#     guides(color = guide_legend(title = "Teacher activity", title.theme = element_text(size=15,angle=0,face="bold")))
# 
# 
# #print(g2)
# png(filename = "threeorchs.png",width = 1200,height = 800)
# multiplot(g1,g2,g3,cols=1)
# dev.off()




#try 1s windows, and also adding the 10s average as additional features
data <- loadCleanData1s()

rankAct <- getCohenRankingActivity(data)
rankSoc <- getCohenRankingSocial(data)
metricsall <- getWinningModels(data)
