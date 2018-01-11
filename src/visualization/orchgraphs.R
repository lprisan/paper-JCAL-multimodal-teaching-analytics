library(dplyr)
library(magrittr)
library(randomForest)
library(caret)
library(e1071)
library(AUC)
library(MLmetrics)
library(FactoMineR)
library(missMDA)
library(grid)

# Bring RF and RF-L9 models to local, and complete dataset!
setwd("C:\\Users\\lprisan\\Downloads\\JCAL-models")

sessions <- c("case1-day1-session1-teacher1","case1-day1-session2-teacher1",
              "case1-day1-session3-teacher1","case1-day1-session4-teacher1",
              "case2-day1-session1-teacher2","case2-day1-session2-teacher2",
              "case2-day2-session1-teacher2","case2-day2-session2-teacher2",
              "case2-day3-session1-teacher2","case2-day3-session2-teacher2",
              "case2-day4-session1-teacher2","case2-day4-session2-teacher2")
sessionst1 <- c("case1-day1-session1-teacher1","case1-day1-session2-teacher1",
              "case1-day1-session3-teacher1","case1-day1-session4-teacher1")
sessionst2 <- c("case2-day1-session1-teacher2","case2-day1-session2-teacher2",
              "case2-day2-session1-teacher2","case2-day2-session2-teacher2",
              "case2-day3-session1-teacher2","case2-day3-session2-teacher2",
              "case2-day4-session1-teacher2","case2-day4-session2-teacher2")


selectFeatures <- function(datasourcestring){
  
  features <- c(1:2) # These are only the session and timestamp
  sources <- unlist(strsplit(datasourcestring, ",", fixed = T))
  for(source in sources){
    if(source=='all'){
      features <- c(features,3:7557)
      break
    }else if(source=='et'){
      features <- c(features,3:12)
    }else if(source=='acc'){
      features <- c(features,13:152)
    }else if(source=='aud'){
      features <- c(features,153:6557)
    }else if(source=='vid'){
      features <- c(features,6558:7557)
    }else{
      stop("Wrong data sources. Possible values: all,et,acc,aud,vid")
    }
  }
  
  features <- c(features,7558:7559) # Add activity and Social
  features
}


createLookbackDataset <- function(origdata, look_back=1){
  sessions <- unique(origdata$session)
  newdata <- data.frame()
  n_feat <- (ncol(origdata)-4)
  for(sess in sessions){
    sessiondata <- origdata %>% filter(session == sess)
    
    # successively roll and append the data, then eliminate the first look_back rows
    for (i in 1:look_back){
      nc <- ncol(sessiondata)
      sessiondata[,(nc+1):(nc+n_feat)] <- NA
      sessiondata[(i+1):nrow(sessiondata),(nc+1):(nc+n_feat)] <- sessiondata[1:(nrow(sessiondata)-i),3:(2+n_feat)]
      names(sessiondata)[(nc+1):(nc+n_feat)] <- paste("LB",i,names(sessiondata)[3:(2+n_feat)],sep="")
    }
    sessiondata <- sessiondata[(look_back+1):nrow(sessiondata),]
    
    if(nrow(newdata)==0) newdata <- sessiondata
    else newdata <- rbind(newdata,sessiondata)
  }
  newdata
}




# Loading and cleaning the data
datafile <- paste(".",'completeDataset.csv',sep=.Platform$file.sep)
fulldata <- read.csv(datafile)
fulldata$Activity.clean <- ifelse(is.na(as.character(fulldata$Activity.win)) | 
                                    as.character(fulldata$Activity.win)=='OFF' |
                                    as.character(fulldata$Activity.win)=='TDT' |
                                    as.character(fulldata$Activity.win)=='TEC',
                                  'Other',as.character(fulldata$Activity.win))
fulldata$Social.clean <- ifelse(is.na(as.character(fulldata$Social.win)),
                                'Other',as.character(fulldata$Social.win))
names(fulldata)[7562:7563] <- c('Activity','Social')
fulldata <- fulldata[,-c(1,4,5,6)]
fulldata$Activity <- factor(fulldata$Activity)
fulldata$Social <- factor(fulldata$Social)




# Best models LOSO - PM: plot all 12 test sessions
# Activity: RF_all 0.715 (missing the accaud one)
# Social: RF-LB9_all 0.824
teachers <- c("teacher1","teacher2")
for(j in 1:length(teachers)){
  teacher <- teachers[j]
  sess <- character()
  if(teacher=="teacher1") sess <- sessionst1
  else sess <- sessionst2
  for(i in 1:length(sess)){
    testsession <- sess[i]
    trainsessions <- sess[sess!=testsession]
    
    # Load the Activity model (RF, accaud, LOSO, i)
    load(paste("./RF_all_PM_LOSO_Activity_t",j,"s",i,".Rdata",sep=""))
    datasourcestringAct <- "all"
    featuresAct <- selectFeatures(datasourcestringAct)
    dataAct <- fulldata[,featuresAct]
    testAct <- dataAct %>% filter(session %in% testsession)
    trainAct <- dataAct %>% filter(session %in% trainsessions)
    # We create the test dataframe to put the real and predicted orch graphs data
    finaldata <- testAct[,c("session","timestamp","Activity","Social")]
    print(dim(finaldata))
    # Predict Activity with the model, and append to the test set
    predAct <- predict(fit, testAct)
    finaldata$Activity.Pred <- predAct
    
    # Preparing data to predict Social (PCA, LB9)
    load(paste("./RF-LB9_all_PM_LOSO_Social_t",j,"s",i,".Rdata",sep=""))
    print("Transforming data...")
    k=100
    # Do PCA of train dataset, then apply it to the test dataset too
    inpca <- trainAct %>% select(-session, -timestamp, -Activity, -Social)
    # Clean infinite values (they break the  NA imputting)
    is.na(inpca) <- do.call(cbind,lapply(inpca, is.infinite))
    modPCAtrain <- list()
    outpca <- data.frame()
    if(ncol(inpca)>k){
      #imputed <- imputePCA(inpca,ncp=k)
      #modPCAtrain<-PCA(imputed$completeObs, scale.unit=T, ncp=k, graph=F)
      modPCAtrain<-PCA(inpca, scale.unit=T, ncp=k, graph=F)
      outpca<-data.frame(modPCAtrain$ind$coord[,1:k])
    }else{
      k <- ncol(inpca)
      #imputed <- imputePCA(inpca,ncp=k)
      #modPCAtrain<-PCA(imputed$completeObs, scale.unit=T, ncp=k, graph=F)
      modPCAtrain<-PCA(inpca, scale.unit=T, ncp=k, graph=F)
      outpca<-data.frame(modPCAtrain$ind$coord[,1:k])
    }
    names(outpca) <- paste("PCA",as.character(1:k),sep="_") 
    trainSoc <- cbind(trainAct[,c('session','timestamp')],outpca,trainAct[,c('Activity','Social')])
    intest <- testAct %>% select(-session, -timestamp, -Activity, -Social)
    pcaTest <- data.frame((predict(modPCAtrain, newdata=intest))$coord[,1:k])
    names(pcaTest) <- paste("PCA",as.character(1:k),sep="_") 
    testSoc <- cbind(testAct[,c('session','timestamp')],pcaTest,testAct[,c('Activity','Social')])
    lookbk <- 9
    testSoc <- createLookbackDataset(testSoc, lookbk)
    predSoc = predict(fit, testSoc)
    finaldata[(lookbk+1):nrow(finaldata),"Social.Pred"] <- predSoc
    
    #Assume predicted NAs are Others
    finaldata[is.na(finaldata$Activity.Pred),"Activity.Pred"] <- "Other"
    finaldata[is.na(finaldata$Social.Pred),"Social.Pred"] <- "Other"

    #TODO: Reorder the factors so that individual is the lowest one, maybe change the colors so that Other is grey??    
    
    plot1 <- finaldata %>%
      select(timestamp, Social, Activity) %>%
      ggplot() +
      geom_segment(aes(x=timestamp/60000, y=Social, colour = Activity, xend=(timestamp+(5000))/60000, yend=Social), size=20, alpha=0.5) + 
      xlab("Time (min)") + ylab("Social interaction") + ggtitle("Personalized - LOSO - Actual graph") + #xlim(0, 50) +
      scale_colour_brewer(palette="Set1", name="Teacher activity") + theme_bw() + 
      theme(legend.position="top", 
            axis.text.x = element_text(size=14), 
            axis.text.y = element_text(size=14), 
            legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 24, face = "bold"))
    
    plot2 <- finaldata %>%
      select(timestamp, Social.Pred, Activity.Pred) %>%
      ggplot() +
      geom_segment(aes(x=timestamp/60000, y=Social.Pred, colour = Activity.Pred, xend=(timestamp+(5000))/60000, yend=Social.Pred), size=20, alpha=0.5) + 
      xlab("Time (min)") + ylab("Social interaction") + ggtitle("Personalized - LOSO - Automatically-extracted graph") + #xlim(0, 50) +
      scale_colour_brewer(palette="Set1", name="Teacher activity") + theme_bw() + 
      theme(legend.position="top", 
            axis.text.x = element_text(size=14), 
            axis.text.y = element_text(size=14), 
            legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 24, face = "bold"))
    
    png(file = paste("LOSO_t",j,"s",i,".png", sep=""), width = 1500, height = 750)
    grid.newpage()
    grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "first"))
    #print(gplot)
    dev.off()
  }

}



# Best models LOTO - GM: plot test teacher 1 -- for each of the 4 sessions separately
# Activity: RF_audvid_GM_LOTO_Activity_1 0.567/0.384
# Social: RF_all_GM_LOTO_Social_1 0.674/0.503

testsession <- sessionst2
trainsessions <- sessionst1

#Load training and testing, and predict once
# Load the Activity model (RF, accaud, LOSO, i)
#load(paste("./RF_audvid_GM_LOTO_Activity_1.Rdata",sep=""))
#datasourcestringAct <- "aud,vid"
load(paste("./RF_all_GM_LOTO_Activity_2.Rdata",sep=""))
datasourcestringAct <- "all"
featuresAct <- selectFeatures(datasourcestringAct)
dataAct <- fulldata[,featuresAct]
testAct <- dataAct %>% filter(session %in% testsession)
trainAct <- dataAct %>% filter(session %in% trainsessions)
# We create the test dataframe to put the real and predicted orch graphs data
finaldata <- testAct[,c("session","timestamp","Activity","Social")]
print(dim(finaldata))
# Predict Activity with the model, and append to the test set
predAct <- predict(fit, testAct)
finaldata$Activity.Pred <- predAct

#Loading the social model
load(paste("./RF_all_GM_LOTO_Social_2.Rdata",sep=""))
datasourcestringSoc <- "all"
featuresSoc <- selectFeatures(datasourcestringSoc)
dataSoc <- fulldata[,featuresSoc]
testSoc <- dataSoc %>% filter(session %in% testsession)
trainSoc <- dataSoc %>% filter(session %in% trainsessions)
# Predict Activity with the model, and append to the test set
predSoc <- predict(fit, testSoc)
finaldata$Social.Pred <- predSoc

#Assume predicted NAs are Others
finaldata[is.na(finaldata$Activity.Pred),"Activity.Pred"] <- "Other"
finaldata[is.na(finaldata$Social.Pred),"Social.Pred"] <- "Other"

#Divide in the four sessions, and draw graph for each
for (s in testsession){
  
  #TODO: Reorder the factors so that individual is the lowest one, maybe change the colors so that Other is grey??    
  
  finaldata2 <- finaldata[finaldata$session==s,]
  
  plot1 <- finaldata2 %>%
    select(timestamp, Social, Activity) %>%
    ggplot() +
    geom_segment(aes(x=timestamp/60000, y=Social, colour = Activity, xend=(timestamp+(5000))/60000, yend=Social), size=20, alpha=0.5) + 
    xlab("Time (min)") + ylab("Social interaction") + ggtitle("General - LOTO - Actual graph") + #xlim(0, 50) +
    scale_colour_brewer(palette="Set1", name="Teacher activity") + theme_bw() + 
    theme(legend.position="top", 
          axis.text.x = element_text(size=14), 
          axis.text.y = element_text(size=14), 
          legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.title = element_text(size = 14),
          plot.title = element_text(size = 24, face = "bold"))
  
  plot2 <- finaldata2 %>%
    select(timestamp, Social.Pred, Activity.Pred) %>%
    ggplot() +
    geom_segment(aes(x=timestamp/60000, y=Social.Pred, colour = Activity.Pred, xend=(timestamp+(5000))/60000, yend=Social.Pred), size=20, alpha=0.5) + 
    xlab("Time (min)") + ylab("Social interaction") + ggtitle("General - LOTO - Automatically-extracted graph") + #xlim(0, 50) +
    scale_colour_brewer(palette="Set1", name="Teacher activity") + theme_bw() + 
    theme(legend.position="top", 
          axis.text.x = element_text(size=14), 
          axis.text.y = element_text(size=14), 
          legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.title = element_text(size = 14),
          plot.title = element_text(size = 24, face = "bold"))
  
  png(file = paste("LOTO_",s,".png", sep=""), width = 1500, height = 750)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "first"))
  #print(gplot)
  dev.off()
  
  
}
