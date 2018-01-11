library(ggplot2)
require(gridExtra)
library(dplyr)
library(magrittr)
library(randomForest)
library(caret)
library(e1071)
library(AUC)
library(MLmetrics)
library(markovchain)
library(grid)
library(sp)

data.orig <- read.csv("../../data/processed/completeDataset.csv", stringsAsFactors = F)
data <- data.orig

data$Teacher <- as.factor(ifelse(grepl("teacher1", data$session, fixed = T), "T1", "T2"))
data$Activity <- ifelse(is.na(as.character(data$Activity.win)) | 
                                    as.character(data$Activity.win)=='OFF' |
                                    as.character(data$Activity.win)=='TDT' |
                                    as.character(data$Activity.win)=='TEC',
                                  'Other',as.character(data$Activity.win))
data$Social <- ifelse(is.na(as.character(data$Social.win)),
                                'Other',as.character(data$Social.win))

data.origlevels <- data

###### Figure on number/type of episodes
                      
data$Activity <- as.factor(data$Activity)
levels(data$Activity) <- c("Explanation", "Monitoring", "Other", "Questioning", "Repairs")
data$Activity <- factor(data$Activity,levels(data$Activity)[c(1,2,4,5,3)])
episodesAct <- as.data.frame(table(data$Activity, data$Teacher))
names(episodesAct) <- c("Type", "Teacher", "Episodes")
plot1 <- ggplot(episodesAct, aes(x=Type, y=Episodes, fill=Teacher))+geom_bar(stat="identity")+
  theme_minimal()+scale_fill_grey()+ggtitle("Number of episodes, Teaching activity")+
  labs(x="Type of teaching activity", y="Episodes")

data$Social <- as.factor(data$Social)
levels(data$Social) <- c("Class-wide", "Small group", "Individual", "Other")
episodesSoc <- as.data.frame(table(data$Social, data$Teacher))
names(episodesSoc) <- c("Type", "Teacher", "Episodes")
plot2 <- ggplot(episodesSoc, aes(x=Type, y=Episodes, fill=Teacher))+geom_bar(stat="identity")+
  theme_minimal()+scale_fill_grey()+ggtitle("Number of episodes, Social plane")+
  labs(x="Social plane of interaction", y="Episodes")

bitmap("Figure2-new-hires.tiff", height = 4, width = 12, units = 'in', type="tifflzw", res=600)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

######## Figure on model performances

load('./perfsdata.Rdata')
# EXecute only if the Rdata does not have the RF-MC models
# moredatafiles <- list.files(".", "RF_MC_*")
# for(f in moredatafiles){
#   load(f)
#   d<-data.frame(label=label, auc=auc, kappa=cm$overall['Kappa'], 
#                 acc=cm$overall['Accuracy'], f1=f1, lang="R", model="RF_MC", 
#                 sources="all", validation="LOSO", 
#                 target=ifelse(grepl(pattern = "Activity", x = label, fixed = T), "Activity", "Social"), 
#                 modeltype=ifelse(grepl(pattern = "GM", x = label, fixed = T), "GM", "PM"),
#                 modelsources="RF_MC_all")
#   df<-rbind(df,d)
# }
# save(df, file = './perfsdata.Rdata')

df$class <- as.factor(ifelse(df$model=="RF" | df$model=="SVMBestNoCorr","Time-independent","Time-aware"))
# Personalized models
evalPM <- df[df$validation=="LOSO" & df$modeltype=="PM" & df$sources=="all",]
evalPM$class <- factor(evalPM$class, levels(evalPM$class)[2:1])
evalPM$model <- factor(evalPM$model)
evalPM$model <- factor(evalPM$model, levels(evalPM$model)[c(6,4,3,1,5,2)])
levels(evalPM$model) <- c("Markov Chain-enhanced\nRandom Forest", 
                          "Random Forest w/Look-back\nPCA per data source", 
                          "Random Forest w/Look-back\nGlobal PCA",
                          "3-layer LSTM\nGlobal PCA",
                          "SVM, 100 Best features",
                          "Random Forest, All features")

meansPM <- aggregate(f1~model+target, evalPM, mean)
mediansPM <- aggregate(f1~model+target, evalPM, median)

plot3 <- ggplot(evalPM, aes(x=model, y=f1, fill=class))+geom_boxplot()+
  theme_minimal()+scale_fill_grey(start = 0.5, end=0.8)+coord_flip()+
  geom_text(data=mediansPM, aes(x=model, label=sprintf("%0.3f", round(f1, digits = 3)), y=1), inherit.aes = F)+
  facet_wrap(~target)+ylim(0,1)+#geom_point()+
  labs(y="F1 Score", x="Model")+
  ggtitle("Personalized model performance, Leave-one-session-out evaluation")+
  theme(panel.spacing = unit(4, "lines"))

bitmap("Figure3-new-hires.tiff", height = 4, width = 12, units = 'in', type="tifflzw", res=600)
plot3
dev.off()


# Generalized models
evalGM <- df[df$validation=="LOSO" & df$modeltype=="GM" & df$sources=="all",]
evalGM <- evalGM[evalGM$model!="LSTM-100PCA-3L" & evalGM$model!="LSTM-100PCA-LP" & evalGM$model!="LSTM-2L-32hidden-50PCA",]
evalGM$class <- factor(evalGM$class, levels(evalGM$class)[2:1])
evalGM$model <- factor(evalGM$model)
evalGM$model <- factor(evalGM$model, levels(evalGM$model)[c(6,4,3,1,5,2)])
levels(evalGM$model) <- c("Markov Chain-enhanced\nRandom Forest", 
                          "Random Forest w/Look-back\nPCA per data source", 
                          "Random Forest w/Look-back\nGlobal PCA",
                          "3-layer LSTM\nGlobal PCA",
                          "SVM, 100 Best features",
                          "Random Forest, All features")


meansGM <- aggregate(f1~model+target, evalGM, mean)
mediansGM <- aggregate(f1~model+target, evalGM, median)
plot4 <- ggplot(evalGM, aes(x=model, y=f1, fill=class))+geom_boxplot()+
  theme_minimal()+scale_fill_grey(start = 0.5, end=0.8)+coord_flip()+
  geom_text(data=mediansGM, aes(x=model, label=sprintf("%0.3f", round(f1, digits = 3)), y=1), inherit.aes = F)+
  facet_wrap(~target)+ylim(0,1)+#geom_point()+
  labs(y="F1 Score", x="Model")+
  ggtitle("Generalized model performance, Leave-one-session-out evaluation")+
  theme(panel.spacing = unit(4, "lines"))

bitmap("Figure4-new-hires.tiff", height = 4, width = 12, units = 'in', type="tifflzw", res=600)
plot4
dev.off()


#### Figure of orchestration graph

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


# selectFeatures <- function(datasourcestring){
#   
#   features <- c(1:2) # These are only the session and timestamp
#   sources <- unlist(strsplit(datasourcestring, ",", fixed = T))
#   for(source in sources){
#     if(source=='all'){
#       features <- c(features,3:7557)
#       break
#     }else if(source=='et'){
#       features <- c(features,3:12)
#     }else if(source=='acc'){
#       features <- c(features,13:152)
#     }else if(source=='aud'){
#       features <- c(features,153:6557)
#     }else if(source=='vid'){
#       features <- c(features,6558:7557)
#     }else{
#       stop("Wrong data sources. Possible values: all,et,acc,aud,vid")
#     }
#   }
#   
#   features <- c(features,7558:7559) # Add activity and Social
#   features
# }
# 
# filterPredictionsMC <- function(predicted, confidence, weight, markov){
#   
#   lastPrediction <- NA
#   filtered <- factor(levels=levels(predicted))
#   for(i in 1:length(predicted)){
#     print(i)
#     print(lastPrediction)
#     if(is.na(lastPrediction)){ # If we do not have a previous prediction yet, we leave the prediction as it is
#       filtered[i] <- predicted[i]
#       lastPrediction <- predicted[i]
#     }else{ # If there is a previous prediction, we add the (weighted) markov probs to the RF probs
#       mcprobs <- markov[as.character(lastPrediction),]
#       rfprobs <- confidence[i,]
#       predprobs <- data.frame()
#       if(sum(is.na(rfprobs))>0) { # If the predictions are NA
#         for(name in names(mcprobs)){
#           predprobs[1,name] <- mcprobs[name] # We just set it to the MC probabilities, for lack of a better guess
#         }
#       }else{
#         #predprobs = data.frame(EXP=numeric(), MON=numeric(), QUE=numeric(), REP=numeric(), TDT=numeric())
#         for(name in names(mcprobs)){
#           predprobs[1,name] <- (weight*mcprobs[name])+rfprobs[name]
#         }
#       }
#       
#       prediction <- names(which.max(predprobs))
#       filtered[i] <- prediction
#       lastPrediction <- filtered[i]
#     }
#     
#   }
#   
#   filtered
# }
# 
# 
# fulldata <- data.origlevels[,-c(1,4,5,6)] %>% select(-matches("Teacher"))
# fulldata$Activity <- factor(fulldata$Activity)
# fulldata$Social <- factor(fulldata$Social)
# # Now the column indices match what is expected in the arguments parsed above
# # * [,1]: ''session id''
# # * [,2]: ''timestamp'' within the session (in ms)
# # * [,3:12]: ''eyetracking'' features (mean/sd pupil diameter, nr. of long fixations, avg. saccade speed, fixation duration, fixation dispersion, saccade duration, saccade amplitude, saccade length, saccade velocity)
# # * [,13:152]: ''accelerometer'' features, including X, Y, Z (mean, sd, max, min, median, and 30 FFT coefficients of each of them) and jerk (mean, sd, max, min, median, and 30 FFT coefficients of each of it)
# # * [,153:6557]: ''audio'' features extracted from an audio snippet of the 10s window, using openSMILE. Includes features about whether there is someone speaking (153:163), emotion recognition models (164:184), and brute-force audio spectrum features and characteristics used in various audio recognition challenges/tasks (185:6557)
# # * [,6558:7557]: ''video'' features extracted from an image taken in the middle of the window (the 1000 values of the last layer when passing the immage through a VGG pre-trained model)
# # * [,7558:7559]: ''Activity,Social'' labels we want to predict
# 
# 
# # LOSO-PM schema
# 
# teachers <- c("teacher1","teacher2")
# for(j in 1:length(teachers)){
#   teacher <- teachers[j]
#   sess <- character()
#   if(teacher=="teacher1") sess <- sessionst1
#   else sess <- sessionst2
#   for(i in 1:length(sess)){
#     testsession <- sess[i]
#     trainsessions <- sess[sess!=testsession]
#     
#     # Load the Activity model (RF, accaud, LOSO, i)
#     load(paste("./RF_MC_all_PM_LOSO_Activity_t",j,"s",i,".Rdata",sep=""))
#     datasourcestringAct <- "all"
#     featuresAct <- selectFeatures(datasourcestringAct)
#     dataAct <- fulldata[,featuresAct]
#     testAct <- dataAct %>% filter(session %in% testsession)
#     trainAct <- dataAct %>% filter(session %in% trainsessions)
#     # We create the test dataframe to put the real and predicted orch graphs data
#     finaldata <- testAct[,c("session","timestamp","Activity","Social")]
#     print(dim(finaldata))
#     
#     print("Training the MC of the train set ACTIVITY")
#     mcSeqs <- list()
#     for(s in unique(trainAct[,"session"])){
#       mcSeqs <- c(mcSeqs, list(as.character(trainAct[trainAct$session==s,"Activity"])))
#     }
#     mc <- markovchainFit(data = mcSeqs)
#     tm <- mc$estimate
#     
#     weight <- 0.25
#     predAct <- NA
#     # Split the test set by session, and apply the MC filter per session before joining the final predictions
#     for(s in unique(testAct$session)){
#       sessiontest <- testAct %>% filter(session %in% s)
#       
#       sessiontest$predicted <- predict(fit, sessiontest)
#       predprob <- predict(fit, sessiontest, "prob")
#       sessiontest$predicted2 <- filterPredictionsMC(sessiontest$predicted,predprob,weight,tm)
#       
#       if(is.na(predAct)) predAct <- sessiontest$predicted2
#       else predAct <- c(predAct, sessiontest$predicted2)
#     }
#     
#     # Predict Activity with the model, and append to the test set
#     finaldata$Activity.Pred <- predAct
#   
#     
#     # Load the Activity model (RF, accaud, LOSO, i)
#     load(paste("./RF_MC_all_PM_LOSO_Social_t",j,"s",i,".Rdata",sep=""))
#     datasourcestringSoc <- "all"
#     featuresSoc <- selectFeatures(datasourcestringSoc)
#     dataSoc <- fulldata[,featuresSoc]
#     testSoc <- dataSoc %>% filter(session %in% testsession)
#     trainSoc <- dataSoc %>% filter(session %in% trainsessions)
# 
#     print("Training the MC of the train set SOCIAL")
#     mcSeqs <- list()
#     for(s in unique(trainSoc[,"session"])){
#       mcSeqs <- c(mcSeqs, list(as.character(trainSoc[trainSoc$session==s,"Social"])))
#     }
#     mc <- markovchainFit(data = mcSeqs)
#     tm <- mc$estimate
#     
#     weight <- 0.25
#     predSoc <- NA
#     # Split the test set by session, and apply the MC filter per session before joining the final predictions
#     for(s in unique(testSoc$session)){
#       sessiontest <- testSoc %>% filter(session %in% s)
#       
#       sessiontest$predicted <- predict(fit, sessiontest)
#       predprob <- predict(fit, sessiontest, "prob")
#       sessiontest$predicted2 <- filterPredictionsMC(sessiontest$predicted,predprob,weight,tm)
#       
#       if(is.na(predSoc)) predSoc <- sessiontest$predicted2
#       else predSoc <- c(predSoc, sessiontest$predicted2)
#     }
#     
#     # Predict Social with the model, and append to the test set
#     finaldata$Social.Pred <- predSoc
#     
#     #Assume predicted NAs are Others
#     finaldata[is.na(finaldata$Activity.Pred),"Activity.Pred"] <- "Other"
#     finaldata[is.na(finaldata$Social.Pred),"Social.Pred"] <- "Other"
#     
#     #TODO: Reorder the factors so that individual is the lowest one, maybe change the colors so that Other is grey??    
#     
#     plot1 <- finaldata %>%
#       select(timestamp, Social, Activity) %>%
#       ggplot() +
#       geom_segment(aes(x=timestamp/60000, y=Social, colour = Activity, xend=(timestamp+(5000))/60000, yend=Social), size=20, alpha=0.5) + 
#       xlab("Time (min)") + ylab("Social interaction") + ggtitle("Actual orchestration graph, coded by a human") + #xlim(0, 50) +
#       scale_colour_brewer(palette="Set1", name="Teacher activity") + theme_bw() + 
#       theme(legend.position="top", 
#             axis.text.x = element_text(size=14), 
#             axis.text.y = element_text(size=14), 
#             legend.text = element_text(size = 12),
#             axis.title.x = element_text(size = 18),
#             axis.title.y = element_text(size = 18),
#             legend.title = element_text(size = 14),
#             plot.title = element_text(size = 24, face = "bold"))
#     
#     plot2 <- finaldata %>%
#       select(timestamp, Social.Pred, Activity.Pred) %>%
#       ggplot() +
#       geom_segment(aes(x=timestamp/60000, y=Social.Pred, colour = Activity.Pred, xend=(timestamp+(5000))/60000, yend=Social.Pred), size=20, alpha=0.5) + 
#       xlab("Time (min)") + ylab("Social interaction") + ggtitle("Markov Chain-enhanced Random Forest - Personalized model - Automatically-extracted graph") + #xlim(0, 50) +
#       scale_colour_brewer(palette="Set1", name="Teacher activity") + theme_bw() + 
#       theme(legend.position="top", 
#             axis.text.x = element_text(size=14), 
#             axis.text.y = element_text(size=14), 
#             legend.text = element_text(size = 12),
#             axis.title.x = element_text(size = 18),
#             axis.title.y = element_text(size = 18),
#             legend.title = element_text(size = 14),
#             plot.title = element_text(size = 24, face = "bold"))
#     
#     png(file = paste("LOSO_t",j,"s",i,".png", sep=""), width = 1500, height = 750)
#     grid.newpage()
#     grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "first"))
#     #print(gplot)
#     dev.off()
#     
#     save(finaldata, file = paste("finaldata.t",j,"s",i,".Rdata", sep=""))
#   } # closing for sessions
#   
# } # closing for teachere
# 




teachers <- c("teacher1","teacher2")
for(j in 1:length(teachers)){
  teacher <- teachers[j]
  sess <- character()
  if(teacher=="teacher1") sess <- sessionst1
  else sess <- sessionst2
  for(i in 1:length(sess)){

    finaldata <- get(load(paste("./finaldata.t",j,"s",i,".Rdata",sep="")))

    finaldata$Social <- factor(finaldata$Social, levels=c("IND", "GRP", "CLS", "Other"))
    finaldata$Social.Pred <- factor(finaldata$Social.Pred, levels=c("IND", "GRP", "CLS", "Other"))
    
    finaldata$Activity <- factor(finaldata$Activity, levels=c("EXP", "MON", "QUE", "REP", "Other"))
    finaldata$Activity.Pred <- factor(finaldata$Activity.Pred, levels=c("EXP", "MON", "QUE", "REP", "Other"))
    
    plot1 <- finaldata %>%
      select(timestamp, Social, Activity) %>%
      ggplot() +
      geom_segment(aes(x=timestamp/60000, y=Social, colour = Activity, xend=(timestamp+(5000))/60000, yend=Social), size=20) + 
      xlab("Time (min)") + ylab("Social interaction") + ggtitle("Actual orchestration graph, coded by a human") + #xlim(0, 50) +
      #scale_colour_brewer(palette="Set1", name="Teacher activity") + 
      scale_color_grey(start = 0, end = .9, name="Teacher activity")+
      #scale_color_manual(values = bpy.colors()[c(1,24,46,68,90)], drop=F) +
      #theme_bw() + 
      theme_minimal() +
      scale_y_discrete(drop=FALSE) +
      theme(legend.position="top", 
            axis.text.x = element_text(size=14), 
            axis.text.y = element_text(size=14), 
            legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 24, face = "bold"),
            plot.margin = unit(c(.5,.5,.5,.5), "cm"))
    
    plot2 <- finaldata %>%
      select(timestamp, Social.Pred, Activity.Pred) %>%
      ggplot() +
      geom_segment(aes(x=timestamp/60000, y=Social.Pred, colour = Activity.Pred, xend=(timestamp+(5000))/60000, yend=Social.Pred), size=20) + 
      xlab("Time (min)") + ylab("Social interaction") + ggtitle("Markov Chain-enhanced Random Forest - Personalized model - Automatically-extracted graph") + #xlim(0, 50) +
      #scale_colour_brewer(palette="Set1", name="Teacher activity") + 
      scale_color_grey(start = 0, end = .9, name="Teacher activity")+
      #scale_color_manual(values = bpy.colors()[c(1,24,46,68,90)], drop=F) +
      #theme_bw() + 
      theme_minimal() +
      scale_y_discrete(drop=FALSE) +
      theme(legend.position="top", 
            axis.text.x = element_text(size=14), 
            axis.text.y = element_text(size=14), 
            legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 24, face = "bold"),
            plot.margin = unit(c(.5,.5,.5,.5), "cm"))
    
    #png(file = paste("Figure5-new-hires.t",j,"s",i,".png", sep=""), width = 1500, height = 750)
    bitmap(paste("Figure5-new-hires.t",j,"s",i,".tiff", sep=""), height = 10, width = 18, units = 'in', type="tifflzw", res=600)
    
    #grid.newpage()
    #grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "first"))
    grid.arrange(plot1, plot2, ncol=1)
    dev.off()
    
    
  } # closing for sessions
  
} # closing for teachere


