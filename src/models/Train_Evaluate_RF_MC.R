#!/usr/bin/env Rscript

# Usage: Train_Evaluate_RF.R <label> <target-variable> <data-sources> <train-set-sessions> <test-set-sessions>
# NB: Normally, use quotes around each parameter, as they are strings with commas and stuff

# The script assumes that the dataset is in the same dir as the script, and writes its output to a file <label>.RData
# The output contains the trained model (model), args = commandArgs(trailingOnly=TRUE)
library(dplyr)
library(magrittr)
library(randomForest)
library(caret)
library(e1071)
library(AUC)
library(MLmetrics)
library(markovchain)

args <- commandArgs(trailingOnly=FALSE)
# Get the --file argument to get the current script location
arg <- (args[grepl("--file=",args,fixed=T)])[1]
fullpath <- unlist(strsplit(arg,"--file=", fixed=T))[2]
path <- dirname(normalizePath(fullpath))

args <- commandArgs(trailingOnly=TRUE)
print(args)
if(length(args)!=5){
  stop("Wrong number of arguments. Usage:\nTrain_Evaluate_RF.R <label> <target-variable> <data-sources> <train-set-sessions> <test-set-sessions>")
}

label <- args[1]
target <- args[2] # either 'Activity' or 'Social'
datasourcestring <- args[3] # Comma separated combinations of all,et,acc,aud,vid
trainstring <- args[4]
teststring <- args[5]

# We parse the data sources, and sessions for the train and test sets
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
print(paste("Selected features: ",length(features)))

sessiontrain <- unlist(strsplit(trainstring, ",", fixed = T)) # Gives an array of the sessions to train in
sessiontest <- unlist(strsplit(teststring, ",", fixed = T)) # Gives an array of the sessions to train in

if(length(sessiontrain)==0 | length(sessiontest)==0){
  stop("Wrong train/test sessions specification. Should be a comma-separated string with the sessions identificators")
}


# READING AND PREPARING THE DATA
processeddatadir <- path
datafile <- paste(processeddatadir,'completeDataset.csv',sep=.Platform$file.sep)
gzdatafile <- paste(processeddatadir,'completeDataset.csv.gz',sep=.Platform$file.sep)
fulldata <- data.frame()
if(file.exists(datafile)){
  fulldata <- read.csv(datafile)
}else if(file.exists(gzdatafile)){
  fulldata <- read.csv(gzfile(datafile))  
}else{
  stop("Data not available in the processed data folder")
}
# We only look for predicting 4 states of activity and 3 of social, the rest (incl.NA) we bunch in 'Other' (so in the end it is a 5- and 4-class classification problem)
fulldata$Activity.clean <- ifelse(is.na(as.character(fulldata$Activity.win)) | 
                                    as.character(fulldata$Activity.win)=='OFF' |
                                    as.character(fulldata$Activity.win)=='TDT' |
                                    as.character(fulldata$Activity.win)=='TEC',
                                  'Other',as.character(fulldata$Activity.win))
fulldata$Social.clean <- ifelse(is.na(as.character(fulldata$Social.win)),
                                'Other',as.character(fulldata$Social.win))

#table(fulldata$session, fulldata$Activity.clean, useNA = "always")
#table(fulldata$session, fulldata$Social.clean, useNA = "always")

names(fulldata)[7562:7563] <- c('Activity','Social')
fulldata <- fulldata[,-c(1,4,5,6)]
fulldata$Activity <- factor(fulldata$Activity)
fulldata$Social <- factor(fulldata$Social)
# Now the column indices match what is expected in the arguments parsed above
# * [,1]: ''session id''
# * [,2]: ''timestamp'' within the session (in ms)
# * [,3:12]: ''eyetracking'' features (mean/sd pupil diameter, nr. of long fixations, avg. saccade speed, fixation duration, fixation dispersion, saccade duration, saccade amplitude, saccade length, saccade velocity)
# * [,13:152]: ''accelerometer'' features, including X, Y, Z (mean, sd, max, min, median, and 30 FFT coefficients of each of them) and jerk (mean, sd, max, min, median, and 30 FFT coefficients of each of it)
# * [,153:6557]: ''audio'' features extracted from an audio snippet of the 10s window, using openSMILE. Includes features about whether there is someone speaking (153:163), emotion recognition models (164:184), and brute-force audio spectrum features and characteristics used in various audio recognition challenges/tasks (185:6557)
# * [,6558:7557]: ''video'' features extracted from an image taken in the middle of the window (the 1000 values of the last layer when passing the immage through a VGG pre-trained model)
# * [,7558:7559]: ''Activity,Social'' labels we want to predict
#names(fulldata)


# SELECTING THE DATASET FEATURES (DATA SOURCES BEING TRIED)
data <- fulldata[,features]
print(dim(data))


# SPLITTING THE DATA
test <- data %>% filter(session %in% sessiontest)
train <- data %>% filter(session %in% sessiontrain)



#######################################################
# DO OTHER DATA TRANSFORMATIONS NEEDED, e.g. PCA, SELECT K-BEST FEATURES, etc (NORMALLY, ON THE TRAIN SET ONLY, TO BE APPLIED LATER TO THE TEST SET)
print("Transforming data...")




#######################################################




# Setup the seeds and training
set.seed(123)
input <- data.frame()
if(target=='Activity'){
  input <- train %>% select(-session, -timestamp, -Social) %>% filter(complete.cases(.)) # For sequence-dependent models the complete.cases may not be needed/advisable  
}else if(target=='Social'){
  input <- train %>% select(-session, -timestamp, -Activity) %>% filter(complete.cases(.)) # For sequence-dependent models the complete.cases may not be needed/advisable  
}else{
  stop("Wrong target variable. Should be either Activity or Social")
}



######################################################
# TRAIN THE MODEL -- SUBSTITUTE BY OTHERS AS NEEDED

print("Training the MC of the train set")

mcSeqs <- list()
for(s in unique(train[,"session"])){
  mcSeqs <- c(mcSeqs, list(as.character(train[train$session==s,target])))
}
mc <- markovchainFit(data = mcSeqs)
tm <- mc$estimate
show(tm)

  
  

print("Training the model...")
fit <- list()
if(target=='Activity'){
  fit <- randomForest(Activity ~ .,
                       data=input,
                       importance=TRUE)
}else if(target=='Social'){
  fit <- randomForest(Social ~ .,
                      data=input,
                      importance=TRUE)
}

#####################################################



# EVALUATE THE MODEL, AND STORE IT AND THE TEST PERFORMANCE
print("Evaluating the model...")


# Filters a factor vector, changing value only when confidence is over the threshold
filterPredictionsMC <- function(predicted, confidence, weight, markov){
  
  lastPrediction <- NA
  filtered <- factor(levels=levels(predicted))
  for(i in 1:length(predicted)){
    #print(i)
    if(is.na(lastPrediction)){ # If we do not have a previous prediction yet, we leave the prediction as it is
      filtered[i] <- predicted[i]
      lastPrediction <- predicted[i]
    }else{ # If there is a previous prediction, we add the (weighted) markov probs to the RF probs
      mcprobs <- markov[as.character(lastPrediction),]
      rfprobs <- confidence[i,]
      predprobs <- data.frame()
      if(sum(is.na(rfprobs))>0) { # If the predictions are NA
        for(name in names(mcprobs)){
          predprobs[1,name] <- mcprobs[name] # We just set it to the MC probabilities, for lack of a better guess
        }
      }else{
        #predprobs = data.frame(EXP=numeric(), MON=numeric(), QUE=numeric(), REP=numeric(), TDT=numeric())
        for(name in names(mcprobs)){
          predprobs[1,name] <- (weight*mcprobs[name])+rfprobs[name]
        }
      }
      
      prediction <- names(which.max(predprobs))
      filtered[i] <- prediction
      lastPrediction <- filtered[i]
    }
    
  }
  
  filtered
}


# Tune the weight (TO BE REMOVED): 
#weights <- seq(from=0.1, to=0.4, by=0.01)
#for(weight in weights){
weight <- 0.25
    predictions <- NA
    # Split the test set by session, and apply the MC filter per session before joining the final predictions
    for(s in unique(test$session)){
      sessiontest <- test %>% filter(session %in% s)
      
      sessiontest$predicted <- predict(fit, sessiontest)
      predprob <- predict(fit, sessiontest, "prob")
      sessiontest$predicted2 <- filterPredictionsMC(sessiontest$predicted,predprob,weight,tm)
      
      if(is.na(predictions)) predictions <- sessiontest$predicted2
      else predictions <- c(predictions, sessiontest$predicted2)
    }
    
    
#    f1 <- F1_Score(y_pred = predictions, y_true = test[,target])
#    print(paste("F1 for weight",weight," - ",f1))
#} # End of weight tuning (TO BE REMOVED)

cm <- confusionMatrix(predictions, test[,target])
#print(paste("confusion matrix for weight ",weight))
print(cm)

#Plot variable importance
#varImpPlot(fit, scale=FALSE)
auc <- tryCatch(auc(roc(predictions, test[,target])), 
                 error=function(e){
                   print(e)
                   print(paste("could not calculate AUC for",label))
                   return(NA)
                 })
print(auc)
f1 <- F1_Score(y_pred = predictions, y_true = test[,target])
print(f1)

save(label, fit, cm, auc, f1, file=paste(processeddatadir,paste(label,".Rdata",sep=""), sep=.Platform$file.sep))