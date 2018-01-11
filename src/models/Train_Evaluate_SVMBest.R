#!/usr/bin/env Rscript

# Usage: Train_Evaluate_SVMBest.R <label> <target-variable> <data-sources> <train-set-sessions> <test-set-sessions>
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

args <- commandArgs(trailingOnly=FALSE)
# Get the --file argument to get the current script location
arg <- (args[grepl("--file=",args,fixed=T)])[1]
fullpath <- unlist(strsplit(arg,"--file=", fixed=T))[2]
path <- dirname(normalizePath(fullpath))

args <- commandArgs(trailingOnly=TRUE)
print(args)
if(length(args)!=5){
  stop("Wrong number of arguments. Usage:\nTrain_Evaluate_SVMBest.R <label> <target-variable> <data-sources> <train-set-sessions> <test-set-sessions>")
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
  stop("Data not available in the script's folder")
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
k <- 100
print(k)
print("Transforming data (Selecting k best features) ...")

# Returns a character vector of the 100 best features for the target variable, along with session, timestamp and the two original target variables
selectKBest <- function(data, target, k){
  cols <- ncol(data)
  print(paste("selecting",k,"features out of",cols))
  feats <- 3:(ncol(data)-2)
  print(length(feats))
  
  df <- data.frame(featnames=character(length(feats)), scores=numeric(length(feats)), stringsAsFactors = F)
  rankedfeatures <- character()
  counter <- 1
  for(i in feats){
    name <- names(data)[i]
    # Find out Cohen's d of each feature through ANOVA?
    m<-tryCatch(anova(lm(data[,name]~data[,target],na.action=na.omit)), 
             error=function(e){
               print(e)
               print(paste("could not calculate cohen d for",name))
               return(NA)
             })
    # print(m)
    df[counter,'featnames'] <- as.character(name)
    if(!is.na(m)){
      if(m$Sum[2]>0){
        df[counter,'scores'] <- m$Sum[1]/m$Sum[2]
      }
    }
    print(paste(df[counter,'featnames'],df[counter,'scores']))
    counter <- counter+1
  }
  
  df <- df[with(df, order(-scores)), ]
  rankedfeatures <- df$featnames[1:100]
  
  newfeatures <- c(names(data)[1:2],rankedfeatures,names(data)[(cols-1):cols])  
  newfeatures
}



# We obtain the list of k features with largest F scores (or cohen's d, or whatever)
newfeatures <- names(train)
if(length(features)>(k+4)){
  newfeatures <- selectKBest(train,target,k)
}
train <- train[,newfeatures]
test <- test[,newfeatures]

print(dim(train))
print(dim(test))


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
print("Training the model...")
fit <- list()
if(target=='Activity'){
  fit <- svm(Activity ~ ., data=input)
  # fit <- tune.svm(Activity ~ ., data=input, 
                      # gamma = 2^(-1:1), cost = 2^(2:4), 
                      # tunecontrol = tune.control(sampling = "boot", nboot=25))
}else if(target=='Social'){
  fit <- svm(Social ~ ., data=input)
  # fit <- tune.svm(Social ~ ., data=input, 
              # gamma = 2^(-1:1), cost = 2^(2:4), 
              # tunecontrol = tune.control(sampling = "boot", nboot=25))
}

#####################################################



# EVALUATE THE MODEL, AND STORE IT AND THE TEST PERFORMANCE
print("Evaluating the model...")
predictions = predict(fit, test, na.action = na.exclude )  # na.exclude to avoid different-length output
cm <- confusionMatrix(predictions, test[,target])
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