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
library(FactoMineR)
library(missMDA)

args <- commandArgs(trailingOnly=FALSE)
# Get the --file argument to get the current script location
arg <- (args[grepl("--file=",args,fixed=T)])[1]
fullpath <- unlist(strsplit(arg,"--file=", fixed=T))[2]
path <- dirname(normalizePath(fullpath))
#path <- "."
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
#label <- "RF-LB9_all_GM_LOSO_Activity_1"
#target <- 'Activity'
#datasourcestring <- "all"
#trainstring <- "case1-day1-session2-teacher1,case1-day1-session3-teacher1,case1-day1-session4-teacher1,case2-day1-session1-teacher2,case2-day1-session2-teacher2,case2-day2-session1-teacher2,case2-day2-session2-teacher2,case2-day3-session1-teacher2,case2-day3-session2-teacher2,case2-day4-session1-teacher2,case2-day4-session2-teacher2"
#teststring <- 'case1-day1-session1-teacher1'

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
print("Transforming data...")


# Do PCA of train dataset, then apply it to the test dataset too
inpca <- train %>% select(-session, -timestamp, -Activity, -Social)
# Clean infinite values (they break the  NA imputting)
#is.na(inpca) <- sapply(inpca, is.infinite)
is.na(inpca) <- do.call(cbind,lapply(inpca, is.infinite))

#k=100
ks <- data.frame(sources=c("et","acc","aud","vid"),featuresfrom=c(1,11,151,6556),featuresto=c(10,150,6555,7555),ncomp=c(5,10,20,10))
modPCAtrain <- list()
outpca <- data.frame()

#k.et <- estim_ncp(inpca[complete.cases(inpca[,1:10]),1:10],scale = T)
pca.et <- PCA(inpca[,1:10], scale.unit=T, ncp=ks[ks$sources=='et','ncomp'], graph=F)
pca.acc <- PCA(inpca[,11:150], scale.unit=T, ncp=ks[ks$sources=='acc','ncomp'], graph=F)
pca.aud <- PCA(inpca[,151:6555], scale.unit=T, ncp=ks[ks$sources=='aud','ncomp'], graph=F)
pca.vid <- PCA(inpca[,6556:7555], scale.unit=T, ncp=ks[ks$sources=='vid','ncomp'], graph=F)

outpca.et<-data.frame(pca.et$ind$coord[,1:ks[ks$sources=='et','ncomp']])
names(outpca.et) <- paste("PCAet",as.character(1:ks[ks$sources=='et','ncomp']),sep="_") 
outpca.acc<-data.frame(pca.acc$ind$coord[,1:ks[ks$sources=='acc','ncomp']])
names(outpca.acc) <- paste("PCAacc",as.character(1:ks[ks$sources=='acc','ncomp']),sep="_") 
outpca.aud<-data.frame(pca.aud$ind$coord[,1:ks[ks$sources=='aud','ncomp']])
names(outpca.aud) <- paste("PCAaud",as.character(1:ks[ks$sources=='aud','ncomp']),sep="_") 
outpca.vid<-data.frame(pca.vid$ind$coord[,1:ks[ks$sources=='vid','ncomp']])
names(outpca.vid) <- paste("PCAvid",as.character(1:ks[ks$sources=='vid','ncomp']),sep="_") 

k <- sum(ks$ncomp)

# if(ncol(inpca)>k){
#   #imputed <- imputePCA(inpca,ncp=k)
#   #modPCAtrain<-PCA(imputed$completeObs, scale.unit=T, ncp=k, graph=F)
#   modPCAtrain<-PCA(inpca, scale.unit=T, ncp=k, graph=F)
#   outpca<-data.frame(modPCAtrain$ind$coord[,1:k])
# }else{
#   k <- ncol(inpca)
#   #imputed <- imputePCA(inpca,ncp=k)
#   #modPCAtrain<-PCA(imputed$completeObs, scale.unit=T, ncp=k, graph=F)
#   modPCAtrain<-PCA(inpca, scale.unit=T, ncp=k, graph=F)
#   outpca<-data.frame(modPCAtrain$ind$coord[,1:k])
# }
# names(outpca) <- paste("PCA",as.character(1:k),sep="_") 

train <- cbind(train[,c('session','timestamp')],outpca.et,outpca.acc,outpca.aud,outpca.vid,train[,c('Activity','Social')])

intest <- test %>% select(-session, -timestamp, -Activity, -Social)

pcaTest.et <- data.frame((predict(pca.et, newdata=intest))$coord[,1:ks[ks$sources=='et','ncomp']])
names(pcaTest.et) <- paste("PCAet",as.character(1:ks[ks$sources=='et','ncomp']),sep="_") 
pcaTest.acc <- data.frame((predict(pca.acc, newdata=intest))$coord[,1:ks[ks$sources=='acc','ncomp']])
names(pcaTest.acc) <- paste("PCAacc",as.character(1:ks[ks$sources=='acc','ncomp']),sep="_") 
pcaTest.aud <- data.frame((predict(pca.aud, newdata=intest))$coord[,1:ks[ks$sources=='aud','ncomp']])
names(pcaTest.aud) <- paste("PCAaud",as.character(1:ks[ks$sources=='aud','ncomp']),sep="_") 
pcaTest.vid <- data.frame((predict(pca.vid, newdata=intest))$coord[,1:ks[ks$sources=='vid','ncomp']])
names(pcaTest.vid) <- paste("PCAvid",as.character(1:ks[ks$sources=='vid','ncomp']),sep="_") 

test <- cbind(test[,c('session','timestamp')],pcaTest.et,pcaTest.acc,pcaTest.aud,pcaTest.vid,test[,c('Activity','Social')])

# Create lookback dataset: for each session, put look_back previous timesteps as new dimensions, and remove the look-back first rows of the dataset
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


lookbk <- 9
train <- createLookbackDataset(train,lookbk)
test <- createLookbackDataset(test, lookbk)


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
predictions = predict(fit, test)
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
