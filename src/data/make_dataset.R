# Munges the raw data files into a set of clean(er) data (CSV) files,
# aligned but with potentially different sampling rates
#
# Assumptions:
# 0. The working directory is that where this R file lives
# 1. The original data files have been copied into the data/raw/ folder,
#    with names like (case1-day1-session2-teacher1)-kindofdata.extension
#    (case1-day1-session2-teacher1) = sessionid
# 2. Will put the cleaner data files in the data/interim folder
source('readAnnotationsFile.R')
source('readTrackerFile.R')
source('getTargetValues.R')

originalwd <- getwd()
rawdatadir <- paste(originalwd,"../../data/raw",sep = .Platform$file.sep)
interimdatadir <- paste(originalwd,"../../data/interim",sep = .Platform$file.sep)
processeddatadir <- paste(originalwd,"../../data/processed",sep = .Platform$file.sep)
#setwd(rawdatadir)

# Add more sessions here as we add case studies with different teachers
sessions <- data.frame(session=c("case1-day1-session1-teacher1",
                              "case1-day1-session2-teacher1",
                              "case1-day1-session3-teacher1",
                              "case1-day1-session4-teacher1",
                              "case2-day1-session1-teacher2",
                              "case2-day1-session2-teacher2",
                              "case2-day2-session1-teacher2",
                              "case2-day2-session2-teacher2",
                              "case2-day3-session1-teacher2",
                              "case2-day3-session2-teacher2",
                              "case2-day4-session1-teacher2",
                              "case2-day4-session2-teacher2"),
                         case=c("case1",
                                "case1",
                                "case1",
                                "case1",
                                "case2",
                                "case2",
                                "case2",
                                "case2",
                                "case2",
                                "case2",
                                "case2",
                                "case2"),
                         day=c("day1",
                               "day1",
                               "day1",
                               "day1",
                               "day1",
                               "day1",
                               "day2",
                               "day2",
                               "day3",
                               "day3",
                               "day4",
                               "day4"),
                         teacher=c("teacher1",
                                   "teacher1",
                                   "teacher1",
                                   "teacher1",
                                   "teacher2",
                                   "teacher2",
                                   "teacher2",
                                   "teacher2",
                                   "teacher2",
                                   "teacher2",
                                   "teacher2",
                                   "teacher2"))

# (possibly partial) suffixes for the different raw data files
suffix.acc <- "accelerometer"
suffix.audio <- "audio"
suffix.et.raw <- "eyetracker-raw"
suffix.et.fix <- "eyetracker-fixations"
suffix.et.sac <- "eyetracker-saccades"
suffix.video <- "video"
suffix.coding <- "videocoding"

# STEP 1: ALIGNMENT OF THE DIFFERENT DATA SOURCES
# We take from the video coding the moment all sources are recording
# (teacher clicks record on mobile phone tracker)
# This origin of time will be applied to all subsequent processing of raw data,
# so as to have aligned data, even for different sampling rates
annotationsData <- data.frame()
for (i in 1:nrow(sessions)){
    session.annot <- readAnnotationsFile(paste(rawdatadir,.Platform$file.sep,sessions[i,'session'],"-",suffix.coding,".eaf",sep=""))
    session.annot$session <- sessions[i,'session'] # We add the session ID to the annotations
    # Join all sessions in a single dataframe
    if(nrow(annotationsData)==0) annotationsData = session.annot
    else annotationsData = rbind(annotationsData,session.annot)
}
# We add the start and end timestamps (from the point of view of the ET recording) of our dataset for each session.
# These will be the origin of our unified/aligned dataset
initendtimes <- annotationsData[annotationsData$tier=="Recording" & annotationsData$annotation=="Recording",c("start","end","session")]
sessions <- merge(sessions,initendtimes)

# We load all our raw data, clean it up, and add the new aligned timestamp, and the two target variables at that point in time, and write it to the interim data folder
# Accelerometer data
accelData <- data.frame()
for (i in 1:nrow(sessions)){

    session.accel <- readTrackerFile(paste(rawdatadir,.Platform$file.sep,sessions[i,'session'],"-",suffix.acc,"-1.txt",sep=""))
    session.accel <- rbind(session.accel, readTrackerFile(paste(rawdatadir,.Platform$file.sep,sessions[i,'session'],"-",suffix.acc,"-2.txt",sep="")))
    session.accel$session <- sessions[i,'session'] # We add the session ID to the annotations
    # We clean up to keep only the accelerometer data
    session.accel <- session.accel[!is.na(session.accel$accelerationX),-c(5:8)]
    session.accel <- session.accel[order(session.accel$timestamp),]
    # We set the aligned timestamp (the initial timestamp for the accel file marks the origin, nothing to do with the eyetracker timestamp)
    session.accel$timestamp.orig <- session.accel$timestamp
    session.accel$timestamp <- session.accel$timestamp - session.accel$timestamp[1]

    # We add the two target variables, in case we want to use the interim data file for training directly
    v <- sapply(session.accel$timestamp, getActivityForTimestamp, annotationsData, sessions[i,'session'])
    v2 <- lapply(v, function(x) ifelse(length(x)==0, NA, x))
    session.accel$Activity <- unlist(v2)
    v <- sapply(session.accel$timestamp, getSocialForTimestamp, annotationsData, sessions[i,'session'])
    v2 <- lapply(v, function(x) ifelse(length(x)==0, NA, x))
    session.accel$Social <- unlist(v2)

    if(nrow(accelData)==0) accelData = session.accel
    else accelData = rbind(accelData,session.accel)
}
# We write the clean, aligned data to an interim csv
z <- gzfile(paste(interimdatadir,"accelDataRaw.csv.gz",sep=.Platform$file.sep),"w")
write.csv(accelData, z)
close(z)
#zz=gzfile(paste(interimdatadir,"accelDataRaw.csv.gz",sep=.Platform$file.sep),'rt')   
#accelData=read.csv(zz,header=T)[,2:9]


source('getWindowTimes.R')
# To extract audio/video/eyetrack features, we need a window length
window.ms <- 10000 # For now, extract in 10s windows, with 5s overlaps
slide.ms <- window.ms/2
window.times <- getWindowTimes(sessions, window.ms, slide.ms) # get the mid-window timestamps, for audio/video/eyetrack data (both original and new)
# Add the target variables for each window (predominant tags per window, tags in the exact mid-window)
v <- apply(window.times, 1, function(x) getActivityForTimestamp(x[1], annotationsData, x[3]))
v2 <- lapply(v, function(x) ifelse(length(x)==0, NA, x))
window.times$Activity.inst <- unlist(v2)
v <- apply(window.times, 1, function(x) getSocialForTimestamp(x[1], annotationsData, x[3]))
v2 <- lapply(v, function(x) ifelse(length(x)==0, NA, x))
window.times$Social.inst <- unlist(v2)
v <- apply(window.times, 1, function(x) getActivityForWindow(x[1], annotationsData, x[3], F, window.ms))
v2 <- lapply(v, function(x) ifelse(length(x)==0, NA, x))
window.times$Activity.win <- unlist(v2)
v <- apply(window.times, 1, function(x) getSocialForWindow(x[1], annotationsData, x[3], F, window.ms))
v2 <- lapply(v, function(x) ifelse(length(x)==0, NA, x))
window.times$Social.win <- unlist(v2)
z <- gzfile(paste(interimdatadir,"windowTimes.csv.gz",sep=.Platform$file.sep),"w")
write.csv(window.times, z)
close(z)
#zz=gzfile(paste(interimdatadir,"windowTimes.csv.gz",sep=.Platform$file.sep),'rt')   
#window.times=read.csv(zz,header=T)[,2:8]

# video data (extract the desired frames from it, and tag them with the timestamp and the target values?
# note: ensure that the directory does not exist before running this!!!
framesDir <- paste(interimdatadir,"videoframes",sep=.Platform$file.sep)
dir.create(framesDir)
source('extractFrameFromVideo.R')
for(i in 1:nrow(window.times)){
    sample <- window.times[i,]
    extractFrameFromVideo(sample$timestamp, sample$timestamp.orig, sample$session, rawdatadir, paste(interimdatadir,"videoframes",sep=.Platform$file.sep))
}
# note: ensure the VGG feature extraction scripts (and models) are downloaded somewhere, see https://github.com/kidzik/deep-features
# TODO: Does not work automatically! ===================================
# go to the function code and execute in a terminal the commands specified ther
source('extractVideoFeatures.R')
#videofeaturesfile <- extractVideoFeatures(outputzipFile = paste(interimdatadir,"videofeatures-lastlayer.csv.zip", sep=.Platform$file.sep),
#                                          imageDir = framesDir,
#                                          featurescriptDir = "../../../deep-features")
videofeaturesfile <- paste(interimdatadir,"videofeatures-lastlayer.csv.zip", sep=.Platform$file.sep)

# We read the resulting zipped csv file
videodata <- read.csv(unz(description = videofeaturesfile, filename = "../../../deep-features/output.csv"), stringsAsFactors = F, header = F)
#If data was split in multiple files
#videodata2 <- read.csv(unz(description = videofeaturesfile, filename = "../../../deep-features/output2.csv"), stringsAsFactors = F, header = F)
#videodata <- rbind(videodata,videodata2)
videodata$filename <- basename(videodata[,1])
videodata$timestamp <- gsub(".*\\__(.*)\\..*", "\\1", videodata$filename)
videodata$session <- sub("__.*", "", videodata$filename)
names(videodata)[[1]] <- 'full.path'
names(videodata)[2:1001] <- paste("V",1:1000,sep="")
#========================================================================



# audio data (create the snippets of the desired length, and tag them with the mid-snippet timestamp?)
interimAudioDir <- paste(interimdatadir,"audiosnippets",sep=.Platform$file.sep)
if(dir.exists(interimAudioDir)) unlink(interimAudioDir, recursive=TRUE)
dir.create(interimAudioDir)
source('extractAudioSnippets.R')
for(i in 1:nrow(window.times)){
    sample <- window.times[i,]
    extractAudioSnippet(sample$timestamp, sample$timestamp.orig, sample$session, rawdatadir, paste(interimdatadir,"audiosnippets",sep=.Platform$file.sep), window.ms)
}

# feature extraction from audio data
openSmileDir <- "/home/lprisan/workspace/openSMILE-2.1.0/" # Change to the dir where openSMILE 2.1.0 has been cloned/installed
source('extractAudioFeatures.R')
audiofeatures <- data.frame() # The instantaneous value of Activity/Social is not interesting since we analyze the snippet of the whole 10s
for(i in 1:nrow(window.times)){ # For each window/snippet
    snippet <- window.times[i,-c(4,5)]

    snippetfeatures <- extractFeaturesFromSnippet(snippet, openSmileDir, interimAudioDir)

    if(nrow(audiofeatures)==0) audiofeatures <- snippetfeatures
    else audiofeatures <- rbind(audiofeatures, snippetfeatures)
}
#We write the audio dataset to an interim csv
z <- gzfile(paste(interimdatadir,"audioData.csv.gz",sep=.Platform$file.sep),"w")
write.csv(audiofeatures, z)
close(z)


# feature extraction from eyetracking data (see LAK paper)
source('extractEyetrackingFeatures.R')
eyetrackdata <- extractEyetrackingFeatures(sessions, rawdatadir, window.ms, slide.ms, suffix.et.raw, suffix.et.fix, suffix.et.sac)
names(eyetrackdata)[[1]] <- "timestamp.orig"
eyetrackdata <- merge(window.times,eyetrackdata,all=T)
# We write the eyetracking dataset to an interim csv
z <- gzfile(paste(interimdatadir,"eyetrackData.csv.gz",sep=.Platform$file.sep),"w")
write.csv(eyetrackdata, z)
close(z)

# feature extraction from accelerometer data (see LAK paper): avg/sd/max/min/median of X,Y,Z, 30-coef FFT of X,Y,Z, avg/sd/max/min/median of Jerk, 30-coef FFT of jerk
source('extractAccelerometerFeatures.R')
accelfeatures <- extractAccelerometerFeatures(sessions, accelData, window.ms, slide.ms, fftcomp=30)
names(accelfeatures)[[1]] <- "timestamp"
accelfeatures <- merge(window.times,accelfeatures,all=T)
z <- gzfile(paste(interimdatadir,"accelFeatures.csv.gz",sep=.Platform$file.sep),"w")
write.csv(accelfeatures, z)
close(z)

# Merge the different interim datasets to form the final clean dataset
totaldata <- merge(eyetrackdata[,-c(4,5)],accelfeatures[,-c(4,5)],all=T)
totaldata <- merge(totaldata, audiofeatures, all=T)
totaldata <- merge(totaldata, videodata[,-c(1,1002)],all=T)
# Order by session and timestamp, in case we want to do time series
totaldata <- totaldata[ order(totaldata$session, totaldata$timestamp), ]
# head(totaldata)
z <- gzfile(paste(processeddatadir,"completeDataset.csv.gz",sep=.Platform$file.sep),"w")
write.csv(totaldata, z)
close(z)
