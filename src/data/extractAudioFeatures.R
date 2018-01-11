# Extracts a number of features from an audio snippet, using openSMILE tool
# snippet - a single-row dataframe with the timestamp, timestamp orig, session, etc.
# openSmileDir - directory where openSmile has been installed/cloned
# interimAudioDir - directory where the audio snippets are located, and where the data files will be generated
extractFeaturesFromSnippet <- function(snippet, openSmileDir, interimAudioDir){

    origDir <- getwd()

    snippetfeatures <- snippet
    # Generate a script to extract audio features using openSMILE
    # From the tutorial at http://records.mlab.no/2015/01/05/opensmile-the-munich-open-source-large-scale-multimedia-feature-extractor-a-tutorial-for-version-2-1/
    # use scripts/vad vad_opensource on each snippet to get the probabilities of it being voice
    # (to be executed from scripts/vad)
    # ../../bin/linux_x64_standalone_static/SMILExtract -I ../../data/luisSession1-5000.wav -C vad_opensource.conf -csvoutput ../../data/vad-luisSession1-5000.csv
    # --> Gives a list of frames (in 0.01s increments, and the likelihood if it being voice ><0 or <>0.5 rather)
    vadDir <- paste(openSmileDir,"scripts/vad",sep="")
    setwd(vadDir)
    snippetFile <- paste(interimAudioDir,.Platform$file.sep,as.character(snippet$session),"__",snippet$timestamp,".wav",sep="")
    vadDataFile <- paste(interimAudioDir,.Platform$file.sep,as.character(snippet$session),"__",snippet$timestamp,"-vad.csv",sep="")
    vadCmd <- paste(openSmileDir,"bin/linux_x64_standalone_static/SMILExtract -I ",snippetFile," -C vad_opensource.conf -csvoutput ",vadDataFile,sep="")
    system(vadCmd, wait=T)
    vadSnippetData <- read.csv(vadDataFile, header = F)
    # Get speech detection features
    # Percentage of the window that is considered speech
    snippetfeatures$vad.perc.speech <- sum(as.numeric(vadSnippetData$V2>0))/nrow(vadSnippetData)
    # aprox. to the curve's integral (sum of the values/number of samples = mean?)
    snippetfeatures$vad.mean.prob <- mean(vadSnippetData$V2)
    # sd of the curve
    snippetfeatures$vad.sd.prob <- sd(vadSnippetData$V2)
    # median of the curve
    snippetfeatures$vad.median.prob <- median(vadSnippetData$V2)
    # min of the curve
    snippetfeatures$vad.min.prob <- min(vadSnippetData$V2)
    # max of the curve
    snippetfeatures$vad.max.prob <- max(vadSnippetData$V2)
    # number of speech runs
    varRle <- data.frame(values=rle(as.numeric(vadSnippetData$V2>0))["values"], lengths=rle(as.numeric(vadSnippetData$V2>0))["lengths"])
    snippetfeatures$vad.nr.speech.runs <- nrow(varRle[varRle$values==1,])
    # average length of speech runs
    snippetfeatures$vad.len.speech.runs <- mean(varRle[varRle$values==1,"lengths"])
    # average length of nonspeech runs
    snippetfeatures$vad.len.nspeech.runs <- mean(varRle[varRle$values==0,"lengths"])
    # max length of speech runs
    snippetfeatures$vad.max.speech.runs <- max(varRle[varRle$values==1,"lengths"])
    # max length of nonspeech runs
    snippetfeatures$vad.max.nspeech.runs <- max(varRle[varRle$values==0,"lengths"])

    # Emotion recognition from emobase
    # Requires to uncompress in the root openSMILE http://www.audeering.com/research-and-open-source/files/emotion-models-0.1.0.zip
    # (from the root openSMILE folder?)
    # For long uncut file: ./bin/linux_x64_standalone_static/SMILExtract -C ./config/emobase_live4_batch.conf -I ./data/luisSession1-5000.wav > result.txt (needs to be parsed!)
    # For snippet files: ./bin/linux_x64_standalone_static/SMILExtract -C ./config/emobase_live4_batch_single.conf -I ./data/luisSession1-5000.wav > result.txt
    # --> gives a formatted file .txt with several kinds of emotion classification per turn, plus an output.arff file which specifies the turns (not sure which part of the turn is the frameTime!)
    setwd(openSmileDir)
    emoResults <- paste(interimAudioDir,.Platform$file.sep,as.character(snippet$session),"__",snippet$timestamp,"-emobase.txt",sep="")
    emoCmd <- paste("./bin/linux_x64_standalone_static/SMILExtract -C ./config/emobase_live4_batch_single.conf -I ",snippetFile," > ",emoResults,sep="")
    system(emoCmd, wait=T)
    emoData <- parseEmobase(emoResults)
    snippetfeatures <- cbind(snippetfeatures,emoData)

    # General-purpose brute force features from AVEC and InterSpeech challenges
    # execute scripts/avec2013/extract_all_audio.pl?
    # around 6000 features per window! ../bin/linux_x64_standalone_static/SMILExtract -I luisSession1-5000.wav -C ../config/IS13_ComParE.conf -csvoutput IS13-luisSession1-5000.csv
    setwd(openSmileDir)
    ISCsvResult <- paste(interimAudioDir,.Platform$file.sep,as.character(snippet$session),"__",snippet$timestamp,"-IS.csv",sep="")
    ISCmd <- paste("./bin/linux_x64_standalone_static/SMILExtract -I ",snippetFile," -C ./config/IS13_ComParE.conf -csvoutput ",ISCsvResult,sep="")
    system(ISCmd, wait=T)
    ISSnippetData <- read.csv(ISCsvResult, sep=";")
    snippetfeatures <- cbind(snippetfeatures, ISSnippetData[,-1])

    setwd(origDir)
    # We return a single-row data frame with 6410 fields (6405 features)
    snippetfeatures

}



# Parses the output of the EMOBASE feature extraction of openSMILE, converting it into a single-row data frame
parseEmobase <- function(filename){

    data <- numeric()
    con <- file(filename, "r")
    lins <- readLines(con)
    for (i in 1:length(lins)){
        fields <- strsplit(lins[i],split = "::", fixed = T)
        if(fields[[1]][1]=="SMILE-RESULT"){ # It is a real results
            if(grepl("arousal",fields[[1]][4],fixed = T)){ # It is the arousal feature
                data <- c(data, emobase.arousal = as.numeric(strsplit(fields[[1]][7],split="=",fixed = T)[[1]][2]))
            } else if(grepl("valence", fields[[1]][4], fixed = T)){ # It is the valence feature
                data <- c(data, emobase.valence = as.numeric(strsplit(fields[[1]][7],split="=",fixed = T)[[1]][2]))
            } else if(grepl("emodbEmotion", fields[[1]][4], fixed = T)){ # It is the emodbEmotion feature
                data <- c(data, emobase.edb.idx = as.numeric(strsplit(fields[[1]][7],split="=",fixed = T)[[1]][2]))
                data <- c(data, emobase.edb.anger = as.numeric(strsplit(fields[[1]][grepl("anger:",fields[[1]],fixed = T)],"anger:", fixed=T)[[1]][2]))
                data <- c(data, emobase.edb.boredom = as.numeric(strsplit(fields[[1]][grepl("boredom:",fields[[1]],fixed = T)],"boredom:", fixed=T)[[1]][2]))
                data <- c(data, emobase.edb.disgust = as.numeric(strsplit(fields[[1]][grepl("disgust:",fields[[1]],fixed = T)],"disgust:", fixed=T)[[1]][2]))
                data <- c(data, emobase.edb.fear = as.numeric(strsplit(fields[[1]][grepl("fear:",fields[[1]],fixed = T)],"fear:", fixed=T)[[1]][2]))
                data <- c(data, emobase.edb.happiness = as.numeric(strsplit(fields[[1]][grepl("happiness:",fields[[1]],fixed = T)],"happiness:", fixed=T)[[1]][2]))
                data <- c(data, emobase.edb.neutral = as.numeric(strsplit(fields[[1]][grepl("neutral:",fields[[1]],fixed = T)],"neutral:", fixed=T)[[1]][2]))
                data <- c(data, emobase.edb.sadness = as.numeric(strsplit(fields[[1]][grepl("sadness:",fields[[1]],fixed = T)],"sadness:", fixed=T)[[1]][2]))
            } else if(grepl("abcAffect", fields[[1]][4], fixed = T)){ # It is the abcAffect feature
                data <- c(data, emobase.abc.idx = as.numeric(strsplit(fields[[1]][7],split="=",fixed = T)[[1]][2]))
                data <- c(data, emobase.abc.agressiv = as.numeric(strsplit(fields[[1]][grepl("agressiv:",fields[[1]],fixed = T)],"agressiv:", fixed=T)[[1]][2]))
                data <- c(data, emobase.abc.cheerful = as.numeric(strsplit(fields[[1]][grepl("cheerful:",fields[[1]],fixed = T)],"cheerful:", fixed=T)[[1]][2]))
                data <- c(data, emobase.abc.intoxicated = as.numeric(strsplit(fields[[1]][grepl("intoxicated:",fields[[1]],fixed = T)],"intoxicated:", fixed=T)[[1]][2]))
                data <- c(data, emobase.abc.nervous = as.numeric(strsplit(fields[[1]][grepl("nervous:",fields[[1]],fixed = T)],"nervous:", fixed=T)[[1]][2]))
                data <- c(data, emobase.abc.neutral = as.numeric(strsplit(fields[[1]][grepl("neutral:",fields[[1]],fixed = T)],"neutral:", fixed=T)[[1]][2]))
                data <- c(data, emobase.abc.tired = as.numeric(strsplit(fields[[1]][grepl("tired:",fields[[1]],fixed = T)],"tired:", fixed=T)[[1]][2]))
            } else if(grepl("avicInterest", fields[[1]][4], fixed = T)){ # It is the avicInterest feature
                data <- c(data, emobase.interest.idx = as.numeric(strsplit(fields[[1]][7],split="=",fixed = T)[[1]][2]))
                data <- c(data, emobase.interest.loi1 = as.numeric(strsplit(fields[[1]][grepl("loi1:",fields[[1]],fixed = T)],"loi1:", fixed=T)[[1]][2]))
                data <- c(data, emobase.interest.loi2 = as.numeric(strsplit(fields[[1]][grepl("loi2:",fields[[1]],fixed = T)],"loi2:", fixed=T)[[1]][2]))
                data <- c(data, emobase.interest.loi3 = as.numeric(strsplit(fields[[1]][grepl("loi3:",fields[[1]],fixed = T)],"loi3:", fixed=T)[[1]][2]))
            }
        }
    }
    close(con)
    data <- as.data.frame(t(data))
    data

}

# VARIANT THAT DOES NOT EXECUTE the openSMILE COMMANDS, ASSUMES THE FILES ARE ALREADY THERE
# Extracts a number of features from an audio snippet, using openSMILE tool
# snippet - a single-row dataframe with the timestamp, timestamp orig, session, etc.
# openSmileDir - directory where openSmile has been installed/cloned
# interimAudioDir - directory where the audio snippets are located, and where the data files will be generated
extractFeaturesFromSnippetNOCMD <- function(snippet, openSmileDir, interimAudioDir){

    # origDir <- getwd()

    snippetfeatures <- snippet
    # Generate a script to extract audio features using openSMILE
    # From the tutorial at http://records.mlab.no/2015/01/05/opensmile-the-munich-open-source-large-scale-multimedia-feature-extractor-a-tutorial-for-version-2-1/
    # use scripts/vad vad_opensource on each snippet to get the probabilities of it being voice
    # (to be executed from scripts/vad)
    # ../../bin/linux_x64_standalone_static/SMILExtract -I ../../data/luisSession1-5000.wav -C vad_opensource.conf -csvoutput ../../data/vad-luisSession1-5000.csv
    # --> Gives a list of frames (in 0.01s increments, and the likelihood if it being voice ><0 or <>0.5 rather)
    # vadDir <- paste(openSmileDir,"scripts/vad",sep="")
    # setwd(vadDir)
    # snippetFile <- paste(interimAudioDir,.Platform$file.sep,as.character(snippet$session),"__",snippet$timestamp,".wav",sep="")
    vadDataFile <- paste(interimAudioDir,.Platform$file.sep,as.character(snippet$session),"__",snippet$timestamp,"-vad.csv",sep="")
    # vadCmd <- paste(openSmileDir,"bin/linux_x64_standalone_static/SMILExtract -I ",snippetFile," -C vad_opensource.conf -csvoutput ",vadDataFile,sep="")
    # system(vadCmd, wait=T)
    vadSnippetData <- read.csv(vadDataFile, header = F)
    # Get speech detection features
    # Percentage of the window that is considered speech
    snippetfeatures$vad.perc.speech <- sum(as.numeric(vadSnippetData$V2>0))/nrow(vadSnippetData)
    # aprox. to the curve's integral (sum of the values/number of samples = mean?)
    snippetfeatures$vad.mean.prob <- mean(vadSnippetData$V2)
    # sd of the curve
    snippetfeatures$vad.sd.prob <- sd(vadSnippetData$V2)
    # median of the curve
    snippetfeatures$vad.median.prob <- median(vadSnippetData$V2)
    # min of the curve
    snippetfeatures$vad.min.prob <- min(vadSnippetData$V2)
    # max of the curve
    snippetfeatures$vad.max.prob <- max(vadSnippetData$V2)
    # number of speech runs
    varRle <- data.frame(values=rle(as.numeric(vadSnippetData$V2>0))["values"], lengths=rle(as.numeric(vadSnippetData$V2>0))["lengths"])
    snippetfeatures$vad.nr.speech.runs <- nrow(varRle[varRle$values==1,])
    # average length of speech runs
    snippetfeatures$vad.len.speech.runs <- mean(varRle[varRle$values==1,"lengths"])
    # average length of nonspeech runs
    snippetfeatures$vad.len.nspeech.runs <- mean(varRle[varRle$values==0,"lengths"])
    # max length of speech runs
    snippetfeatures$vad.max.speech.runs <- max(varRle[varRle$values==1,"lengths"])
    # max length of nonspeech runs
    snippetfeatures$vad.max.nspeech.runs <- max(varRle[varRle$values==0,"lengths"])

    # Emotion recognition from emobase
    # Requires to uncompress in the root openSMILE http://www.audeering.com/research-and-open-source/files/emotion-models-0.1.0.zip
    # (from the root openSMILE folder?)
    # For long uncut file: ./bin/linux_x64_standalone_static/SMILExtract -C ./config/emobase_live4_batch.conf -I ./data/luisSession1-5000.wav > result.txt (needs to be parsed!)
    # For snippet files: ./bin/linux_x64_standalone_static/SMILExtract -C ./config/emobase_live4_batch_single.conf -I ./data/luisSession1-5000.wav > result.txt
    # --> gives a formatted file .txt with several kinds of emotion classification per turn, plus an output.arff file which specifies the turns (not sure which part of the turn is the frameTime!)
    # setwd(openSmileDir)
    emoResults <- paste(interimAudioDir,.Platform$file.sep,as.character(snippet$session),"__",snippet$timestamp,"-emobase.txt",sep="")
    # emoCmd <- paste("./bin/linux_x64_standalone_static/SMILExtract -C ./config/emobase_live4_batch_single.conf -I ",snippetFile," > ",emoResults,sep="")
    # system(emoCmd, wait=T)
    emoData <- parseEmobase(emoResults)
    snippetfeatures <- cbind(snippetfeatures,emoData)

    # General-purpose brute force features from AVEC and InterSpeech challenges
    # execute scripts/avec2013/extract_all_audio.pl?
    # around 6000 features per window! ../bin/linux_x64_standalone_static/SMILExtract -I luisSession1-5000.wav -C ../config/IS13_ComParE.conf -csvoutput IS13-luisSession1-5000.csv
    # setwd(openSmileDir)
    ISCsvResult <- paste(interimAudioDir,.Platform$file.sep,as.character(snippet$session),"__",snippet$timestamp,"-IS.csv",sep="")
    # ISCmd <- paste("./bin/linux_x64_standalone_static/SMILExtract -I ",snippetFile," -C ./config/IS13_ComParE.conf -csvoutput ",ISCsvResult,sep="")
    # system(ISCmd, wait=T)
    ISSnippetData <- read.csv(ISCsvResult, sep=";")
    snippetfeatures <- cbind(snippetfeatures, ISSnippetData[,-1])

    # setwd(origDir)
    # We return a single-row data frame with 6410 fields (6405 features)
    snippetfeatures

}
