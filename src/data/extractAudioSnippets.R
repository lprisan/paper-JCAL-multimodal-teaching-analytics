# Assumes timestamp is related to the video!
extractAudioSnippet <- function(timestamp, timestamp.orig, sessionid, datadir, destdir, win.length=10000){
    
    timestamp <- as.numeric(timestamp)
    timestamp.orig <- as.numeric(timestamp.orig)
    videoFile <- paste(datadir,.Platform$file.sep,sessionid,"-video.avi",sep="")
    snippetFile <- paste(destdir,.Platform$file.sep,sessionid,"__",timestamp,".wav",sep="")
    
    if(file.exists(videoFile)){
        #command should be: ffmpeg -i case2-day4-session2-teacher2-video.avi -ss 00:00:00 -t 00:00:10  -acodec copy test.wav
        command <- paste("ffmpeg -i ",videoFile," -ss ",msToHMS(timestamp.orig-(win.length/2))," -t 00:00:10 -acodec copy ",snippetFile, sep="")
        system(command, wait=T)
        print(paste("DONE",snippetFile))
    }else{
        print(paste("File",videoFile,"does not exist!"))
    }
    
    
}


msToHMS <- function(ms){
    H <- floor(ms/(60*60*1000))
    M <- floor((ms-(H*60*60*1000))/(60*1000))
    S <- floor((ms-(H*60*60*1000)-(M*60*1000))/1000)
    
    
    H <- formatC(H, width = 2, format = "d", flag = "0")
    M <- formatC(M, width = 2, format = "d", flag = "0")
    S <- formatC(S, width = 2, format = "d", flag = "0")
    
    paste(H,M,S,sep=":")
}

