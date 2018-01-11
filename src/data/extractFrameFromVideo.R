# Assumes timestamp is related to the video!
extractFrameFromVideo <- function(timestamp, timestamp.orig, sessionid, datadir, destdir){
    
    timestamp <- as.numeric(timestamp)
    timestamp.orig <- as.numeric(timestamp.orig)
    videoFile <- paste(datadir,.Platform$file.sep,sessionid,"-video.avi",sep="")
    frameFile <- paste(destdir,.Platform$file.sep,sessionid,"__",timestamp,".jpg",sep="")
    
    if(file.exists(videoFile)){
        command <- paste("ffmpeg -ss ",msToHMS(timestamp.orig)," -i ",videoFile," -vframes 1 ",frameFile, sep="")
        system(command, wait=T)
        print(paste("DONE",frameFile))
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

