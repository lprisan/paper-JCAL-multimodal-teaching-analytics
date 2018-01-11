
getActivityForTimestamp <- function(timestamp, annotationsData, sessionid, originalTimestamp=F){
    
    value <- ""
    start <- annotationsData[annotationsData$tier=="Recording" & annotationsData$annotation=="Recording" & annotationsData$session==sessionid, "start"]
    timestamp <- as.numeric(timestamp)
    if(!originalTimestamp){ # Default behavior, we consider the passed timestamp is from the origin of our aligned data
        value <- as.character(annotationsData[annotationsData$start<=(timestamp+start) & annotationsData$end>=(timestamp+start) & annotationsData$session==sessionid & annotationsData$tier=="Activity","annotation"])
    }else{ # The timestamp passed refers to the original video/ET file's origin
        value <- as.character(annotationsData[annotationsData$start<=timestamp & annotationsData$end>=timestamp & annotationsData$session==sessionid & annotationsData$tier=="Activity","annotation"])
    }
    
    
    value    
}


getSocialForTimestamp <- function(timestamp, annotationsData, sessionid, originalTimestamp=F){
    
    value <- ""
    start <- annotationsData[annotationsData$tier=="Recording" & annotationsData$annotation=="Recording" & annotationsData$session==sessionid, "start"]
    timestamp <- as.numeric(timestamp)
    if(!originalTimestamp){ # Default behavior, we consider the passed timestamp is from the origin of our aligned data
        value <- as.character(annotationsData[annotationsData$start<=(timestamp+start) & annotationsData$end>=(timestamp+start) & annotationsData$session==sessionid & annotationsData$tier=="Social","annotation"])
    }else{ # The timestamp passed refers to the original video/ET file's origin
        value <- as.character(annotationsData[annotationsData$start<=timestamp & annotationsData$end>=timestamp & annotationsData$session==sessionid & annotationsData$tier=="Social","annotation"])
    }
    
    
    value    
}

getActivityForWindow <- function(timestamp, annotationsData, sessionid, originalTimestamp=F, window.length=10000){
    
    
    timestamp <- as.numeric(timestamp)

    sessionstart <- annotationsData[annotationsData$tier=="Recording" & annotationsData$annotation=="Recording" & annotationsData$session==sessionid, "start"]
    window.start <- timestamp-(window.length/2)
    window.end <- timestamp+(window.length/2)
    
    relevant <- data.frame()
    if(!originalTimestamp){ # Default behavior, we consider the passed timestamp is from the origin of our aligned data
        relevant <- annotationsData[annotationsData$tier=="Activity" & annotationsData$session==sessionid & annotationsData$start<(sessionstart+window.end) & annotationsData$end>(sessionstart+window.start),]
    }else{ # The timestamp passed refers to the original video/ET file's origin
        relevant <- annotationsData[annotationsData$tier=="Activity" & annotationsData$session==sessionid & annotationsData$start<window.end & annotationsData$end>window.start,]
    }
    
    codetimes = data.frame(value=character(),time=numeric())
    for(i in 1:nrow(relevant)){
        rel <- relevant[i,]
        codestart = ifelse(rel$start < window.start,window.start,rel$start) # We chop off the part of the code outside the window
        codeend = ifelse(rel$end > window.end,window.end,rel$end) # We chop off the part of the code outside the window
        
        if(nrow(subset(codetimes,value==as.character(rel$annotation)))==0){# New code in the list
            code = data.frame(value=as.character(rel$annotation), time=codeend-codestart, stringsAsFactors = F)
            codetimes <- rbind(codetimes,code)
        }else{# The code was already in the list
            codetimes[codetimes$value==as.character(rel$annotation),"time"] <- codetimes[codetimes$value==as.character(rel$annotation),"time"]+(codeend-codestart)
        }
    }
    
    # We order in terms of time, and add the longest running code
    codetimes <- codetimes[order(-codetimes$time),]
    if(nrow(subset(codetimes,time==max(time)))>1) val <- NA # If there is a tie, we add an NA
    else val <- codetimes[1,"value"]
    
    val
}


getSocialForWindow <- function(timestamp, annotationsData, sessionid, originalTimestamp=F, window.length=10000){

    timestamp <- as.numeric(timestamp)

    sessionstart <- annotationsData[annotationsData$tier=="Recording" & annotationsData$annotation=="Recording" & annotationsData$session==sessionid, "start"]
    window.start <- timestamp-(window.length/2)
    window.end <- timestamp+(window.length/2)
    
    relevant <- data.frame()
    if(!originalTimestamp){ # Default behavior, we consider the passed timestamp is from the origin of our aligned data
        relevant <- annotationsData[annotationsData$tier=="Social" & annotationsData$session==sessionid & annotationsData$start<(sessionstart+window.end) & annotationsData$end>(sessionstart+window.start),]
    }else{ # The timestamp passed refers to the original video/ET file's origin
        relevant <- annotationsData[annotationsData$tier=="Social" & annotationsData$session==sessionid & annotationsData$start<window.end & annotationsData$end>window.start,]
    }
    
    codetimes = data.frame(value=character(),time=numeric())
    for(i in 1:nrow(relevant)){
        rel <- relevant[i,]
        codestart = ifelse(rel$start < window.start,window.start,rel$start) # We chop off the part of the code outside the window
        codeend = ifelse(rel$end > window.end,window.end,rel$end) # We chop off the part of the code outside the window
        
        if(nrow(subset(codetimes,value==as.character(rel$annotation)))==0){# New code in the list
            code = data.frame(value=as.character(rel$annotation), time=codeend-codestart, stringsAsFactors = F)
            codetimes <- rbind(codetimes,code)
        }else{# The code was already in the list
            codetimes[codetimes$value==as.character(rel$annotation),"time"] <- codetimes[codetimes$value==as.character(rel$annotation),"time"]+(codeend-codestart)
        }
    }
    
    # We order in terms of time, and add the longest running code
    codetimes <- codetimes[order(-codetimes$time),]
    if(nrow(subset(codetimes,time==max(time)))>1) val <- NA # If there is a tie, we add an NA
    else val <- codetimes[1,"value"]
    
    val
    
}
