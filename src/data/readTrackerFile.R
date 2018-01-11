require(rjson)
require(plyr)

# Reads a teacher tracker file (in json format), and transforms it to a more-or-less-clean data frame
readTrackerFile <- function(file){
    
    trackerData <- data.frame(accelerationX=numeric(),
                              accelerationY=numeric(),
                              accelerationZ=numeric(),
                              timestamp=numeric(),
                              beaconID=character(),
                              proximity=numeric(),
                              distance=numeric(),
                              rssi=numeric())

    lines <- paste(readLines(file),collapse="")
    # Retouch it a bit so that it is proper json
    lines <- gsub("[]", "", lines, fixed=T)
    lines <- gsub("][", ",", lines, fixed=T)
    # We parse the json
    json_data <- fromJSON(lines)
    df <- ldply(json_data, data.frame)
    
    if(ncol(df)!=ncol(trackerData)){# If our resulting data frame does not have all columns, we add them (for the rbind to work)
        dummy <- data.frame(accelerationX=numeric(), accelerationY=numeric(), accelerationZ=numeric(),
                            timestamp=numeric(), beaconID=character(), proximity=numeric(), 
                            distance=numeric(), rssi=numeric())
        df <- merge(df,dummy,all = T)
    }
    
    trackerData <- rbind(trackerData,df)
    trackerData <- unique(trackerData)
    
    trackerData
}