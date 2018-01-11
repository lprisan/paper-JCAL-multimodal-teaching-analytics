getWindowTimes <- function(sessions, window.length=10000, slide.length=5000){
    
    times <- data.frame()
    for(i in 1:nrow(sessions)){
        session <- sessions[i,]
        timestamps <- seq(from=window.length/2, to=(session$end-session$start), by=slide.length)
        timestamps.orig <- seq(from=session$start+(window.length/2), to=session$end, by=slide.length)
        sessiontimes <- data.frame(timestamp=numeric(length(timestamps)), timestamp.orig=numeric(length(timestamps.orig)))
        sessiontimes$timestamp <- timestamps
        sessiontimes$timestamp.orig <- timestamps.orig
        sessiontimes$session <- session$session
        if(nrow(times)==0) times <- sessiontimes
        else times <- rbind(times,sessiontimes)
    }
    times
}

