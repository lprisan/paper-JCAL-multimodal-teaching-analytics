source('rollingWindows.R')

calculateJerk <- function(t, x, y, z){

    jerk <- numeric(length(t))

    jerk[1] <- NA

    for(i in 2:length(t)){
        jerk[i] <- sqrt((x[i]-x[i-1])^2+(y[i]-y[i-1])^2+(z[i]-z[i-1])^2)/(t[i]-t[i-1])
    }

    jerk
}


# This function receives a dataframe with the time series Accelerometer data and calculates the max, min, mean, median, sd and FFT components of X,Y, Z
# , plus the total jerk (dif. acceleration/dif. time), its max, min, mean, median, sd and FFT components
# For a sliding window of N seconds (N/2 s of slide between windows)
extractAccelerometerFeatures <- function(sessions, rawdata, window=10000, slide=5000, fftcomp=30){

    # Some basic parameters for the sliding windows (in seconds)
    totaldata <- data.frame()
    sessionids <- unique(sessions$session)

    for (session in sessionids){

        data <- rawdata[rawdata$session == session,]

        # We calculate the jerk of each time point
        data$jerk <- calculateJerk(t=data$timestamp, x=data$accelerationX, y=data$accelerationY, z=data$accelerationZ)


        # We get the start and end times for this session (start is always 0 as the accelerometer tracking marks the beginning of the period of interest)
        inittime <- 0
        endtime <- (sessions[sessions$session==session,'end'] - sessions[sessions$session==session,'start'])

        # We get the rolling window for the Attention and the Theta
        xmeandata <- rollingMean(data$timestamp,data$accelerationX,window,slide, inittime=inittime, endtime=endtime)
        xsddata <- rollingSd(data$timestamp,data$accelerationX,window,slide, inittime=inittime, endtime=endtime)
        xmaxdata <- rollingMax(data$timestamp,data$accelerationX,window,slide, inittime=inittime, endtime=endtime)
        xmindata <- rollingMin(data$timestamp,data$accelerationX,window,slide, inittime=inittime, endtime=endtime)
        xmediandata <- rollingMedian(data$timestamp,data$accelerationX,window,slide, inittime=inittime, endtime=endtime)
        xfftdata <- rollingFFT(data$timestamp,data$accelerationX,window,slide, inittime=inittime, endtime=endtime)
        
        ymeandata <- rollingMean(data$timestamp,data$accelerationY,window,slide, inittime=inittime, endtime=endtime)
        ysddata <- rollingSd(data$timestamp,data$accelerationY,window,slide, inittime=inittime, endtime=endtime)
        ymaxdata <- rollingMax(data$timestamp,data$accelerationY,window,slide, inittime=inittime, endtime=endtime)
        ymindata <- rollingMin(data$timestamp,data$accelerationY,window,slide, inittime=inittime, endtime=endtime)
        ymediandata <- rollingMedian(data$timestamp,data$accelerationY,window,slide, inittime=inittime, endtime=endtime)
        yfftdata <- rollingFFT(data$timestamp,data$accelerationY,window,slide, inittime=inittime, endtime=endtime)
        
        zmeandata <- rollingMean(data$timestamp,data$accelerationZ,window,slide, inittime=inittime, endtime=endtime)
        zsddata <- rollingSd(data$timestamp,data$accelerationZ,window,slide, inittime=inittime, endtime=endtime)
        zmaxdata <- rollingMax(data$timestamp,data$accelerationZ,window,slide, inittime=inittime, endtime=endtime)
        zmindata <- rollingMin(data$timestamp,data$accelerationZ,window,slide, inittime=inittime, endtime=endtime)
        zmediandata <- rollingMedian(data$timestamp,data$accelerationZ,window,slide, inittime=inittime, endtime=endtime)
        zfftdata <- rollingFFT(data$timestamp,data$accelerationZ,window,slide, inittime=inittime, endtime=endtime)
        
        jerkmeandata <- rollingMean(data$timestamp,data$jerk,window,slide, inittime=inittime, endtime=endtime)
        jerksddata <- rollingSd(data$timestamp,data$jerk,window,slide, inittime=inittime, endtime=endtime)
        jerkmaxdata <- rollingMax(data$timestamp,data$jerk,window,slide, inittime=inittime, endtime=endtime)
        jerkmindata <- rollingMin(data$timestamp,data$jerk,window,slide, inittime=inittime, endtime=endtime)
        jerkmediandata <- rollingMedian(data$timestamp,data$jerk,window,slide, inittime=inittime, endtime=endtime)
        jerkfftdata <- rollingFFT(data$timestamp,data$jerk,window,slide, inittime=inittime, endtime=endtime, fftcomp=fftcomp)

        newdata <- merge(xmeandata,xsddata,by="time",suffixes = c(".X.Mean",".X.SD"),all=T)
        newdata <- merge(newdata,xmaxdata,by="time",all=T)
        names(newdata)[[4]] <- paste(names(newdata)[[4]],"X.Max",sep=".")
        newdata <- merge(newdata,xmindata,by="time",all=T)
        names(newdata)[[5]] <- paste(names(newdata)[[5]],"X.Min",sep=".")
        newdata <- merge(newdata,xmediandata,by="time",all=T)
        names(newdata)[[6]] <- paste(names(newdata)[[6]],"X.Median",sep=".")
        newdata <- merge(newdata,xfftdata,by="time",all=T)
        for(i in 1:fftcomp){
            names(newdata)[[6+i]] <- paste("value.X.FFT",i,sep=".")
        }
        newdata <- merge(newdata,ymeandata,by="time",all=T)
        names(newdata)[[fftcomp+7]] <- paste(names(newdata)[[fftcomp+7]],"Y.Mean",sep=".")
        newdata <- merge(newdata,ysddata,by="time",all=T)
        names(newdata)[[fftcomp+8]] <- paste(names(newdata)[[fftcomp+8]],"Y.SD",sep=".")
        newdata <- merge(newdata,ymaxdata,by="time",all=T)
        names(newdata)[[fftcomp+9]] <- paste(names(newdata)[[fftcomp+9]],"Y.Max",sep=".")
        newdata <- merge(newdata,ymindata,by="time",all=T)
        names(newdata)[[fftcomp+10]] <- paste(names(newdata)[[fftcomp+10]],"Y.Min",sep=".")
        newdata <- merge(newdata,ymediandata,by="time",all=T)
        names(newdata)[[fftcomp+11]] <- paste(names(newdata)[[fftcomp+11]],"Y.Median",sep=".")
        newdata <- merge(newdata,yfftdata,by="time",all=T)
        for(i in 1:fftcomp){
            names(newdata)[[fftcomp+11+i]] <- paste("value.Y.FFT",i,sep=".")
        }
        newdata <- merge(newdata,zmeandata,by="time",all=T)
        names(newdata)[[(2*fftcomp)+12]] <- paste(names(newdata)[[(2*fftcomp)+12]],"Z.Mean",sep=".")
        newdata <- merge(newdata,zsddata,by="time",all=T)
        names(newdata)[[(2*fftcomp)+13]] <- paste(names(newdata)[[(2*fftcomp)+13]],"Z.SD",sep=".")
        newdata <- merge(newdata,zmaxdata,by="time",all=T)
        names(newdata)[[(2*fftcomp)+14]] <- paste(names(newdata)[[(2*fftcomp)+14]],"Z.Max",sep=".")
        newdata <- merge(newdata,zmindata,by="time",all=T)
        names(newdata)[[(2*fftcomp)+15]] <- paste(names(newdata)[[(2*fftcomp)+15]],"Z.Min",sep=".")
        newdata <- merge(newdata,zmediandata,by="time",all=T)
        names(newdata)[[(2*fftcomp)+16]] <- paste(names(newdata)[[(2*fftcomp)+16]],"Z.Median",sep=".")
        newdata <- merge(newdata,zfftdata,by="time",all=T)
        for(i in 1:fftcomp){
            names(newdata)[[(2*fftcomp)+16+i]] <- paste("value.Z.FFT",i,sep=".")
        }
        newdata <- merge(newdata,jerkmeandata,by="time",all=T)
        names(newdata)[[(3*fftcomp)+17]] <- paste(names(newdata)[[(3*fftcomp)+17]],"Jerk.Mean",sep=".")
        newdata <- merge(newdata,jerksddata,by="time",all=T)
        names(newdata)[[(3*fftcomp)+18]] <- paste(names(newdata)[[(3*fftcomp)+18]],"Jerk.SD",sep=".")
        newdata <- merge(newdata,jerkmaxdata,by="time",all=T)
        names(newdata)[[(3*fftcomp)+19]] <- paste(names(newdata)[[(3*fftcomp)+19]],"Jerk.Max",sep=".")
        newdata <- merge(newdata,jerkmindata,by="time",all=T)
        names(newdata)[[(3*fftcomp)+20]] <- paste(names(newdata)[[(3*fftcomp)+20]],"Jerk.Min",sep=".")
        newdata <- merge(newdata,jerkmediandata,by="time",all=T)
        names(newdata)[[(3*fftcomp)+21]] <- paste(names(newdata)[[(3*fftcomp)+21]],"Jerk.Median",sep=".")
        newdata <- merge(newdata,jerkfftdata,by="time",all=T)
        for(i in 1:fftcomp){
            names(newdata)[[(3*fftcomp)+21+i]] <- paste("value.Jerk.FFT",i,sep=".")
        }
        
        

        newdata$session <- session


        # We join the game data to our global dataset
        if(length(totaldata)==0) totaldata <- newdata
        else totaldata <- rbind(totaldata,newdata)

    }

    totaldata

}
