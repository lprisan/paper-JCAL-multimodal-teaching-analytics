source('rollingWindows.R')

# This function loads the time series pupil data from the eyetracker, the fixation and saccade details (in separate
# txt/csv files), and calculates the averages/counts of multiple eyetracking metrics
# and game variables for rolling windows of 10s (5s of slide between windows)
# it also receives a data frame with the starting and ending point for the calculations in each session (if not present, the whole file is used)
# Files from each session should be called [session]-events.txt, [session]-fixationDetails, [session]-saccadeDetails.txt
extractEyetrackingFeatures <- function(sessions, datadir, window=10000, slide=5000,
  suffix.et.raw="eyetracker-raw", suffix.et.fix="eyetracker-fixations", suffix.et.sac="eyetracker-saccades"){

    eyetrackdata <- data.frame()

    for (session in sessions$session){

        # We load the raw events export
        filename = paste(datadir,"/",session,"-",suffix.et.raw,".txt", sep="")
        print(filename)
        filedata <- read.csv(filename,as.is=T,comment.char="#")

        # From all the data, we only need timestamp, pupil diameter (L,R, in mm)
        filedata <- filedata[c(1,6,9)]
        filedata$session <- session
        pupildata <- data.frame(filedata)

        # We calculate the time baseline of the session
        time0 <- min(pupildata$Time)
        pupildata$Time.ms <- (pupildata$Time - time0) / 1000

        # We load the fixation details file
        filename = paste(datadir,"/",session,"-",suffix.et.fix,".txt", sep="")
        filedata <- read.csv(filename,comment.char="#", sep=";")
        if(ncol(filedata)==1){
            filedata <- read.csv(filename,comment.char="#", sep=",") # Different files have different separators!  
            filedata <- filedata[,c(7,8,9,15,16)]
        } else {
            # we select the meaningful columns (for now, only fixation start, duration, end in ms)
            # it is different in the exports we have for different teachers!
            filedata <- filedata[,c(8,9,10,16,17)]
        }


        filedata$session <- session
        fixdata <- data.frame(filedata)

        #We set the time of the fixation in the middle of the fixation
        fixdata$Time.ms <- (fixdata$Fixation.Start..ms. + (fixdata$Fixation.Duration..ms./2))
        # We create a Time field so that we have the time in both timestamp and ms formats
        fixdata$Time <- time0 + (fixdata$Time.ms)*1000
        fixdata$Fixation.Dispersion <- sqrt((fixdata$Dispersion.X*fixdata$Dispersion.X)+(fixdata$Dispersion.Y*fixdata$Dispersion.Y))

        # We load the saccade details file
        filename = paste(datadir,"/",session,"-",suffix.et.sac,".txt", sep="")
        filedata <- read.csv(filename,comment.char="#", sep=";")
        if(ncol(filedata)==1){
            filedata <- read.csv(filename,comment.char="#", sep=",") # Different files have different separators!  
            filedata <- filedata[,c(7,8,9:14)]
        } else {
            # we select the meaningful columns (for now, only saccade start, duration, end in ms and amplitude in degrees)
            # it is different in the exports we have for different teachers!
            filedata <- filedata[,c(8,9,10:15)]
        }
        
        filedata$session <- session
        sacdata <- data.frame(filedata)
        # We add the saccade speed and linear velocity for each saccade
        sacdata$Saccade.Speed <- sacdata$Amplitude.... / sacdata$Saccade.Duration..ms.
        sacdata$Saccade.Length<-sqrt(((sacdata$StartPosition.X-sacdata$EndPosition.X)*(sacdata$StartPosition.X-sacdata$EndPosition.X))
                                     +((sacdata$StartPosition.Y-sacdata$EndPosition.Y)*(sacdata$StartPosition.Y-sacdata$EndPosition.Y)))
        sacdata$Saccade.Velocity<-sacdata$Saccade.Length/sacdata$Saccade.Duration..ms.


        #We set the time of saccade in the middle of the fixation
        sacdata$Time.ms <- (sacdata$Saccade.Start..ms. + (sacdata$Saccade.Duration..ms./2))
        # We create a Time field so that we have the time in both timestamp and ms formats
        sacdata$Time <- time0 + (sacdata$Time.ms)*1000

        # We get the start and end times for this session
        inittime <- sessions[sessions$session==session,"start"]
        endtime <- sessions[sessions$session==session,"end"]

        # We get the rolling window for the mean pupil diameter, and its median value for a median cut
        meandata <- rollingMean(pupildata$Time.ms,pupildata$L.Pupil.Diameter..mm.,window,slide, inittime=inittime, endtime=endtime)

        # We get the rolling window for the SD of pupil diameter, and its median value for a median cut
        sddata <- rollingSd(pupildata$Time.ms,pupildata$L.Pupil.Diameter..mm.,window,slide, inittime=inittime, endtime=endtime)

        # We get the number of long fixations in the window, and its median
        longdata <- rollingLong(fixdata$Time.ms,fixdata$Fixation.Duration..ms.,window,slide, inittime=inittime, endtime=endtime)

        # We get the saccade speed in the window
        sacspdata <- rollingMean(sacdata$Time.ms,sacdata$Saccade.Speed,window,slide, inittime=inittime, endtime=endtime)

        # Fixation duration
        fdurdata <- rollingMean(fixdata$Time.ms,fixdata$Fixation.Duration..ms.,window,slide, inittime=inittime, endtime=endtime)

        # Fixation dispersion
        fdisdata <- rollingMean(fixdata$Time.ms,fixdata$Fixation.Dispersion,window,slide, inittime=inittime, endtime=endtime)

        # Saccade Duration
        sdurdata <- rollingMean(sacdata$Time.ms,sacdata$Saccade.Duration..ms.,window,slide, inittime=inittime, endtime=endtime)

        # Saccade Amplitude
        sampdata <- rollingMean(sacdata$Time.ms,sacdata$Amplitude....,window,slide, inittime=inittime, endtime=endtime)

        # Saccade Length
        slendata <- rollingMean(sacdata$Time.ms,sacdata$Saccade.Length,window,slide, inittime=inittime, endtime=endtime)

        # Saccade Velocity
        sveldata <- rollingMean(sacdata$Time.ms,sacdata$Saccade.Velocity,window,slide, inittime=inittime, endtime=endtime)


        data <- merge(meandata,sddata,by="time",suffixes = c(".Mean",".SD"),all=T)
        data <- merge(data,longdata,by="time",all=T)
        names(data)[[4]] <- paste(names(data)[[4]],"Fix",sep=".")
        data <- merge(data,sacspdata,by="time",all=T)
        names(data)[[5]] <- paste(names(data)[[5]],"Sac",sep=".")
        data <- merge(data,fdurdata,by="time",all=T)
        names(data)[[6]] <- paste(names(data)[[6]],"Fix.Dur",sep=".")
        data <- merge(data,fdisdata,by="time",all=T)
        names(data)[[7]] <- paste(names(data)[[7]],"Fix.Disp",sep=".")
        data <- merge(data,sdurdata,by="time",all=T)
        names(data)[[8]] <- paste(names(data)[[8]],"Sac.Dur",sep=".")
        data <- merge(data,sampdata,by="time",all=T)
        names(data)[[9]] <- paste(names(data)[[9]],"Sac.Amp",sep=".")
        data <- merge(data,slendata,by="time",all=T)
        names(data)[[10]] <- paste(names(data)[[10]],"Sac.Len",sep=".")
        data <- merge(data,sveldata,by="time",all=T)
        names(data)[[11]] <- paste(names(data)[[11]],"Sac.Vel",sep=".")




        data$session <- session

        # We join the game data to our global dataset
        if(length(eyetrackdata)==0) eyetrackdata <- data
        else eyetrackdata <- rbind(eyetrackdata,data)


    }

    eyetrackdata

}
