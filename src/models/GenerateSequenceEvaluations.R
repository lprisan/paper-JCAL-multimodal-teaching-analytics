# This helper script generates the sequence of commands needed to run
# the whole evaluation harness of the paper, for a certain training/testing model
# It outputs a txt file with the series of commands to run, names <command>.inputs.txt
# It assumes the R script is runnable directly (e.g., in Linux, starts with #!/usr/bin/env Rscript)

##############################################
# This prefix serves to distinguish the labels of, say, the kind of model we train/test here, e.g., Random Forest, LSTMs5layers, etc.
# Change it each time you generate a new file like this, to keep things findable!
# And do not put "_" in the middle of the prefix (we use _ as separators in the visualization scripts)
command <- "Train_Evaluate_RF_MC.R"
# LABELPREFIX <- "SVMBestNoCorr_"
LABELPREFIX <- "RF_MC_"
###############################################


outfile <- paste(command,".inputs.txt",sep="")
fileConn<-file(outfile)

lines <- character()

# List of sessions
sessions <- c("case1-day1-session1-teacher1","case1-day1-session2-teacher1",
              "case1-day1-session3-teacher1","case1-day1-session4-teacher1",
              "case2-day1-session1-teacher2","case2-day1-session2-teacher2",
              "case2-day2-session1-teacher2","case2-day2-session2-teacher2",
              "case2-day3-session1-teacher2","case2-day3-session2-teacher2",
              "case2-day4-session1-teacher2","case2-day4-session2-teacher2")

wrap <- function(string){
  new <- paste("\'",string,"\'",sep="")
  new
}

# For each target variable
targets <- c("Activity", "Social")
for(target in targets){

  # For each combination of data sources...
  #sources <- c("et","acc","vid","aud","acc,aud","aud,vid","acc,aud,vid","all")
  sources <- c("all")
  for(source in sources){

    # General models (multi-teacher)

    ## Leave one session out
    for(i in 1:length(sessions)){
      s <- sessions[i]

      trainsessions <- sessions[!(sessions %in% s)]
      trainstr <- paste(trainsessions, collapse=",")
      testsessions <- sessions[sessions %in% s]
      teststr <- paste(testsessions, collapse=",")

      label <- paste(LABELPREFIX,gsub(pattern = ",", replacement = "", x = source),
                     "_GM_LOSO_",target,"_",i,sep="")

      # We have to build commands of the form: <command> <label> <target-variable> <data-sources> <train-set-sessions> <test-set-sessions>
      cmdLine <- paste(command,wrap(label),wrap(target),wrap(source),wrap(trainstr),wrap(teststr))

      lines <- c(lines, cmdLine)
    }


    ## Leave one teacher out (train on one, test on the other)
    teachers <- c("teacher1","teacher2")
    # for(i in 1:length(teachers)){
    #   t <- teachers[i]
    # 
    #   trainsessions <- sessions[!grepl(t,sessions,fixed=T)]
    #   trainstr <- paste(trainsessions, collapse=",")
    #   testsessions <- sessions[grepl(t,sessions,fixed=T)]
    #   teststr <- paste(testsessions, collapse=",")
    # 
    #   label <- paste(LABELPREFIX,gsub(pattern = ",", replacement = "", x = source),
    #                  "_GM_LOTO_",target,"_",i,sep="")
    # 
    #   # We have to build commands of the form: <command> <label> <target-variable> <data-sources> <train-set-sessions> <test-set-sessions>
    #   cmdLine <- paste(command,wrap(label),wrap(target),wrap(source),wrap(trainstr),wrap(teststr))
    # 
    #   lines <- c(lines, cmdLine)
    # }

    # Personalized models (trained and tested with data from ONE teacher)

    ## Leave one session out
    for(j in 1:length(teachers)){
      t <- teachers[j]
      partsessions <- sessions[grepl(t,sessions,fixed=T)] # Sessions of this teacher
      # We train/test on the data for one teacher only
      for(i in 1:length(partsessions)){
        s <- partsessions[i]

        trainsessions <- partsessions[!(partsessions %in% s)]
        trainstr <- paste(trainsessions, collapse=",")
        testsessions <- partsessions[partsessions %in% s]
        teststr <- paste(testsessions, collapse=",")

        label <- paste(LABELPREFIX,gsub(pattern = ",", replacement = "", x = source),
                       "_PM_LOSO_",target,"_t",j,"s",i,sep="")

        # We have to build commands of the form: <command> <label> <target-variable> <data-sources> <train-set-sessions> <test-set-sessions>
        cmdLine <- paste(command,wrap(label),wrap(target),wrap(source),wrap(trainstr),wrap(teststr))

        lines <- c(lines, cmdLine)
      }
    }

    ## Leave one situation out (only for teacher 2, teacher 1 has only 1 situation)
    # partsessions <- sessions[grepl("teacher2",sessions,fixed=T)]
    # situations <- c("day1","day2","day3","day4")
    # for(i in 1:length(situations)){
    #   s <- situations[i]
    #   trainsessions <- partsessions[!grepl(s,partsessions,fixed=T)]
    #   trainstr <- paste(trainsessions, collapse=",")
    #   testsessions <- partsessions[grepl(s,partsessions,fixed=T)]
    #   teststr <- paste(testsessions, collapse=",")
    # 
    #   label <- paste(LABELPREFIX,gsub(pattern = ",", replacement = "", x = source),
    #                  "_PM_LOSitO_",target,"_",i,sep="")
    # 
    #   # We have to build commands of the form: <command> <label> <target-variable> <data-sources> <train-set-sessions> <test-set-sessions>
    #   cmdLine <- paste(command,wrap(label),wrap(target),wrap(source),wrap(trainstr),wrap(teststr))
    # 
    #   lines <- c(lines, cmdLine)
    # 
    # }


  }




}

writeLines(lines, fileConn)
close(fileConn)
