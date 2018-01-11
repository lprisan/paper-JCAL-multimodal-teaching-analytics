#!/usr/bin/env Rscript

# This script takes 
# Usage: visualizePerformancesFolder.R <dir-with-rdata-files>
# NB: It requires quite a few data files from different schemas for the graphs to work correctly

library(ggplot2)
library(jsonlite)
library(caret)

args <- commandArgs(trailingOnly=FALSE)
# Get the --file argument to get the current script location
arg <- (args[grepl("--file=",args,fixed=T)])[1]
fullpath <- unlist(strsplit(arg,"--file=", fixed=T))[2]
path <- dirname(normalizePath(fullpath))

args <- commandArgs(trailingOnly=TRUE)
print(args)
if(length(args)!=1){
  stop("Wrong number of arguments. Usage:\nvisualizePerformancesFolder.R <dir-with-rdata-files>")
}

strdir <- args[1]
#strdir <- "../models/perfs/"

# Find and read the RData files
files <- list.files(path = strdir, pattern = "\\.RData$", ignore.case = T)

nor=F

df <- data.frame()
if(length(files)>0){
  
  for(file in files){
    
    out <- load(paste(strdir,file,sep=.Platform$file.sep))
    #print(out)
    data <- data.frame(label=label, auc=auc, kappa=cm$overall['Kappa'], acc=cm$overall['Accuracy'], f1=f1, lang="R", stringsAsFactors = F)
    
    if(nrow(df)==0){
      df <- data 
    }
    else{
      df <- rbind(df,data) 
    }
  }
  
}else{
  print("No R data files in the specified location!")
  nor=T
}


# Same thing for the python performance files
files <- list.files(path = strdir, pattern = "\\.perf.csv$", ignore.case = T)
if(length(files)>0){
  
  for(file in files){
    
    out <- read.csv(paste(strdir,file,sep=.Platform$file.sep))
    # Parsing confusion matrix
    matrix <- fromJSON(as.character(out$cm))
    cm <-  confusionMatrix(as.table(matrix))
    #print(out)
    data <- data.frame(label=out$label, auc=out$auc, kappa=out$kappa, acc=out$acc, f1=out$f1, lang="Python", stringsAsFactors = )
    if(nrow(df)==0){
      df <- data 
    }
    else{
      df <- rbind(df,data) 
    }
  }
  
}else{
  print("No Python data files in the specified location!")
  if(nor){
    stop("No data files to analyze!")
  }
}





# Add the language used to model, to spot check performance metric variability
df$lang <- factor(df$lang)
# Add some group variables for each kind of target, sources, validation model
df$model <- factor(sapply(as.character(df$label), FUN=function(x){unlist(strsplit(x, split = "_", fixed = T))[1]}))
df$sources <- factor(sapply(as.character(df$label), FUN=function(x){unlist(strsplit(x, split = "_", fixed = T))[2]}))
df$modeltype <- factor(sapply(as.character(df$label), FUN=function(x){unlist(strsplit(x, split = "_", fixed = T))[3]}))
df$validation <- factor(sapply(as.character(df$label), FUN=function(x){unlist(strsplit(x, split = "_", fixed = T))[4]}))
df$target <- factor(sapply(as.character(df$label), FUN=function(x){unlist(strsplit(x, split = "_", fixed = T))[5]}))
df$modelsources <- factor(paste(df$model,df$sources,sep="_"))


qplot(f1, acc, data=df, colour=lang, geom=c('point', 'smooth'), main='Evaluation metrics in the different languages') # Curves look quite different between R and Py, but the correlation is strong
# qplot(kappa, acc, data=df, colour=lang, geom=c('point', 'smooth'))
# qplot(auc, acc, data=df, colour=lang, geom=c('point', 'smooth')) # R part looks worrysome... how can we have auc 0.9 with acc 0.4?
# qplot(auc, f1, data=df, colour=lang, geom=c('point', 'smooth')) # Not sure we can trust AUC to be consistent across implementations


print(str(df))

setwd(path)

# Graphs for Activity, leave for now only f1, to be used as main metric
# gg <- ggplot(df[df$target=='Activity',], aes(x=modelsources, y=acc)) + 
#   geom_boxplot(aes(fill=model)) + facet_grid(validation ~ modeltype) +
#   theme_bw() + ggtitle("Accuracy, Activity") + geom_jitter(alpha=0.1) + coord_flip()
# gg
# gg <- ggplot(df[df$target=='Activity',], aes(x=modelsources, y=kappa)) + 
#   geom_boxplot(aes(fill=model)) + facet_grid(validation ~ modeltype) +
#   theme_bw() + ggtitle("Kappa, Activity")  + geom_jitter(alpha=0.1) + coord_flip()
# gg
# gg <- ggplot(df[df$target=='Activity',], aes(x=modelsources, y=auc)) + 
#   geom_boxplot(aes(fill=model)) + facet_grid(validation ~ modeltype) +
#   theme_bw() + ggtitle("AUC, Activity") + geom_jitter(alpha=0.1) + coord_flip()
# gg
# Get some specific labels to put on the graph (optional)
#df$partlabel = ifelse(endsWith(as.character(df$label),"LOSO_Activity_1"),as.character(df$label),NA)
gg <- ggplot(df[df$target=='Activity',], aes(x=modelsources, y=f1)) + 
  geom_boxplot(aes(fill=model)) + facet_grid(validation ~ modeltype) +
  theme_bw() + ggtitle("F1 Score, Activity") + geom_jitter(alpha=0.1) + coord_flip() # + geom_text(aes(x=modelsources,y=f1,label=partlabel))   
gg

# Graphs for Social
# gg <- ggplot(df[df$target=='Social',], aes(x=modelsources, y=acc)) + 
#   geom_boxplot(aes(fill=model)) + facet_grid(validation ~ modeltype) +
#   theme_bw() + ggtitle("Accuracy, Social") + geom_jitter(alpha=0.1) + coord_flip()
# gg
# gg <- ggplot(df[df$target=='Social',], aes(x=modelsources, y=kappa)) + 
#   geom_boxplot(aes(fill=model)) + facet_grid(validation ~ modeltype) +
#   theme_bw() + ggtitle("Kappa, Social") + geom_jitter(alpha=0.1) + coord_flip()
# gg
# gg <- ggplot(df[df$target=='Social',], aes(x=modelsources, y=auc)) + 
#   geom_boxplot(aes(fill=model)) + facet_grid(validation ~ modeltype) +
#   theme_bw() + ggtitle("AUC, Social") + geom_jitter(alpha=0.1) + coord_flip()
# gg
gg <- ggplot(df[df$target=='Social',], aes(x=modelsources, y=f1)) + 
  geom_boxplot(aes(fill=model)) + facet_grid(validation ~ modeltype) +
  theme_bw() + ggtitle("F1 Score, Social") + geom_jitter(alpha=0.1) + coord_flip()
gg
