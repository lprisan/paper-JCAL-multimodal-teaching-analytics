library(ggplot2)
require(gridExtra)

data <- read.csv("../../data/processed/completeDataset.csv", stringsAsFactors = F)


data$Teacher <- as.factor(ifelse(grepl("teacher1", data$session, fixed = T), "T1", "T2"))
data$Activity <- ifelse(is.na(as.character(data$Activity.win)) | 
                                    as.character(data$Activity.win)=='OFF' |
                                    as.character(data$Activity.win)=='TDT' |
                                    as.character(data$Activity.win)=='TEC',
                                  'Other',as.character(data$Activity.win))
data$Social <- ifelse(is.na(as.character(data$Social.win)),
                                'Other',as.character(data$Social.win))


###### Figure on number/type of episodes
                      
data$Activity <- as.factor(data$Activity)
levels(data$Activity) <- c("Explanation", "Monitoring", "Other", "Questioning", "Repairs")
data$Activity <- factor(data$Activity,levels(data$Activity)[c(1,2,4,5,3)])
episodesAct <- as.data.frame(table(data$Activity, data$Teacher))
names(episodesAct) <- c("Type", "Teacher", "Episodes")
plot1 <- ggplot(episodesAct, aes(x=Type, y=Episodes, fill=Teacher))+geom_bar(stat="identity")+
  theme_minimal()+scale_fill_grey()+ggtitle("Number of episodes, Teaching activity")+
  labs(x="Type of teaching activity", y="Episodes")

data$Social <- as.factor(data$Social)
levels(data$Social) <- c("Class-wide", "Small group", "Individual", "Other")
episodesSoc <- as.data.frame(table(data$Social, data$Teacher))
names(episodesSoc) <- c("Type", "Teacher", "Episodes")
plot2 <- ggplot(episodesSoc, aes(x=Type, y=Episodes, fill=Teacher))+geom_bar(stat="identity")+
  theme_minimal()+scale_fill_grey()+ggtitle("Number of episodes, Social plane")+
  labs(x="Social plane of interaction", y="Episodes")

bitmap("Figure2-new-hires.tiff", height = 4, width = 12, units = 'in', type="tifflzw", res=600)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

######## Figure on model performances

load('./perfsdata.Rdata')
# EXecute only if the Rdata does not have the RF-MC models
# moredatafiles <- list.files(".", "RF_MC_*")
# for(f in moredatafiles){
#   load(f)
#   d<-data.frame(label=label, auc=auc, kappa=cm$overall['Kappa'], 
#                 acc=cm$overall['Accuracy'], f1=f1, lang="R", model="RF_MC", 
#                 sources="all", validation="LOSO", 
#                 target=ifelse(grepl(pattern = "Activity", x = label, fixed = T), "Activity", "Social"), 
#                 modeltype=ifelse(grepl(pattern = "GM", x = label, fixed = T), "GM", "PM"),
#                 modelsources="RF_MC_all")
#   df<-rbind(df,d)
# }
# save(df, file = './perfsdata.Rdata')

df$class <- as.factor(ifelse(df$model=="RF" | df$model=="SVMBestNoCorr","Time-independent","Time-aware"))
# Personalized models
evalPM <- df[df$validation=="LOSO" & df$modeltype=="PM" & df$sources=="all",]
evalPM$class <- factor(evalPM$class, levels(evalPM$class)[2:1])
evalPM$model <- factor(evalPM$model)
evalPM$model <- factor(evalPM$model, levels(evalPM$model)[c(6,4,3,1,5,2)])
levels(evalPM$model) <- c("Markov Chain-enhanced\nRandom Forest", 
                          "Random Forest w/Look-back\nPCA per data source", 
                          "Random Forest w/Look-back\nGlobal PCA",
                          "3-layer LSTM\nGlobal PCA",
                          "SVM, 100 Best features",
                          "Random Forest, All features")

meansPM <- aggregate(f1~model+target, evalPM, mean)
mediansPM <- aggregate(f1~model+target, evalPM, median)

plot3 <- ggplot(evalPM, aes(x=model, y=f1, fill=class))+geom_boxplot()+
  theme_minimal()+scale_fill_grey(start = 0.5, end=0.8)+coord_flip()+
  geom_text(data=mediansPM, aes(x=model, label=sprintf("%0.3f", round(f1, digits = 3)), y=1), inherit.aes = F)+
  facet_wrap(~target)+ylim(0,1)+#geom_point()+
  labs(y="F1 Score", x="Model")+
  ggtitle("Personalized model performance, Leave-one-session-out evaluation")+
  theme(panel.spacing = unit(4, "lines"))

bitmap("Figure3-new-hires.tiff", height = 4, width = 12, units = 'in', type="tifflzw", res=600)
plot3
dev.off()


# Generalized models
evalGM <- df[df$validation=="LOSO" & df$modeltype=="GM" & df$sources=="all",]
evalGM <- evalGM[evalGM$model!="LSTM-100PCA-3L" & evalGM$model!="LSTM-100PCA-LP" & evalGM$model!="LSTM-2L-32hidden-50PCA",]
evalGM$class <- factor(evalGM$class, levels(evalGM$class)[2:1])
evalGM$model <- factor(evalGM$model)
evalGM$model <- factor(evalGM$model, levels(evalGM$model)[c(6,4,3,1,5,2)])
levels(evalGM$model) <- c("Markov Chain-enhanced\nRandom Forest", 
                          "Random Forest w/Look-back\nPCA per data source", 
                          "Random Forest w/Look-back\nGlobal PCA",
                          "3-layer LSTM\nGlobal PCA",
                          "SVM, 100 Best features",
                          "Random Forest, All features")


meansGM <- aggregate(f1~model+target, evalGM, mean)
mediansGM <- aggregate(f1~model+target, evalGM, median)
plot4 <- ggplot(evalGM, aes(x=model, y=f1, fill=class))+geom_boxplot()+
  theme_minimal()+scale_fill_grey(start = 0.5, end=0.8)+coord_flip()+
  geom_text(data=mediansGM, aes(x=model, label=sprintf("%0.3f", round(f1, digits = 3)), y=1), inherit.aes = F)+
  facet_wrap(~target)+ylim(0,1)+#geom_point()+
  labs(y="F1 Score", x="Model")+
  ggtitle("Generalized model performance, Leave-one-session-out evaluation")+
  theme(panel.spacing = unit(4, "lines"))

bitmap("Figure4-new-hires.tiff", height = 4, width = 12, units = 'in', type="tifflzw", res=600)
plot4
dev.off()
