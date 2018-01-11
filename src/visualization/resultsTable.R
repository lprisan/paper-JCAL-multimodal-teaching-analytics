load('./perfsdata.Rdata')

ActMean <- aggregate(f1 ~ modelsources+validation+target, data = df[df$target=='Activity' & df$modeltype=="PM",], mean)
names(ActMean)[4]<-"f1.mean"
ActSD <- aggregate(f1 ~ modelsources+validation+target, data = df[df$target=='Activity' & df$modeltype=="PM",], sd)
names(ActSD)[4]<-"f1.sd"
SocMean <- aggregate(f1 ~ modelsources+validation+target, data = df[df$target=='Social' & df$modeltype=="PM",], mean)
names(SocMean)[4]<-"f1.mean"
SocSD <- aggregate(f1 ~ modelsources+validation+target, data = df[df$target=='Social' & df$modeltype=="PM",], sd)
names(SocSD)[4]<-"f1.sd"

Act <- merge(ActMean,ActSD)
Soc <- merge(SocMean,SocSD)
