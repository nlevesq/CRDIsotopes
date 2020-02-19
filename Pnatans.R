si<-read.csv("SI_Data - All Data.csv", header= TRUE)
natans<-subset(si, species== "natans")
natans$id
plot(natans$pond)
plot(natans$sampletype)
