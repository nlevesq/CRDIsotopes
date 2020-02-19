#Reads in the file
run6<-read.csv("ExportYYYYMMDD.csv", header = T)

#Removes all the blanks
run6a<-subset(run6, Identifier.1!='blank')

#Should gather the Nitrogen data and ignore the reference peaks
run6N<-subset(run6a, Peak.Nr==4)

#Should gather the Carbon data and ignore the reference peaks
run6C<-subset(run6a, Peak.Nr==5)

#Shoves the Carbon and Nitrogen data together
run6b<-merge(run6C, run6N, by='Row', no.dups = F)

#Exports a file into your working directory that must be varified and further cleaned up 
write.csv(run6b, "ProcessedYYYYMMDD.csv")
