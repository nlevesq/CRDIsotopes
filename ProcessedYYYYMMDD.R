#Reads in the file
run<-read.csv("ExportYYYYMMDD.csv", header = T)

#Removes all the blanks
runa<-subset(run, Identifier.1!='blank')

#Should gather the Nitrogen data and ignore the reference peaks
runN<-subset(runa, Peak.Nr==4)

#Should gather the Carbon data and ignore the reference peaks
runC<-subset(runa, Peak.Nr==5)

#Shoves the Carbon and Nitrogen data together
runb<-merge(runC, runN, by='Row', no.dups = T)

#Keeps only the necessary rows 
columns<-c('Row', 'Identifier.1.x',	"Identifier.2.x",	
            "Amount.x",	"d.13C.12C.x",	'd.15N.14N.y')
processed<-subset(runb, select = columns)

#Exports a file into your working directory that must be verified and further cleaned up 
###Change this to a new file name###
write.csv(processed, "ProcessedYYYYMMDD.csv")
