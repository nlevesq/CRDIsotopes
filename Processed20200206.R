#Reads in the file
run8<-read.csv("Export20200206.csv", header = T)

#Removes all the blanks
run8a<-subset(run8, Identifier.1!='blank')

#Should gather the Nitrogen data and ignore the reference peaks
run8N<-subset(run8a, Peak.Nr==4)

#Should gather the Carbon data and ignore the reference peaks
run8C<-subset(run8a, Peak.Nr==5)

#Shoves the Carbon and Nitrogen data together
run8b<-merge(run8C, run8N, by='Row', no.dups = T)

#Keeps only the necessary rows 
columns<-c('Row', 'Identifier.1.x',	"Identifier.2.x",	
            "Amount.x",	"d.13C.12C.x",	'd.15N.14N.y')
processed<-subset(run8b, select = columns)

#Exports a file into your working directory that must be verified and further cleaned up 
###Change this to a new file name###
write.csv(processed, "Processed20200206.csv")
