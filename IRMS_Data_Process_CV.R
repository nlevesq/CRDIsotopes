#IRMS.r
#updated by CV 3/8/16

#script to read in raw data from the CEST IRMS (Delta V), pullout the important data, and save an Excel file containing the raw data, processed data, and data prepared for the database.
#script also saves reference data into a csv file in order to keep track of reference data overtime. This can be turned off in settings.
#Data format: data should be as exported from the Isodat Software on the Delta Plus IRMS at CEST.  You should call the reference samples ref1,ref2... for Identifier 1, and acetan for Identifier 2.  Blanks should be called blank1..., enter sampleIDs into Identifier1

library(xlsx) #allows you to work with.xls files
library(plyr) #data manipulation functions filter, select, summarize, etc
library(dplyr) #data manipulation functions filter, select, summarize, etc
library(ggplot2) #basic plots

rm(list=ls())  #clear variables
graphics.off()  #close figs

setwd("C:/Users/Carmella/Desktop/SI Data/Excel output")

#settings#####
compare.refs<-1 
savefile<-1 #save Excel file 

###############################
###load in data################
filename<-file.choose() #choose file
data.raw<-read.xlsx(filename,1)
###############################

#Check the Blanks
data.blanks<-filter(data.raw, Identifier.2=="blank") 

#check if there are any N or CO2 blank peaks - in blanks 
#if there are more than 4 peaks in blank it will tell you where
if (max(data.blanks$Peak.Nr)>4){
  message("there are blank peaks in these samples: ")
  print(data.blanks[data.blanks$Peak.Nr>4,1:4])
  readline("press enter to continue")
} 


######
#QA/QC Sample Data
######

#Sample Data that excludes blanks
data.refs.samples1<-filter(data.raw, !Identifier.2=="blank") #Removes blank
#Pull out N and C peaks and match with sample
data.refs.samples<-select(data.refs.samples1, -c(1,5,9:15,17:19,21:25,27:33,36)) #select is from dplyr
data.N<-filter(data.refs.samples, Peak.Nr==3) %>% select(-c(8,10)) # %>% called piping...allows you to use multiple plyr functions linked together
data.C<-filter(data.refs.samples, Peak.Nr==4) %>% select(c(7,8,10))
data.NC<-cbind(data.N,data.C)# combine N and C data

######################################MUST ENTER DATE#####################
#date<-("03/07/16") # enter date
########################################################################
data.NC<-cbind(date,data.NC)
names(data.NC)[11]<-"C_Peak_Area" #Changes column names
names(data.NC)[8]<-"N_Peak_Area" #Changes column names

# select Reference data
reference<-filter(data.NC, Type=="Start Reference Mean"|Type=="Add Reference Mean")
# calculate calibration factor for N, average
cf_N<-(10.36*reference$Amount/reference$N_Peak_Area)
cf_N<-mean(cf_N)
# calculate percent N for all samples
Per_N<-(cf_N*data.NC$N_Peak_Area/data.NC$Amount)
# calculate calibration factor for C, average
cf_C<-(71.09*reference$Amount/reference$C_Peak_Area)
cf_C<-mean(cf_C)
# calculate percent C for all samples
Per_C<-(cf_C*data.NC$C_Peak_Area/data.NC$Amount)

# add Percent C and N columns to dataframe
data.NC$Per_N<-(Per_N)
data.NC$Per_C<-(Per_C)
# calculate C:N ratio
data.NC$CN<-(data.NC$Per_C/data.NC$Per_N)
data.NC<-select(data.NC, -c(7)) #drops the Peak column
colnames<-c("Date","ID1","ID2","Well","Type","Sample_Amount","N_Peak_Area", "Amp_28","d15N_14N","C_Peak_Area","Ampl_44","d13C_12C", "Per_N", "Per_C","CN")
names(data.NC)<-colnames
View(data.NC) #look at the dataset

# select soil data
soil<-filter(data.NC, ID2=="soil")
# select sorghum data
sorghum<-filter(data.NC, ID2=="sorghum")
# select protein data
protein<-filter(data.NC, ID2=="protein")
#select acetanilide data
acetan<-filter(data.NC, ID2=="acetan")

# average delta 13C sorghum data
avgC_sorghum<-mean(sorghum$d13C_12C)
# average delta 13C protein data
avgC_protein<-mean(protein$d13C_12C)
#select delta 13C acetanilide data
avgC_acetan<-mean(acetan$d13C_12C)

# average delta 15N sorghum data
avgN_sorghum<-mean(sorghum$d15N_14N)
# average delta 15N protein data
avgN_protein<-mean(protein$d15N_14N)
#select delta 15N acetanilide data
avgN_acetan<-mean(acetan$d15N_14N)

# set up vectors for observed and expected delta 13 C values
obs_13C<-c(soil$d13C_12C,avgC_sorghum,avgC_protein,avgC_acetan)
exp_13C<-c(-26.66,-13.68,-26.98,-33.14)

# run a linear regression on expected vs. observed values
modelC<-glm(exp_13C~obs_13C)
ccoeff<-coefficients(modelC)
# correct delta 13C values based on calibration equation
data.NC$d13C_12C<-(ccoeff[[1]]+ccoeff[[2]]*data.NC$d13C_12C)

# set up vectors for observed and expected delta 15N values
obs_15N<-c(soil$d15N_14N,avgN_sorghum,avgN_protein,avgN_acetan)
exp_15N<-c(7.30,1.58,5.94,-0.73)

# run a linear regression on expected vs. observed values
modelN<-glm(exp_15N~obs_15N)
ncoeff<-coefficients(modelN)
# correct delta 15N values based on calibration equation
data.NC$d15N_14N<-(ncoeff[[1]]+ncoeff[[2]]*data.NC$d15N_14N)

#Reference data
reference<-filter(data.NC, Type=="Start Reference Mean"|Type=="Add Reference Mean")
#Reference data ran as sample unknowns
ref_unknown<-filter(data.NC, Type=="Sample", ID2=="acetan")
#Unknown samples
samples<-filter(data.NC, Type=="Sample", !ID2=="acetan", !ID2=="soil", !ID2=="sorghum",!ID2=="protein")
#References all
ref_all<-filter(data.NC, ID2=="acetan")
# Delta stds
delta_stds<-filter(data.NC, Type=="Sample", !ID2=="acetan", ID2=="soil"|ID2=="sorghum"|ID2=="protein")

#Summary and Plot of Reference Data
Ref_summary<-summarize(ref_all, Mean13C=mean(d13C_12C, na.rm=TRUE), Sd13C=sd(d13C_12C, na.rm=TRUE), Mean15N=mean(d15N_14N, na.rm=TRUE), Sd15N=sd(d15N_14N, na.rm=TRUE))
ggplot(ref_all, aes(d13C_12C, d15N_14N))+ geom_point()

#Summary and Plot of Reference unknown data
Ref_Unknown_summary<-summarize(ref_unknown, Mean13C=mean(d13C_12C, na.rm=TRUE), Sd13C=sd(d13C_12C, na.rm=TRUE), Mean15N=mean(d15N_14N, na.rm=TRUE), Sd15N=sd(d15N_14N, na.rm=TRUE))
ggplot(ref_unknown, aes(d13C_12C, d15N_14N))+ geom_point()

#Summary and Plot of Sample data
samp_summary<-summarize(samples, Mean13C=mean(d13C_12C, na.rm=TRUE), Sd13C=sd(d13C_12C, na.rm=TRUE), Mean15N=mean(d15N_14N, na.rm=TRUE), Sd15N=sd(d15N_14N, na.rm=TRUE))
ggplot(samples, aes(d13C_12C, d15N_14N))+ geom_point()

#Reference Data through time
ggplot(ref_unknown, aes(Date, d15N_14N))+ geom_point()

View(data.NC)



#save data - save raw, processed, and database prep data into a new Excel document
setwd("C:/Users/Carmella/Desktop/SI Data/Processed Data")
if (savefile==1){
  #savefilename<-'2013_Inverts_3-7-16.xls'
  write.xlsx(data.raw,savefilename,sheet='raw data',row.names=F)
  write.xlsx(ref_all,savefilename,sheet='refs',row.names=F,append=T)
  write.xlsx(delta_stds,savefilename,sheet='delta stds',row.names=F,append=T)
  write.xlsx(samples,savefilename,sheet='samples',row.names=F,append=T)
  }


#######################COPY WHOLE THING OR DATES GET MESSED UP#######
#add refs to past refs, then compare - calc means,sd,cv, plot over time, etc.
if (compare.refs==1)
{
  #load file
  refs<-read.table('DeltaV_IRMS_Refs.csv',header=T,sep=',')
  names(refs)<-colnames 
  
  #add new refs to file
    refs<-rbind(refs,ref_all) #add refs to file
    write.table(refs,'DeltaV_IRMS_Refs.csv',col.names=T,row.names=F,sep=',') #save file
 

  #calc means etc
  #acetan refs
  refs.a<-refs
  refs.a.mean<-sapply(refs.a[,c(13,9,14,12)],mean)
  refs.a.sd<-sapply(refs.a[,c(13,9,14,12)],sd)
  refs.a.cv<-refs.a.sd/refs.a.mean*100
  acetan<-ref_all
  acetan.mean<-sapply(acetan[,c(13,9,14,12)],mean)
  acetan.sd<-sapply(acetan[,c(13,9,14,12)],sd)
  acetan.cv<-acetan.sd/acetan.mean*100

  print(rbind(refs.a.mean,refs.a.sd,refs.a.cv)) #All refs
  print(rbind(acetan.mean,acetan.sd,acetan.cv)) #this run

  #plot refs - compare current to past refs
  windows()
  par(oma=c(1,1,3,1))
  par(mar=c(5,4,4,2) + 0.1)  
  par(mfrow=c(2,2))
  boxplot(refs.a[,13],acetan[,13],main='%N',names=c('all','current'))
  boxplot(refs.a[,9],acetan[,9],main='d15N',names=c('all','current'))
  boxplot(refs.a[,14],acetan[,14],main='%C',names=c('all','current'))
  boxplot(refs.a[,12],acetan[,12],main='d13C',names=c('all','current'))
  mtext('acetan', side=3, line=1, cex=1.5,outer=TRUE)

 
  #mean acetan refs by date
  ref.date.mean<-aggregate(refs.a[,c(13,9,14,12)],by=list(refs.a$Date),mean)
  windows()
  par(mfrow=c(2,1))
  plot(refs.a[,1],refs.a[,9],main='Acetan d15N',xlab='',ylab='del')
  lines(ref.date.mean[,1],ref.date.mean[,3],type='o',pch=16,col='red')
  plot(refs.a[,1],refs.a[,12],main='Acetan d13C',xlab='',ylab='del')
  lines(ref.date.mean[,1],ref.date.mean[,5],type='o',pch=16,col='red')

}  