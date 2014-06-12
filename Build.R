source('C:/Users/zakaria/Desktop/Intership/Program/Program/LoadPackage.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/Signal.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/DataClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/RRClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/AutClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/Utils.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/Bloc.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/LoadExperimentation.R', echo=TRUE)



aut_file<-LoadAut("C:\\Data\\test_aut.csv")
data_file<-LoadData("C:\\Data\\test_data.csv")
rr_file <-LoadRR("C:\\Data\\test_rr.csv")

paraSympMean<-autMean(aut_file@ParaSymp@val)
sympMean<-autMean(aut_file@Symp@val)
tempMean<-TemperatureMean1(data_file@Temp@val)
synchro.var<-synchro(nrow(sympMean),nrow(tempMean))
tempMean<-TemperatureMean(data_file@Temp@val,synchro.var)
tempMean<-LabelTemp(tempMean)
Activity<-activityMean(data_file,synchro.var)
ActivityLab<-LabelActivity(Activity)



time<-seq(10, length=length(ActivityLab), by=10)
Time=data.frame(time)






for (i in 1:length(ActivityLab)) {
  addExperimentation(1,6,Time[i,],ActivityLab[i,],tempMean[i,],paraSympMean[i,],sympMean[i,])
}







library(TraMineR)
data.exp <-LoadExperimentation(1,6)
data.seq <-CreateSequence(data.exp)
plotFullSequence(data.seq)
plotSequence(data.seq)








