source('C:/Users/zakaria/Desktop/Intership/Program/Program/LoadPackage.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/Signal.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/Bloc.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/DataClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/RRClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/AutClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/Utils.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/DataBase.R', echo=TRUE)





aut_file<-LoadAut("C:\\Data\\pravee1_aut.csv")
data_file<-LoadData("C:\\Data\\pravee1_data.csv")
rr_file <-LoadRR("C:\\Data\\pravee1_rr.csv")



paraSympMean<-autMean(aut_file@ParaSymp@val)
sympMean<-autMean(aut_file@Symp@val)
LFHF.ratio<-Cutaut(sympMean,paraSympMean)
LFHF.ratio<-LabelHRV(LFHF.ratio)

synchro.var<-synchro(nrow(LFHF.ratio),nrow(TemperatureMean1(data_file@Temp@val)))

Activity<-activityMean(data_file,synchro.var=synchro.var/10)

ActivityLab<-LabelActivity(Activity)


time<-seq(10, length=length(ActivityLab), by=10)
Time=data.frame(time)






for (i in 1:length(ActivityLab)) {
  addExperimentation(1,1,Time[i,],ActivityLab[i,],LFHF.ratio[i,])
}







library(TraMineR)
data.exp <-LoadExperimentation(1,1)
data.seq <-CreateSequence(data.exp)
plotFullSequence(data.seq)
plotSequence(data.seq)
seqlegend(data.seq)
seqiplot(data.seq,cex.plot=0.8,withlegend = TRUE,cex.legend=0.5,xtstep=10,title="Full sequences ")
seqtab(data.seq,weighted=FALSE, tlim = 1:10)





