source('C:/Users/zakaria/Desktop/Intership/Program/Program/LoadPackage.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/Signal.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/DataClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/RRClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/AutClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/Program/Utils.R', echo=TRUE)







aut_file<-LoadAut("C:\\Data\\zaka_aut.csv")
data_file<-LoadData("C:\\Data\\zaka_data.csv",NumExp=3,sujet=1)



# z<-activityMean(data_file)
Activity<-LabelActivity(activityMean(data_file))
tempMean<-TemperatureMean(data_file@Temp@val)
paraSympMean<-autMean(aut_file@ParaSymp@val)
sympMean<-autMean(aut_file@Symp@val)


#plot()
length(Activity)
nrow(tempMean)

for (i in 1:length(Activity)) {
  addExperimentation(1,3,Activity[i,],tempMean[i,],paraSympMean[i,],sympMean[i,])
}


