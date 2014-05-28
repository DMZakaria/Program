source('C:/Users/zakaria/Desktop/Intership/Program/DBControl/LoadPackage.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/DBControl/Signal.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/DBControl/DataClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/DBControl/RRClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/DBControl/AutClass.R', echo=TRUE)
source('C:/Users/zakaria/Desktop/Intership/Program/DBControl/Utils.R', echo=TRUE)







aut_file<-LoadAut("C:\\Data\\zaka_aut.csv")
data_file<-LoadData("C:\\Data\\zaka_data.csv",NumExp=3,sujet=1)





paraSympMean<-autMean(aut_file@ParaSymp@val)
sympMean<-autMean(aut_file@Symp@val)
ActivityFinal<-LabelActivity(activityMean(data_file))
tempMean<-TemperatureMean(data_file@Temp@val)




for (i in 1:length(data)) {
  addExperimentation(1,3,ActivityFinal[i,],tempMean[i,],paraSympMean[i,],sympMean[i,])
}


