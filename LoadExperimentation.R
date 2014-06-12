################################################################
# @Description   load Experimenaton From the data base         # 
#                                                              #
# @param         sujet                                         #
# @param         expNum                                        #
# @param         activity                                      #
# @param         temperature                                   #
# @param         parasymp                                      #
# @param         sympa                                         #
################################################################

LoadExperimentation<- function (sujet,numNight)
{
  
  #open a connexion
  channel <- odbcConnect(dsn="RSQL",uid="root",pwd="toor")
  
  requetesql <- paste("select Time,Body_activity,Temperature from record_data where Sujet = '",sujet,"' and Num_Night='",numNight,"' ;" )
  
  #send the query ,store the result into data frame
  data.exp<-sqlQuery(channel,requetesql)
  
  
  #colse the connexion
  close(channel)
  
  return (data.exp)
}



CreateSequence <- function (data.exp)
{

  #Make time as column name
  data.transverse <- data.frame(t(data.exp[,c('Body_activity', 'Temperature')]))
  colnames(data.transverse)<-data.exp$Time  
  
  ## Defining short and long labels
  data.states <- c( "high","Tlow", "Tmedium", "Tnormal", "medium","weak","null")
  data.shortlab <- c("BAH","TML","TMM","TMN","BAM","BAW","BAN")
  # reponse.alpha<-c("high","medium","Tlow", "Tmedium","Tnormal")
  
  ## Create the state sequence object,
  data.seq <- seqdef(data.transverse[1:2,],states=data.shortlab, labels=data.states)
  
  ## print the sequence
  sequence<-print(data.seq[1:2, ], format = "SPS")
 
  return (data.seq)
  
}

plotFullSequence <-function (data.seq)
{
  seqIplot(data.seq,cex.plot=0.8,withlegend = TRUE,cex.legend=0.5,xtstep=10)
}

plotSequence <- function (data.seq)
{
  mycols <-c("1800","3600",'5400',"7200","9000","10800","12600","14400","16200","18000","19800","21600","23400","24800")
  match(mycols, colnames(data.seq))
  # which(names(data.seq) %in% mycols)
  # which(names(data.seq)=="10800") 
  
  
  
  
  par(mfrow = c(2, 2),mfcol = c(2, 2))
  seqiplot(data.seq[,1:180], withlegend = FALSE)
  seqiplot(data.seq[,181:360], withlegend = FALSE)
  seqiplot(data.seq[,361:540], withlegend = FALSE)
  seqiplot(data.seq[,541:720], withlegend = FALSE)
  seqiplot(data.seq[,721:900], withlegend = FALSE)
  seqiplot(data.seq[,901:1080], withlegend = FALSE)
  seqiplot(data.seq[,1081:1260], withlegend = FALSE)
  seqiplot(data.seq[,1261:1440], withlegend = FALSE)
  seqiplot(data.seq[,1441:1620], withlegend = FALSE)
  seqiplot(data.seq[,1621:1800], withlegend = FALSE)
  seqiplot(data.seq[,1801:1980], withlegend = FALSE)
  seqiplot(data.seq[,1981:2160], withlegend = FALSE)
  seqiplot(data.seq[,2161:2340], withlegend = FALSE)
  seqiplot(data.seq[,2341:2480], withlegend = FALSE)
  seqlegend(data.seq, fontsize = 0.65)
  
}





# ## Creating cohort variable
# reponse$time <- cut(reponse$Time, c(10,1820,3630,5440,7250,9060,10960,12580,14550,16360,18260,20060,21860,23760),
#                      labels=c("10-1810", "1820-3620", "3630-5430", "5440-7240","7250-9050","9060-10950","10960-12570","12580-14550",
#                               "14550-16350", "16360-18250", "18260-20050","20060-21850","21860-23750"), right=FALSE)























