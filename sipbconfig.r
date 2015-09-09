mainfolder <- "D:/SIPro"
dbfsfolder<-"D:/SIPro/SIP20030103/Dbfs"
datadictfolder<-"D:/SIPro/SIP20030103/Datadict"
staticfolder<-"D:/SIPro/SIP20030103/Static"
sipbInstallDates<-as.character(read.csv("SIPBInstallDates.csv",header = T)[,1])

dbflocations <- function(strDate){
    out <- list()
    out$dbfs <- paste(mainfolder,"/SIP",strDate,"/Dbfs",sep="")
    out$datadict <- paste(mainfolder,"/SIP",strDate,"/Datadict",sep="")
    out$static <- paste(mainfolder,"/SIP",strDate,"/Static",sep="")
    return(out)
}