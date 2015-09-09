library(foreign)
dbfsfolder<-"D:/SIPro/SIP20030103/Dbfs"
datadictfolder<-"D:/SIPro/SIP20030103/Datadict"
staticfolder<-"D:/SIPro/SIP20030103/Static"


# Load P/B

loadfile<-function(fn,folder,prnt=T){
    data<-read.dbf(file=paste(folder,"/",fn,sep=""),as.is = T)
    if(prnt){
        print(str(data))
    }
    return(data)
}

tonumeric<-function(data,varnames){
    for (v in varnames){
        data[,v] <- as.numeric(data[,v])
    }
    return(data)
}

growthrate<-function(val.from, val.to){
    pos.idx <- val.to >= val.from & val.from!=0 & !is.na(val.from) & !is.na(val.to)
    neg.idx <- val.to < val.from & val.from!=0 & !is.na(val.from) & !is.na(val.to)
    na.idx <- is.na(val.from) | is.na(val.to) | val.from==0
    out <- val.from*0
    out[pos.idx] <- abs(100*(val.to[pos.idx]-val.from[pos.idx])/val.from[pos.idx])
    out[neg.idx] <- -abs(100*(val.to[neg.idx]-val.from[neg.idx])/val.from[neg.idx])
    out[na.idx]<-NA
    return(out)
}
    
si_ci <- loadfile("si_ci.dbf", staticfolder,F)[,c("COMPANY_ID","COMPANY","TICKER")]    
si_mlt <- loadfile("si_mlt.dbf", dbfsfolder,F)[,c("COMPANY_ID", "PBVPS")]
si_isq <- loadfile("si_isq.dbf", staticfolder,F)[,c("COMPANY_ID", "EPSCON_Q1","EPSCON_Q2",
                                                    "EPSCON_Q5","EPSCON_Q6")]
si_gr <- loadfile("si_gr.dbf", dbfsfolder,F)[,c("COMPANY_ID", "EPSC_G5F","EPSC_G1Q5","EPSC_G2Q6")]
si_perc <- loadfile("si_perc.dbf", dbfsfolder,F)[,c("COMPANY_ID", "RRSW_4Q")]
si_rat <- loadfile("si_rat.dbf", dbfsfolder,F)[,c("COMPANY_ID", "PTM_12M")]
si_psd <- loadfile("si_psd.dbf", dbfsfolder,F)[,c("COMPANY_ID", "PRP_2YH","SHR_AQ1","MKTCAP","PRCHG_SD3Y","PRICE","PRICE_DATE")]

data <- merge(si_ci,si_mlt,by="COMPANY_ID")
data <- merge(data,si_isq,by="COMPANY_ID")
data <- merge(data,si_gr,by="COMPANY_ID")
data <- merge(data,si_perc,by="COMPANY_ID")
data <- merge(data,si_rat,by="COMPANY_ID")
data <- merge(data,si_psd,by="COMPANY_ID")
varnames <- c("PBVPS","EPSCON_Q1","EPSCON_Q2","EPSCON_Q5","EPSCON_Q6", "EPSC_G5F", "RRSW_4Q",
              "PTM_12M", "PRP_2YH", "SHR_AQ1","EPSC_G1Q5","EPSC_G2Q6","MKTCAP","PRCHG_SD3Y","PRICE")

data <- tonumeric(data,varnames)


test <- subset(data,PBVPS<=1.5 & EPSCON_Q1>EPSCON_Q5 & EPSCON_Q2>EPSCON_Q6 & EPSC_G1Q5 > EPSC_G2Q6 & 
               EPSC_G5F>0 & RRSW_4Q>=70 & PTM_12M>0 & PRP_2YH>=85 & SHR_AQ1<=20)


